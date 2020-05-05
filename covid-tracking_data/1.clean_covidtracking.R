#!/usr/bin/R

#-------------------------------------------------------
# Lin Lab - Covid19 Project
# Covidtracking raw data cleaning
#-------------------------------------------------------
rm(list=ls())

library(grid)
library(gridExtra)
library(ggpubr)
library(data.table)
library(pracma)
require(filesstrings)
# library(Hmisc)
# library(psych)
# library(stats)

# ==================
# Directories setup
# ==================
wd <- "C:/Users/LiHui/OneDrive - Harvard University/Grad_School/COVID-19/covid_tracking/"
currentTime <- Sys.time()
globalTime <- format(currentTime,c('%Y_%m_%d'))
setwd(paste0(wd, "covidtracking_pull_", globalTime))

# Move raw data to folder
dir.create(file.path(getwd(), "raw"), showWarnings = TRUE)
raw.path <- paste0(getwd(), "/raw/")
raw_data <- list.files(pattern = ".csv", recursive = TRUE)
for (file in raw_data){
  file.move(file, raw.path, overwrite = TRUE)
}

dir.create(file.path(getwd(), "cleaned"), showWarnings = TRUE)
cleaned.path <- paste0(getwd(), "/cleaned/")
dir.create(file.path(getwd(), "figures"), showWarnings = TRUE)
figures.path <- paste0(getwd(), "/figures/")

# load state historical data
state_hist <- read.table(paste0(raw.path, "states_daily.csv"), sep=",", header=TRUE)

# merge in state grade/score for reporting reliability info
state_curr <- read.table(paste0(raw.path, "states_current.csv"), sep=",", header=TRUE)
table(state_curr$grade)
state_data <- merge(state_hist, state_curr[c('state','grade','score')], by="state", all.x = TRUE)

# merge in state full names
state_info <- read.delim(paste0(raw.path, "states_info.csv"), sep=",", header=TRUE)
state_info <- state_info[, c("state", "name")]
state_cleaned <- merge(state_data, state_info, by="state", all.x = TRUE)
setnames(state_cleaned, "name", "stateName")

# descriptive
dim(state_cleaned)
summary(state_cleaned)
# describe(state_cleaned)
# describeBy(state_cleaned, state_cleaned$state)

# =======================
# Consistency checks
# =======================
dt_state_cleaned <- data.table(state_cleaned)
setorder(dt_state_cleaned, state, date)

# 1. Check for negative values
sub_col <- c("positiveIncrease", "negativeIncrease", "totalTestResultsIncrease",
             "deathIncrease","hospitalizedIncrease")
negative_count <- dt_state_cleaned[, lapply(.SD, function(x) sum(x<0, na.rm=TRUE)), .SDcols = (sub_col)]
positive_count <- dt_state_cleaned[, lapply(.SD, function(x) sum(x>0, na.rm=TRUE)), .SDcols = (sub_col)]
zero_count <- dt_state_cleaned[, lapply(.SD, function(x) sum(x=0, na.rm=TRUE)), .SDcols = (sub_col)]

dt_state_cleaned[positiveIncrease<0, c("date","stateName","positive", "positiveIncrease")]
dt_state_cleaned[negativeIncrease<0, c("date","stateName","negative", "negativeIncrease")]
dt_state_cleaned[deathIncrease<0, c("date","stateName","death", "deathIncrease")]
dt_state_cleaned[hospitalizedIncrease<0, c("date","stateName","hospitalized", "hospitalizedIncrease")]

# 2. Check consistency of "Increase" variable
dt_state_cleaned[, positiveChange := c(NA, diff(positive)), by = state]
dt_state_cleaned[positiveChange!=positiveIncrease, list(date, state, stateName, positive, positiveChange, positiveIncrease)]

dt_state_cleaned[, negativeChange := c(NA, diff(negative)), by = state]
dt_state_cleaned[negativeChange!=negativeIncrease, list(date, state, stateName, negative, negativeChange, negativeIncrease)]

dt_state_cleaned[, deathChange := c(NA, diff(death)), by = state]
dt_state_cleaned[deathChange!=deathIncrease, list(date, state, stateName, death, deathChange, deathIncrease)]

dt_state_cleaned[, hospitalizedChange := c(NA, diff(hospitalized)), by = state]
dt_state_cleaned[hospitalizedChange!=hospitalizedIncrease, list(date, state, stateName, hospitalized, hospitalizedChange, hospitalizedIncrease)]

dt_state_cleaned[, totalTestResultsChange := c(NA, diff(totalTestResults)), by = state]
dt_state_cleaned[totalTestResultsChange!=totalTestResultsIncrease, list(date, state, stateName, totalTestResults, totalTestResultsChange, totalTestResultsIncrease)]

# 3. Check US historical vs State historical
us_hist <- read.table(paste0(raw.path, "us_daily.csv"), sep=",", header=TRUE)

us_hist_keepcol <- c("date", "states", "positive", "negative", "totalTestResults",
                     "death","hospitalized",
                     "positiveIncrease", "negativeIncrease",
                     "totalTestResultsIncrease","deathIncrease","hospitalizedIncrease")
us_cleaned <- us_hist[us_hist_keepcol]

# (treating NA as zero)
colstocheck <- c("positive", "negative", "totalTestResults",
                 "death","hospitalized",
                 "positiveIncrease", "negativeIncrease",
                 "totalTestResultsIncrease","deathIncrease",
                 "hospitalizedIncrease")
state_agg_day <- aggregate(x = state_cleaned[colstocheck],
                          FUN = sum, na.rm = TRUE, by = list(date = state_cleaned$date))
hist_check <- merge(state_agg_day, us_cleaned, by = "date", suffixes = c(".state_agg", ".national"))
us_state_agg_check <- vector("list", 10)

for (i in 1:10){
  dat <- hist_check[c("date", paste0(colstocheck[i],".state_agg"), paste0(colstocheck[i], ".national"))]
  dat[paste0(colstocheck[i],".state_agg")] <- log10(dat[paste0(colstocheck[i],".state_agg")])
  dat[paste0(colstocheck[i],".national")] <- log10(dat[paste0(colstocheck[i],".national")])
  
  us_state_agg_check[[i]] <- ggplot(dat, aes_string(y=paste0(colstocheck[i],".state_agg"), x=paste0(colstocheck[i], ".national"))) + 
    geom_point() + geom_abline(intercept=0, slope=1, color='red') +
    labs(x="U.S. (log10)", y="State aggregated (log10)") +
    ggtitle(colstocheck[i]) + 
    # scale_x_continuous(breaks=NULL) +
    theme(plot.title=element_text(hjust=0.5, size=15),
          axis.title.x=element_text(margin=margin(t=10, r=0, b=0, l=0), hjust=0.5, size=10),
          axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0), vjust=0.5, size=10),
          axis.text.x=element_text(size=10),
          axis.text.y=element_text(size=10))
  # ggsave(paste0(colstocheck[i],"_us_state_agg.jpeg"), scatter_plot, width=10, height=10)
}
plot_g <- ggarrange(us_state_agg_check[[1]], us_state_agg_check[[2]], us_state_agg_check[[3]],
                    us_state_agg_check[[4]], us_state_agg_check[[5]], us_state_agg_check[[6]],
                    us_state_agg_check[[7]], us_state_agg_check[[8]], us_state_agg_check[[9]],
                    us_state_agg_check[[10]],
                    ncol=4, nrow=3, common.legend = TRUE, legend="right")
plot_gg <- arrangeGrob(plot_g, bottom=textGrob(paste0("Data updated: ", globalTime), x=0.5, vjust=0.05, gp=gpar(fontsize=15)))
ggsave(paste0(figures.path, "covidtracking_us_state_agg.jpeg"), plot_gg, width=15, height=10)

# close-up of discrepancies
dt_hist_check <- data.table(hist_check)
pos_check <- subset(dt_hist_check, positiveIncrease.national != positiveIncrease.state_agg)
pos_check[, list(date, positiveIncrease.state_agg, positiveIncrease.national)]
neg_check <- subset(dt_hist_check, negativeIncrease.national != negativeIncrease.state_agg)
neg_check[, list(date, negativeIncrease.state_agg, negativeIncrease.national)]
death_check <- subset(dt_hist_check, deathIncrease.national != deathIncrease.state_agg)
death_check[, list(date, deathIncrease.state_agg, deathIncrease.national)]
tottest_check <- subset(dt_hist_check, totalTestResultsIncrease.national != totalTestResultsIncrease.state_agg)
tottest_check[, list(date, totalTestResultsIncrease.state_agg, totalTestResultsIncrease.national)]
hosp_check <- subset(dt_hist_check, hospitalizedIncrease.national != hospitalizedIncrease.state_agg)
hosp_check[, list(date, hospitalizedIncrease.state_agg, hospitalizedIncrease.national)]

# # 4. Check totalTestRestuls vs CDC reporting tests
# cdc_tests <- read.table(paste0(raw.path, "cdc_daily.csv"), sep=",", header=TRUE)
# 
# # unify date column
# cdc_tests$date <- paste(cdc_tests$dateCollected, "2020", sep="/")
# cdc_tests$date <- strptime(cdc_tests[['date']], format='%m/%d/%y')
# us_cleaned$date <- paste(substr(us_cleaned$date, 5,6), substr(us_cleaned$date, 7,8), substr(us_cleaned$date, 1, 4), sep="/")
# us_cleaned$date <- strptime(us_cleaned[['date']], format='%m/%d/%y')
# 
# # merge CDC with U.S. historical
# cdc_check <- merge(us_cleaned, cdc_tests, by = "date", all.x = TRUE)
# cdc_check <- cdc_check[c("totalTestResultsIncrease", "dailyTotal")]
# 
# # plot scatterplot
# scatter_plot <- ggplot(cdc_check, aes(x=dailyTotal, y=totalTestResultsIncrease)) + 
#   geom_point() + geom_abline(intercept=0, slope=1, color='red') +
#   
#   labs(x="CDC number of tests", y="U.S. Total test results increase", 
#        caption = "Data updated to 4/4/2020.") +
#   ggtitle(paste0("Consistency check for number of tests")) + 
#   theme(plot.title=element_text(hjust=0.5, size=25),
#         axis.title.x=element_text(margin=margin(t=10, r=0, b=0, l=0), hjust=0.5, size=20),
#         axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0), vjust=0.5, size=20),
#         axis.text.x=element_text(size=20),
#         axis.text.y=element_text(size=20))
# ggsave(paste0(figures.path, "n_tests_covidtracking_cdc.jpeg"), scatter_plot, width=10, height=10)

# =================
# Writing to files
# =================

# unify date column
state_cleaned$date <- paste(substr(state_cleaned$date, 5,6), substr(state_cleaned$date, 7,8), substr(state_cleaned$date, 1, 4), sep="/")
state_cleaned$date <- strptime(state_cleaned[['date']], format='%m/%d/%y')

# order by state + date
state_cleaned <- state_cleaned[order(state_cleaned$state, state_cleaned$date),]

# keep useful columns
state_hist_keepcol <- c("date", "state", "stateName", "grade","score","positive","negative",
                        "totalTestResults","death","hospitalized",
                        "positiveIncrease", "negativeIncrease",
                        "totalTestResultsIncrease","deathIncrease","hospitalizedIncrease")
state_cleaned <- state_cleaned[state_hist_keepcol]

# save U.S. and State historical data 
write.table(state_cleaned, paste0(cleaned.path,"covidtracking_State.csv"), append = FALSE, sep = ",",
            row.names = FALSE, col.names = TRUE)

us_cleaned <- us_cleaned[order(us_cleaned$date),]
us_cleaned$date <- paste(substr(us_cleaned$date, 5,6), substr(us_cleaned$date, 7,8), substr(us_cleaned$date, 1, 4), sep="/")
us_cleaned$date <- strptime(us_cleaned[['date']], format='%m/%d/%y')
write.table(us_cleaned, paste0(cleaned.path,"covidtracking_US.csv"), append = FALSE, sep = ",",
            row.names = FALSE, col.names = TRUE)
