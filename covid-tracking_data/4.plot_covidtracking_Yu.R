#!/usr/bin/R

#-------------------------------------------------------
# Lin Lab - Covid19 Project
# Consistency check between CovidTracking and Yu Group
#-------------------------------------------------------
library(grid)
library(gridExtra)
library(ggpubr)
library(reshape2)
library(data.table)
library(tidyr)

# ==============================
# STATE LEVEL CONSISTENCY CHECK
# ==============================
rm(list=ls())

wd <- "C:/Users/LiHui/OneDrive - Harvard University/Grad_School/COVID-19/covid_tracking/"
currentTime <- Sys.time()
globalTime <- format(currentTime,c('%Y_%m_%d'))
setwd(paste0(wd, "covidtracking_pull_", globalTime))
cleaned.path <- paste0(getwd(), "/cleaned/")
figures.path <- paste0(getwd(), "/figures/")

# check that Yu Group data have been copied to the "cleaned" folder
stopifnot(file.exists(paste0(cleaned.path, "Yu_Group_State.csv")))

# load cleaned covid-tracking and CSSE
df_covidtr <- read.table(paste0(cleaned.path, "covidtracking_State.csv"), sep=",", header=TRUE)
df_covidtr$date <- format(as.Date(df_covidtr$date), '%m/%d/%y')
df_yugroup <- read.table(paste0(cleaned.path, "Yu_Group_State.csv"), sep=",", header=TRUE)
df_yugroup$date <- format(as.Date(df_yugroup$date), '%m/%d/%y')

# merge two datasets
df_merged <- merge(df_covidtr, df_yugroup, by.x = c("date","stateName"), by.y = c("date", "state"),
                   suffixes = c(".covtr", ".yugroup"))

# plot Covidtracking vs Yu Group
scatter_plot <- vector("list", 4)
commonvars = c("positive", "positiveIncrease", "death","deathIncrease")
vartag = c("Cases (cumulative)", "Cases (daily increase)", "Death (cumulative)", "Death (daily increase)")

for (i in 1:4){
  dat <- df_merged[c("date", paste0(commonvars[i],".yugroup"), paste0(commonvars[i], ".covtr"))]
  dat <- na.omit(dat[dat[paste0(commonvars[i],".yugroup")] >= 1 
                     & dat[paste0(commonvars[i],".covtr")] >= 1, ])
  dat[paste0(commonvars[i],".yugroup")] <- log10(dat[paste0(commonvars[i],".yugroup")])
  dat[paste0(commonvars[i],".covtr")] <- log10(dat[paste0(commonvars[i],".covtr")])
  scatter_plot[[i]] <- ggplot(dat, aes_string(y=paste0(commonvars[i],".yugroup"), 
                                              x=paste0(commonvars[i], ".covtr"))) + 
    geom_point(size = 2.5, color = "royalblue4") + geom_abline(intercept=0, slope=1, color='red') +
    geom_text(x=0.50*max(dat[paste0(commonvars[i],".covtr")], na.rm = TRUE), 
              y=0.85*max(dat[paste0(commonvars[i],".yugroup")], na.rm = TRUE), size = 5, 
              label=paste0("Correlation: ", round(cor(df_merged[[paste0(commonvars[i],".covtr")]], 
                                                      df_merged[[paste0(commonvars[i],".yugroup")]], use = "pairwise.complete.obs"), 3))) +
    labs(x="covidTracking (log10)", y="Yu Group (log10)") +
    ggtitle(vartag[i]) + 
    theme(plot.title=element_text(hjust=0.5, size=25),
          axis.title.x=element_text(margin=margin(t=10, r=0, b=0, l=0), hjust=0.5, size=20),
          axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0), vjust=0.5, size=20),
          axis.text.x=element_text(size=20),
          axis.text.y=element_text(size=20))
}

plot_g <- ggarrange(scatter_plot[[1]], scatter_plot[[2]], scatter_plot[[3]], scatter_plot[[4]], 
                    ncol=2, nrow=2, common.legend = TRUE, legend="right")
plot_gg <- arrangeGrob(plot_g, bottom=textGrob(paste0("Data updated: ", globalTime), x=0.5, vjust=0.05, gp=gpar(fontsize=15)))
ggsave(paste0(figures.path, "covidtracking_yugroup_state_log10.jpeg"), plot_gg, width=15, height=10)


# =================================
# NATIONAL LEVEL CONSISTENCY CHECK
# =================================
rm(list=ls())

wd <- "C:/Users/LiHui/OneDrive - Harvard University/Grad_School/COVID-19/covid_tracking/"
currentTime <- Sys.time()
globalTime <- format(currentTime,c('%Y_%m_%d'))
setwd(paste0(wd, "covidtracking_pull_", globalTime))
cleaned.path <- paste0(getwd(), "/cleaned/")
figures.path <- paste0(getwd(), "/figures/")

# check that Yu Group data have been copied to the "cleaned" folder
stopifnot(file.exists(paste0(cleaned.path, "Yu_Group_US.csv")))

# load cleaned covid-tracking and CSSE
df_covidtr <- read.table(paste0(cleaned.path, "covidtracking_US.csv"), sep=",", header=TRUE)
df_covidtr$date <- format(as.Date(df_covidtr$date), '%m/%d/%y')
df_yugroup <- read.table(paste0(cleaned.path, "Yu_Group_US.csv"), sep=",", header=TRUE)
df_yugroup$date <- format(as.Date(df_yugroup$date), '%m/%d/%y')


# merge two datasets
df_merged <- merge(df_covidtr, df_yugroup, by="date",
                   suffixes = c(".covtr", ".yugroup"))

# plot Covidtracking vs Yu Group
scatter_plot <- vector("list", 4)
commonvars = c("positive", "positiveIncrease", "death","deathIncrease")
vartag = c("Cases (cumulative)", "Cases (daily increase)", "Death (cumulative)", "Death (daily increase)")

for (i in 1:4){
  dat <- df_merged[c("date", paste0(commonvars[i],".yugroup"), paste0(commonvars[i], ".covtr"))]
  dat <- na.omit(dat[dat[paste0(commonvars[i],".yugroup")] >= 1 
                     & dat[paste0(commonvars[i],".covtr")] >= 1, ])
  dat[paste0(commonvars[i],".yugroup")] <- log10(dat[paste0(commonvars[i],".yugroup")])
  dat[paste0(commonvars[i],".covtr")] <- log10(dat[paste0(commonvars[i],".covtr")])
  scatter_plot[[i]] <- ggplot(dat, aes_string(y=paste0(commonvars[i],".yugroup"), 
                                              x=paste0(commonvars[i], ".covtr"))) + 
    geom_point(size = 2.5, color = "royalblue4") + geom_abline(intercept=0, slope=1, color='red') +
    geom_text(x=0.50*max(dat[paste0(commonvars[i],".covtr")], na.rm = TRUE), 
              y=0.85*max(dat[paste0(commonvars[i],".yugroup")], na.rm = TRUE), size = 5, 
              label=paste0("Correlation: ", round(cor(df_merged[[paste0(commonvars[i],".covtr")]], 
                                                      df_merged[[paste0(commonvars[i],".yugroup")]], use = "pairwise.complete.obs"), 3))) +
    labs(x="covidTracking (log10)", y="Yu Group (log10)") +
    ggtitle(vartag[i]) + 
    theme(plot.title=element_text(hjust=0.5, size=25),
          axis.title.x=element_text(margin=margin(t=10, r=0, b=0, l=0), hjust=0.5, size=20),
          axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0), vjust=0.5, size=20),
          axis.text.x=element_text(size=20),
          axis.text.y=element_text(size=20))
}

plot_g <- ggarrange(scatter_plot[[1]], scatter_plot[[2]], scatter_plot[[3]], scatter_plot[[4]], 
                    ncol=2, nrow=2, common.legend = TRUE, legend="right")
plot_gg <- arrangeGrob(plot_g, bottom=textGrob(paste0("Data updated: ", globalTime), x=0.5, vjust=0.05, gp=gpar(fontsize=15)))
ggsave(paste0(figures.path, "covidtracking_yugroup_us_log10.jpeg"), plot_gg, width=15, height=10)

