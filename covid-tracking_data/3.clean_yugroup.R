#!/usr/bin/R

#-------------------------------------------------------
# Lin Lab - Covid19 Project
# Yu Group county-level data aggregation
# Input date downloaded from: 
# ~xlin/Lab/corbinq/COVID19/covid19-severity-prediction/
#-------------------------------------------------------
rm(list=ls())
library(grid)
library(gridExtra)
library(ggpubr)
library(data.table)
library(pracma)
library(tidyr)

# ==============
# Data cleaning
# ==============
wd = "C:/Users/LiHui/OneDrive - Harvard University/Grad_School/COVID-19/Yu_Group/"

# Load county-level data based on Yu Group
dt <- readRDS(paste0(wd, "Berkeley_Yu.county_level_merged.07April20.rds"))
setnames(dt, "CountyNamew/StateAbbrev", "county_state")
dt$county_state <- factor(dt$county_state)

# Use columns: "#Cases_[DATE]", "#Deaths_[DATE]"
dt_countyInfo = dt[, c("State","CountyName","county_state")]
case_cols = c("county_state", grep('#Cases', colnames(dt), value=TRUE))
dt_cases_wide = dt[, ..case_cols]
death_cols = c("county_state", grep('#Deaths', colnames(dt), value=TRUE))
dt_deaths_wide = dt[, ..death_cols]

# Convert data format wide --> long
dt_cases_long <- gather(dt_cases_wide, date, positive, 
                  grep('#Cases', colnames(dt_cases_wide), value=TRUE), 
                  factor_key=TRUE)
dt_cases_long$date <- substr(dt_cases_long$date, 8, 20)

dt_deaths_long <- gather(dt_deaths_wide, date, death, 
                        grep('#Deaths', colnames(dt_deaths_wide), value=TRUE), 
                        factor_key=TRUE)
dt_deaths_long$date <- substr(dt_deaths_long$date, 9, 20)

# Glue countyInfo, long cases and long deaths back together
dt_cleaned <- merge(dt_cases_long, dt_deaths_long, by = c("county_state", "date"))
dt_cleaned <- merge(dt_countyInfo, dt_cleaned, by = "county_state")


# =======================================
# Formatting data and writing to files
# =======================================
# Aggregate data by state
county_agg_day <- aggregate(x = dt_cleaned[, c("positive", "death")],
                           FUN = sum, na.rm = TRUE, 
                           by = list(date = dt_cleaned$date, state = dt_cleaned$State))

# Format date column
county_agg_day$date <- as.Date(county_agg_day$date, format = "%m/%d/%y")

# Calculate and add daily increase columns
dt_county_agg <- data.table(county_agg_day)
setorder(dt_county_agg, state, date)
dt_county_agg[, positiveIncrease := c(NA, diff(positive)), by = state]
dt_county_agg[, deathIncrease := c(NA, diff(death)), by = state]

# Save state-date level data to file
write.table(dt_county_agg, paste0(wd, "../covid_tracking/cleaned/Yu_Group_State.csv"), append = FALSE, sep = ",",
            row.names = FALSE, col.names = TRUE)

# Aggregate data by date
state_agg_day <- aggregate(x = dt_county_agg[, c("positive", "positiveIncrease", "death", "deathIncrease")],
                           FUN = sum, na.rm = TRUE,
                           by = list(date = dt_county_agg$date))

# Save national level data to file
write.table(state_agg_day, paste0(wd, "../covid_tracking/cleaned/Yu_Group_US.csv"), append = FALSE, sep = ",",
            row.names = FALSE, col.names = TRUE)
