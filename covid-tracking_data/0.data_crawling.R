#!/usr/bin/R

#-------------------------------------------------------
# Lin Lab - Covid19 Project
# Download CovidTracking datasets
#-------------------------------------------------------

########## URLs
# ### States Current Values
# https://covidtracking.com/api/v1/states/current.csv
# ### States Historical Data
# https://covidtracking.com/api/v1/states/daily.csv
# ### States Information
# https://covidtracking.com/api/v1/states/info.csv
# ### US Current Values
# https://covidtracking.com/api/v1/us/current.csv
# ### US Historical Data
# https://covidtracking.com/api/us/daily.csv
# ### Counties
# https://covidtracking.com/api/counties.csv

########## Download
(currentTime <- Sys.time())
#(globalTime <- format(currentTime,c('%Y_%m_%d_%H_%M_%S')))
(globalTime <- format(currentTime,c('%Y_%m_%d')))

url.path <- "https://covidtracking.com/api"
url.filenames <- c("v1/states/current.csv",
                   "v1/states/daily.csv",
                   "v1/states/info.csv",
                   "v1/us/current.csv",
                   "us/daily.csv",
                   "counties.csv",
                   "cdc/daily.csv")

local.path <- "C:/Users/LiHui/OneDrive - Harvard University/Grad_School/COVID-19/covid_tracking/"
local.filenames <- paste0(c("states_current",
                            "states_daily",
                            "states_info",
                            "us_current",
                            "us_daily",
                            "counties",
                            "cdc_daily"),".csv")

dir.create(file.path(local.path, paste0("covidtracking_pull_", globalTime)), showWarnings = TRUE)
local.path <- paste0(local.path, "covidtracking_pull_", globalTime)

download <- function(url.filenames, local.filenames) {
  url <- file.path(url.path, url.filenames)
  local <- file.path(local.path, local.filenames)
  for (i in 1:length(url)){
    download.file(url[i], local[i])
    print(paste("Download completed at time:", currentTime))
  }
}

bin <- download(url.filenames, local.filenames)



