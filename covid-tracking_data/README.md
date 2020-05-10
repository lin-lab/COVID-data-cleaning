# Documentation for Covid Tracking Project data cleaning scripts

*Note: Need to adapt the input and output directory paths for new usage.*

* `0.data_crawling.R`:

  Download the latest datasets from Covid Tracking Project websites. The new download will not overwrite datasets downloaded from any previous days.
  
* `1.clean_covidtracking.R`:
  
  Clean and format the State-level and U.S.-level historical data. More variable information are provided in the [spreadsheet](https://docs.google.com/spreadsheets/d/1xnCAWiomeHNifh-edg6C8VDabRCjA5_MZOo4oeBsIGY/edit). 
  
  Make plots that check consistency between reported national level data and the aggregated state-level data. 

* `2.plot_covidtracking_CSSE.R`:

  Compare daily case increase, daily death increase, cumulative cases and cumulative deaths between JHU datasets and Covid Tracking Project. 
  
* `3.clean_yugroup.R`:

  Clean and reformat Yu Group dataset in RDS format (`Berkeley_Yu.county_level_merged.07April20.rds`). This script was originally prepared in order to generate comparison plots between Yu Group and Covid Tracking Project data. It can be superceded by Corbin's cleaning script. 
  
* `4.plot_covidtracking_Yu.R`:

  Generate consistency plots to compare data from the Yu Group vs Covid Tracking Project.     
  
