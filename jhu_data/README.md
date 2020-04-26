# Data cleaning code for JHU data

The data is downloaded from the [JHU CSSE
repository](https://github.com/CSSEGISandData/COVID-19).

To run the data cleaning code:

1. Run `./01_get-data.sh` to download the data. This uses wget and tar
   to download and unzip the data.
2. Run `Rscript 02_munge-data.R` to process the data. This script will
   reformat the data into long format and aggregate the county-level
   data into state-level data. The cleaned will be created in the
   `CSSE_cleaned` folder. Additionally, in the `figures` folder, this
   script will create plots checking the aggregated state-level data to
   the country level data.
