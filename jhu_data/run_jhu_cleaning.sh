#!/bin/bash

set -euxo pipefail

# set path to Rscript
RSCRIPT=/opt/R/4.0.4/bin/Rscript
if [[ ! -f ${RSCRIPT} ]]
then
    RSCRIPT=/usr/bin/Rscript
fi

./01_get-data.sh && $RSCRIPT --vanilla ./02_munge-data.R

cd cleaned_data
for ext in County State Global Subnational
do
    filename=JHU_COVID-19_${ext}.csv
    out_zip=${filename}.zip
    zip - ${filename} > ${out_zip}
    aws s3 cp ${out_zip} s3://hsph-covid-study/JHU_Cleaned/
done

