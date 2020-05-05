#!/bin/bash

# Script to download, process, and clean current Yu group Covid-19 data

# contact: qcorbin@hsph.harvard.edu

# This script updates the Yu group git respository, which includes current cases and deaths

# It then downloads the Yu group pkl data from Google Drive 

# It then calls clean_yu_data.R to clean the data and write.

# Files will be marked with today's date. 

# python binary
python="python"

# github repo dir  
git_dir="covid19-severity-prediction/"
                                                                                                       # update git repo
if [ -d "$DIRECTORY" ]; then
	cd $git_dir && git pull && cd -                                                                
else
	git clone https://github.com/Yu-Group/covid19-severity-prediction.git
fi

mkdir -p raw_data
mkdir -p cleaned_data

# URL for Yu Group pkl file, from this directory:
#	https://drive.google.com/drive/u/0/folders/1OfeUn8RcOfkibgjtuuVt2z9ZtzC_4Eq5
# The file name in the drive is 'df_county_level_cached.pkl'
# Note: this URL may need to be updated. 
gdrive_url="https://drive.google.com/open?id=1OzCSD0m-mnRaEo_VZ2H_GInawbvRiWt8"

# URL of gdown perl script
gdown_url="https://raw.githubusercontent.com/circulosmeos/gdown.pl/master/gdown.pl"

# download pkl file
/usr/bin/perl <(curl $gdown_url) $gdrive_url "${git_dir}/data/df_county_level_cached.pkl"

# Today;s date
DATE=$(date +'%m_%d_%Y')

# where to write the merged pkl file
raw_out_name="raw_data/yu_data.${DATE}.pkl"

# create the merged file
$python <(echo "
import os
import sys
sys.path.append(\"$git_dir\")
os.chdir(\"$git_dir\")
import load_data
df = load_data.load_county_level(data_dir='data')
df.to_pickle(\"${raw_out_name}\")
")

# store data in long format
Rscript process_yu_data.R ${DATE}



