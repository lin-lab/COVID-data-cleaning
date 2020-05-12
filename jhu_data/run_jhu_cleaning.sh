#!/bin/bash

./01_get-data.sh && Rscript --vanilla ./02_munge-data.R
