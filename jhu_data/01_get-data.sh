#!/bin/bash

GIT_URL="https://github.com/CSSEGISandData/COVID-19/archive/master.tar.gz"
OUT_DIR="JHU_CSSE_COVID-19"

if [ ! -d ${OUT_DIR} ]
then
    mkdir ${OUT_DIR}
fi

echo "Downloading data from ${GIT_URL}..."
wget --quiet --output-document=/dev/stdout ${GIT_URL} |
    tar -xzvf /dev/stdin -C ${OUT_DIR} --strip-components=1
