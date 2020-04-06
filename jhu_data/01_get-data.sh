#!/bin/bash

GIT_URL="https://github.com/CSSEGISandData/COVID-19.git"
OUT_DIR="JHU_CSSE_COVID-19"

if [ -d $OUT_DIR ]
then
    echo "Updating ${OUT_DIR} with git pull..."
    git -C $OUT_DIR pull
else
    echo "Cloning JHU data to directory ${OUT_DIR}..."
    git clone $GIT_URL $OUT_DIR
fi
