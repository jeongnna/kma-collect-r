#!/bin/sh
mkdir data/
Rscript src/weather_collect.R inputfiles/daily.json
Rscript src/preprocess.R inputfiles/daily.json
Rscript src/weather_collect.R inputfiles/hourly.json
Rscript src/preprocess.R inputfiles/hourly.json
