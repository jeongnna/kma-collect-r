#!/bin/sh
mkdir data/
Rscript weather_collect.R infile_daily.txt
Rscript weather_collect.R infile_hourly.txt
Rscript preprocess.R
