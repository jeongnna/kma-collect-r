#!/bin/sh
mkdir data/
Rscript weather_collect.R infile_daily
Rscript weather_collect.R infile_hourly
Rscript preprocess.R
