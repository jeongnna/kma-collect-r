library(tidyverse)
library(lubridate)
library(rjson)
source("src/na_fill_functions.R")


# Arguments ---------------------------------------------------------------

cmdargs <- commandArgs(TRUE)
args <- fromJSON(file = cmdargs[1])

start_year <- args$collect$start_year
end_year <- args$collect$end_year
date_cd <- args$collect$date_cd
infile <- args$path$raw
outfile <- args$path$processed
selected <- unlist(args$features)


# Process -----------------------------------------------------------------

convert_fn <- list(
  "interpolate" = na_interpolate,
  "zero" = na_zero
)

if (date_cd == "HR") {
  st <- ymd_h(str_c(start_year, "-01-01 00"))
  ed <- ymd_h(str_c(end_year, "-12-31 23"))
  period_len <- as.integer(difftime(ed, st, units = "hour"))
  time_set <- tibble(time = ymd_h("2009-01-01 00") + hours(1) * c(0, seq_len(period_len)))
  
} else if (date_cd == "DAY") {
  st <- as_date(str_c(start_year, "-01-01"))
  ed <- as_date(str_c(end_year, "-12-31"))
  period_len <- as_date("2018-12-31") - as_date("2009-01-01")
  time_set <- tibble(time = as_date("2009-01-01") + c(0, seq_len(period_len)))
}

df <- infile %>% 
  read_csv() %>% 
  select(c("STN_ID", "TM", names(selected))) %>% 
  right_join(time_set, by = c("TM" = "time"))

for (i in seq_along(selected)) {
  ft <- selected[i]
  nm <- names(ft)
  fn <- convert_fn[[ft]]
  df[[nm]] <- fn(df[[nm]])
}


# Save file ---------------------------------------------------------------

message("Saving preprocessed data")
if (file.exists(outfile)) {
  warning("Destination file is already exists. It replaced with new file.")
}
write_csv(df, outfile)
