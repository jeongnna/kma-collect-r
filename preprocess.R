library(tidyverse)
library(lubridate)


# Functions ---------------------------------------------------------------

na_zero <- function(x) {
  x[is.na(x)] <- 0
  x
}

na_interpolate <- function(x) {
  is_not_na <- function(x) {
    !is.na(x)
  }
  n <- length(x)
  if (is.na(x[1])) {
    first_idx <- detect_index(x, is_not_na)
    x[1:first_idx] <- x[first_idx]
  }
  if (is.na(x[n])) {
    last_idx <- detect_index(x, is_not_na, .dir = "backward")
    x[last_idx:n] <- x[last_idx]
  }
  i <- 2
  while (i < n) {
    if (is.na(x[i])) {
      j <- detect_index(x[(i + 1):n], is_not_na)
      x[(i - 1):(i + j)] <- seq(x[i - 1], x[i + j], length.out = j + 2)
      i <- i + j
    }
    i <- i + 1
  }
  x
}


# Process -----------------------------------------------------------------

infile <- "data/weather_daily.csv"
outfile <- "data/weather_daily_pp.csv"

period_len <- as_date("2018-12-31") - as_date("2009-01-01")
date_set <- tibble(date = as_date("2009-01-01") + c(0, seq_len(period_len)))

infile %>% 
  read_csv() %>% 
  select(STN_ID, TM, MAX_TA, MIN_TA, AVG_WS, AVG_TCA, AVG_TD) %>% 
  right_join(date_set, by = c("TM" = "date")) %>% 
  mutate_at(vars(-c("STN_ID", "TM")), na_interpolate) %>% 
  write_csv(outfile)


infile <- "data/weather_hourly.csv"
outfile <- "data/weather_hourly_pp.csv"

period_len <- as.integer(difftime(ymd_h("2018-12-31 23"), ymd_h("2009-01-01 00"), units = "hour"))
time_set <- tibble(time = ymd_h("2009-01-01 00") + hours(1) * c(0, seq_len(period_len)))

infile %>% 
  read_csv() %>% 
  select(STN_ID, TM, RN, TA, WS, TD, DC10_TCA) %>% 
  right_join(time_set, by = c("TM" = "time")) %>% 
  mutate_at("RN", na_zero) %>% 
  mutate_at(vars(-c("STN_ID", "TM", "RN")), na_interpolate) %>% 
  write_csv(outfile)
