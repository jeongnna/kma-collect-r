library(tidyverse)
library(lubridate)
library(rjson)
library(httr)
library(XML)


# Arguments ---------------------------------------------------------------

cmdargs <- commandArgs(TRUE)
args <- fromJSON(file = cmdargs[1])

stn_id <- args$collect$stn_id
start_year <- args$collect$start_year
end_year <- args$collect$end_year
date_cd <- args$collect$date_cd
outfile <- args$path$raw

key <- read_lines("api_key")


# Preparation -------------------------------------------------------------

set_config(config(ssl_verifypeer = 0L))

end_of_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

set_url <- function(start_date, end_date, stn_id, key, date_cd) {
  if (!all(is.Date(start_date), is.Date(end_date))) {
    stop("`date` must be a Date object")
  }
  
  n_dates <- as.integer(diff.Date(c(start_date, end_date))) + 1
  start_date <- format(start_date, "%Y%m%d")
  end_date <- format(end_date, "%Y%m%d")
  
  str_c(
    "https://data.kma.go.kr/apiData/getData?",
    "type=xml",
    "&dataCd=ASOS",
    "&dateCd=", date_cd,
    "&startDt=", start_date,
    ifelse(date_cd == "HR", "&startHh=00", ""),
    "&endDt=", end_date,
    ifelse(date_cd == "HR", "&endHh=23", ""),
    "&stnIds=", stn_id,
    "&schListCnt=", n_dates * ifelse(date_cd == "HR", 24, 1),
    "&pageIndex=1",
    "&apiKey=", key
  )
}


# Process -----------------------------------------------------------------

# collect
df <- NULL
years <- start_year:end_year
for (yr in years) {
  cat("year", yr, "\n")
  for (mn in 1:12) {
    cat("  month", mn)
    
    # set dates
    st <- as_date(str_c(yr, mn, 1, sep = "-"))
    ed <- as_date(str_c(yr, mn, end_of_month[mn], sep = "-"))
    if (mn == 2) ed <- ed + leap_year(yr)
    
    # send request
    url <- set_url(st, ed, stn_id, key, date_cd)
    cat(" sending request... ")
    response <- GET(url)
    Sys.sleep(1)
    
    # parse xml and collect data
    x <- try(xmlParse(response), silent = TRUE)
    rm(response)
    if (inherits(x, "try-error")) {
      cat("fail\n")
      message(x[1])
    } else {
      xr <- xmlRoot(x)
      xl <- xmlToList(xr)
      cat(xl$msg, "\n")
      tmp_df <- bind_rows(xl[names(xl) == "info"])
      df <- bind_rows(df, tmp_df)
      
      # check date
      if (any(year(tmp_df$TM) != yr)) {
        message("Warning: inconsistent year detected.")
      }
      if (any(month(tmp_df$TM) != mn)) {
        message("Warning: inconsistent month detected.")
      }
      
      # check number of rows
      n_dates <- as.integer(diff.Date(c(st, ed))) + 1
      n_expect <- n_dates * ifelse(date_cd == "HR", 24, 1)
      n_rows <- nrow(tmp_df)
      if (n_expect != n_rows) {
        message("Warning: Number of rows does not match with the expected.")
        message("expected : ", n_expect)
        message("actual   : ", n_rows)
      }
    }
  }
}

# correct data types
if (date_cd == "HR") {
  not_numeric <- c("TM", "CLFM_ABBR_CD", "STN_NM")
  time_conversion <- ymd_h
} else {
  not_numeric <- c("TM", "STN_NM", "ISCS")
  time_conversion <- ymd
}
df <- df %>% 
  mutate_at("TM", time_conversion) %>% 
  mutate_at(vars(-not_numeric), as.numeric)


# Save file ---------------------------------------------------------------

message("Saving colleted data")
if (file.exists(outfile)) {
  warning("Destination file is already exists. It replaced with new file.")
}
write_csv(df, outfile)
