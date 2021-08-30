## code to prepare `ecg_beats` dataset goes here
library(tidyverse)

set_date <- function(datetime, new_date) {
  result <- rep(new_date, length(datetime))
  lubridate::hour(result) <- lubridate::hour(datetime)
  lubridate::minute(result) <- lubridate::minute(datetime)
  lubridate::second(result) <- lubridate::second(datetime)
  result
}

tz <- "Etc/GMT+7"
corky_date <- lubridate::ymd_hm("2021-08-16 00:00", tz = tz)

ecg_beats <- readxl::read_excel(
  "analysis/data/raw_data/Corky 8-16-21 Heart rate data for BCG test.xlsx",
  col_names = FALSE,
  na = "--"
) %>%
  select(1:3) %>%
  set_names(c("dt", "ecg", "bpm")) %>%
  mutate(dt = set_date(dt, corky_date))

usethis::use_data(ecg_beats, overwrite = TRUE)
