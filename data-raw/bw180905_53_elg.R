## code to prepare `bw180905_53_elg` dataset goes here
library(tidyverse)

tz <- "Etc/GMT+7"

# Regions eligible for BCG
# Manually identified as motionless periods (bottom phase of dive, no fluking)
bw180905_53_elg <- read_csv("analysis/data/raw_data/bcg_eligible.csv") %>%
  mutate(across(c(start, stop), ~ lubridate::force_tz(.x, tz)),
         region_id = row_number()) %>%
  select(dive, region_id, start, stop)

usethis::use_data(bw180905_53_elg, overwrite = TRUE)
