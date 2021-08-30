## code to prepare `corky_10hz` dataset goes here
library(tidyverse)

corky_10hz_mat <- R.matlab::readMat(
  "analysis/data/raw_data/corky210816-97 10Hzprh.mat"
)
corky_10hz <- tibble(
  dn = as.vector(corky_10hz_mat$DN),
  aw = as.matrix(corky_10hz_mat$Aw),
  pitch = as.vector(corky_10hz_mat$pitch),
  roll = as.vector(corky_10hz_mat$roll),
  head = as.vector(corky_10hz_mat$head)
) %>%
  mutate(dt = as.POSIXct((dn - 719529) * 86400,
                         origin = "1970-01-01",
                         tz = "UTC") %>%
           lubridate::force_tz(tz),
         jerk = njerk(aw * 9.8, 10)) %>%
  select(dt, pitch, roll, head, jerk)

usethis::use_data(corky_10hz, overwrite = TRUE)
