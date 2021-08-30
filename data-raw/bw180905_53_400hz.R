## code to prepare `bw180905_53_400hz` dataset goes here
library(tidyverse)

tz <- "Etc/GMT+7"

bw180905_53_mat <- R.matlab::readMat("analysis/data/raw_data/bw180905-53.mat")

# 400 Hz acceleration
bw180905_53_400hz <- tibble(
  surge = as.vector(bw180905_53_mat$Aw400Hz[, 1]),
  dn = as.vector(bw180905_53_mat$DN400Hz),
  sway = as.vector(bw180905_53_mat$Aw400Hz[, 2]),
  heave = as.vector(bw180905_53_mat$Aw400Hz[, 3])
) %>%
  mutate(dt = as.POSIXct((dn - 719529) * 86400,
                         origin = "1970-01-01",
                         tz = "UTC") %>%
           lubridate::force_tz(tz)) %>%
  select(dt, surge, sway, heave)

usethis::use_data(bw180905_53_400hz, overwrite = TRUE)
