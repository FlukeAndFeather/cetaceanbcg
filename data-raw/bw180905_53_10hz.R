## code to prepare `bw180905_53_10hz` dataset goes here
library(tidyverse)

tz <- "Etc/GMT+7"

bw180905_53_mat <- R.matlab::readMat("analysis/data/raw_data/bw180905-53.mat")

# 10 Hz PRH data
bw180905_53_10hz <- tibble(
  dn = as.vector(bw180905_53_mat$DN10Hz),
  depth = as.vector(bw180905_53_mat$depth10Hz),
  aw = as.matrix(bw180905_53_mat$Aw10Hz)
) %>%
  mutate(dt = as.POSIXct((dn - 719529) * 86400,
                         origin = "1970-01-01",
                         tz = "UTC") %>%
           lubridate::force_tz(tz)) %>%
  select(dt, depth, aw)

usethis::use_data(bw180905_53_10hz, overwrite = TRUE)
