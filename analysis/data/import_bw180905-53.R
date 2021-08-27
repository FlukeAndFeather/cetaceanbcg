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

saveRDS(bw180905_53_400hz, "analysis/data/derived_data/bw180905_53_400hz.rds")

# 10 Hz PRH data
bw180905_53_10hz <- tibble(
  dn = as.vector(bw180905_53_mat$DN10Hz),
  depth = as.vector(bw180905_53_mat$depth10Hz),
  jerk = njerk(as.matrix(bw180905_53_mat$Aw10Hz) * 9.8, 10)
) %>%
  mutate(dt = as.POSIXct((dn - 719529) * 86400,
                         origin = "1970-01-01",
                         tz = "UTC") %>%
           lubridate::force_tz(tz)) %>%
  select(dt, depth, jerk)

saveRDS(bw180905_53_10hz, "analysis/data/derived_data/bw180905_53_10hz.rds")

# Regions eligible for BCG
# Manually identified as motionless periods (bottom phase of dive, no fluking)
b180905_53_elg <- read_csv("analysis/data/raw_data/bcg_eligible.csv") %>%
  mutate(across(c(start, stop), ~ lubridate::force_tz(.x, tz)),
         region_id = row_number()) %>%
  select(dive, region_id, start, stop)

saveRDS(b180905_53_elg, "analysis/data/derived_data/bw180905_53_elg.rds")
