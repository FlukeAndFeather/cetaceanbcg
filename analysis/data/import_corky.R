library(tidyverse)

# 400 Hz acceleration data
acc_400hz_mat <- R.matlab::readMat(
  "analysis/data/raw_data/20210816-134844-CATS 97.mat"
)
tz <- "Etc/GMT+7"
acc_400hz <- tibble(
  dn = as.vector(acc_400hz_mat$Atime),
  surge = as.vector(acc_400hz_mat$Adata[, 1]),
  sway = as.vector(acc_400hz_mat$Adata[, 2]),
  heave = as.vector(acc_400hz_mat$Adata[, 3])
) %>%
  mutate(dt = as.POSIXct((dn - 719529) * 86400,
                         origin = "1970-01-01",
                         tz = "UTC") %>%
           lubridate::force_tz(tz)) %>%
  select(dt, surge, sway, heave)

saveRDS(acc_400hz, "analysis/data/derived_data/acc_400hz.rds")

# 10 Hz PRH data
njerk <- function(A, fs) {
  jerk <- (A[-1, ] - A[-nrow(A), ]) / fs
  jerk <- rbind(rep(NA, 3), jerk)
  apply(jerk, 1, function(row) sqrt(sum(row^2)))
}
prh_10hz_mat <- R.matlab::readMat(
  "analysis/data/raw_data/corky210816-97 10Hzprh.mat"
)
prh_10hz <- tibble(
  dn = as.vector(prh_10hz_mat$DN),
  aw = as.matrix(prh_10hz_mat$Aw),
  pitch = as.vector(prh_10hz_mat$pitch),
  roll = as.vector(prh_10hz_mat$roll),
  head = as.vector(prh_10hz_mat$head)
) %>%
  mutate(dt = as.POSIXct((dn - 719529) * 86400,
                         origin = "1970-01-01",
                         tz = "UTC") %>%
           lubridate::force_tz(tz),
         jerk = njerk(aw * 9.8, 10)) %>%
  select(dt, pitch, roll, head, jerk)

saveRDS(prh_10hz, "analysis/data/derived_data/prh_10hz.rds")

# ECG data
set_date <- function(datetime, new_date) {
  result <- rep(new_date, length(datetime))
  lubridate::hour(result) <- lubridate::hour(datetime)
  lubridate::minute(result) <- lubridate::minute(datetime)
  lubridate::second(result) <- lubridate::second(datetime)
  result
}
corky_date <- lubridate::ymd_hm("2021-08-16 00:00", tz = tz)
ecg_beats <- readxl::read_excel(
  "analysis/data/raw_data/Corky 8-16-21 Heart rate data for BCG test.xlsx",
  col_names = FALSE,
  na = "--"
) %>%
  select(1:3) %>%
  set_names(c("dt", "ecg", "bpm")) %>%
  mutate(dt = set_date(dt, corky_date))

saveRDS(ecg_beats, "analysis/data/derived_data/ecg_beats.rds")
