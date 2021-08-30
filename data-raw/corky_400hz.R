## code to prepare `corky_400hz` dataset goes here
library(tidyverse)

corky_400hz_mat <- R.matlab::readMat(
  "analysis/data/raw_data/20210816-134844-CATS 97.mat"
)
tz <- "Etc/GMT+7"
corky_400hz <- tibble(
  dn = as.vector(corky_400hz_mat$Atime),
  surge = as.vector(corky_400hz_mat$Adata[, 1]),
  sway = as.vector(corky_400hz_mat$Adata[, 2]),
  heave = as.vector(corky_400hz_mat$Adata[, 3])
) %>%
  mutate(dt = as.POSIXct((dn - 719529) * 86400,
                         origin = "1970-01-01",
                         tz = "UTC") %>%
           lubridate::force_tz(tz)) %>%
  select(dt, surge, sway, heave)

usethis::use_data(corky_400hz, overwrite = TRUE)
