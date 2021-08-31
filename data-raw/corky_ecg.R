library(tidyverse)

## code to prepare `corky_ecg` dataset goes here

# .UBC is a somewhat funky text file produced by the ECG recorder's software
ubc_path <- "analysis/data/raw_data/Corky 08-16-21 old float Plus K unit  for BCG test .ubc"
tz <- "Etc/GMT+7"
corky_ecg <- suppressWarnings(readLines(ubc_path)) %>%
  # Drop the header lines
  `[`(-c(1, 2)) %>%
  # Replace weird control characters with "|", which will be the column delimiter
  gsub("[^0-9:/.]", "|", .) %>%
  # Convert to data.frame. Columns are date, time, (uncalibrated) ecg
  strsplit("[|]+") %>%
  { data.frame(matrix(unlist(.), ncol = 3, byrow = TRUE)) } %>%
  # Clean up data.frame
  set_names(c("date", "time", "ecg")) %>%
  mutate(dt = lubridate::mdy_hms(paste(date, time), tz = tz),
         ecg_uV = as.numeric(ecg) * 0.15) %>%
  select(dt, ecg_uV)

# Annotate with heartbeats. Adjust time up to 0.5 s to match R-wave peak.
nearest_peak <- function(t, dt, ecg, tol = 0.5) {
  # dt of maximum ecg near t (+- tol)
  neighborhood <- dt >= t - tol & dt <= t + tol
  dt[neighborhood][which.max(ecg[neighborhood])]
}
rwave_peaks <- sapply(ecg_beats$dt[ecg_beats$dt <= max(corky_ecg$dt)],
                      nearest_peak,
                      dt = corky_ecg$dt,
                      ecg = corky_ecg$ecg_uV) %>%
  as.POSIXct(origin = "1970-01-01", tz = tz)
rwaves <- data.frame(dt = rwave_peaks, ecg_beat = TRUE)
corky_ecg <- left_join(corky_ecg, rwaves, by = "dt") %>%
  arrange(dt) %>%
  replace_na(list(ecg_beat = FALSE))

usethis::use_data(corky_ecg, overwrite = TRUE)
