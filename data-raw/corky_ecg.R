## code to prepare `corky_ecg` dataset goes here

# .UBC is a somewhat funky text file produced by the ECG recorder's software
corky_ecg <- suppressWarnings(readLines("analysis/data/raw_data/Corky 08-16-21 old float Plus K unit  for BCG test .ubc"))
# Drop the header lines
corky_ecg <- corky_ecg[-c(1, 2)]
# Replace weird control characters with "|", which will be the column delimiter
corky_ecg <- gsub("[^0-9:/.]", "|", corky_ecg)
# Convert to data.frame. Columns are date, time, (uncalibrated) ecg
corky_ecg <- strsplit(corky_ecg, "[|]+")
corky_ecg <- data.frame(matrix(unlist(corky_ecg), ncol = 3, byrow = TRUE))
# Parse datetime
tz <- "Etc/GMT+7"
corky_ecg$dt <- lubridate::mdy_hms(paste(corky_ecg[, 1], corky_ecg[, 2]), tz = tz)
# Calibrate ECG data using scaling factor of 0.15
corky_ecg$ecg_uV <- as.numeric(corky_ecg[, 3]) * 0.15
# Keep only datetime and ecg
corky_ecg <- corky_ecg[, c("dt", "ecg_uV")]

usethis::use_data(corky_ecg, overwrite = TRUE)
