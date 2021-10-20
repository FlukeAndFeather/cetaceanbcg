library(cetaceanbcg)
library(tidyverse)

fs <- 400
fny <- fs / 2
window <- 1.8 # s
bw_elg <- bw180905_53_elg

bandpass_filter <- function(x, pass) {
  signal::butter(4, pass / fny, type = "pass") %>%
    signal::filtfilt(x)
}

# Apply filters as with killer whale data, but narrower band
bw_400hz <- bw180905_53_400hz %>%
  mutate(
    # Pass-band filter acceleration
    across(surge:heave,
           bandpass_filter, pass = c(1.0, 10.0),
           .names = "{.col}_filt"),
    # Calculate jerk norm and apply smoother
    njerk = njerk(cbind(surge_filt, sway_filt, heave_filt),
                  p = 4, n = window * fs),
    njerk_smooth = tma(njerk, window * fs),
    # Annotate regions
    rid_left = approx(bw_elg$start, bw_elg$region_id, dt, "constant")$y,
    rid_right = approx(bw_elg$stop, bw_elg$region_id, dt, "constant", yleft = 0)$y + 1,
    region_id = ifelse(rid_left == rid_right, rid_left, NA),
    # Find heart beats using jerk cue
    surge_range = local_range(surge_filt, window * fs),
    bcg_beat = find_beats_jerk(njerk_smooth, !is.na(region_id), window, fs)
  ) %>%
  # Calculate heart rate within each region
  group_by(region_id) %>%
  mutate(bcg_bpm = bpm(bcg_beat, dt)) %>%
  ungroup() %>%
  select(-c(rid_left, rid_right))

saveRDS(bw_400hz, "analysis/data/derived_data/bw_400hz.rds")
