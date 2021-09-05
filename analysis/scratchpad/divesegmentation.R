split_dives <- function(dt, depth, surface = 0, min_depth = 0, min_dur = 0) {
  dive_start <- depth >= surface & lag(depth) < surface
  dive_start[1] <- FALSE
  dive_id <- cumsum(dive_start)
  dive_id[depth < surface | dive_id == 0] <- NA
  dive_id <- factor(dive_id)
  dive_depth <- tapply(depth, dive_id, max)
  dive_dur <- tapply(dt,
                     dive_id,
                     function(x) as.numeric(max(x) - min(x), unit = "secs"))
  not_dives <- which(dive_depth < min_depth | dive_dur < min_dur)
  dive_id[dive_id %in% not_dives] <- NA
  dive_id <- droplevels(dive_id)
  levels(dive_id) <- seq_along(levels(dive_id))
  dive_id
}

segment_phases <- function(depth, dive_id) {
  by_dive <- split(depth, dive_id) %>%
    lapply(function(x) {
      i <- seq_along(x)
      depth_lm <- lm(x ~ i)
      depth_seg <- segmented::segmented(depth_lm, seg.Z = ~i, npsi = 2)
      psi <- floor(depth_seg$psi[, "Est."])
      result <- character(length(x))
      result[1:psi[1]] <- "descent"
      result[psi[1]:psi[2]] <- "bottom"
      result[psi[2]:length(x)] <- "ascent"
      result
    })
  phases_all <- rep(NA, length(dive_id))
  for (dive in names(by_dive)) {
    phases_all[!is.na(dive_id) & dive_id == dive] <- by_dive[[dive]]
  }
  factor(phases_all, levels = c("descent", "bottom", "ascent"))
}

normalize_dive <- function(dive_phase, dive_id) {
  by_dive <-split(dive_phase, dive_id) %>%
    lapply(function(x) {
      result <- rep(NA, length(x))
      b <- which(x == "bottom")
      result[x == "bottom"] <- (b - min(b)) / (max(b) - min(b))
      result
    })
  all_bottom <- rep(NA, length(dive_id))
  for (dive in names(by_dive)) {
    all_bottom[!is.na(dive_id) & dive_id == dive] <- by_dive[[dive]]
  }
  all_bottom
}

bw10hz <- bw180905_53_10hz %>%
  mutate(
    dive_id = split_dives(dt, depth,
                          surface = 2, min_depth = 10, min_dur = 5 * 60),
    phase = segment_phases(depth, dive_id),
    bottom_norm = normalize_bottom_phase(phase, dive_id)
  )

bw_400hz$bottom_norm <- approx(bw10hz$dt, bw10hz$bottom_norm, bw_400hz$dt)$y

bpm_data <- bw_400hz %>%
  filter(bcg_beat) %>%
  drop_na(bcg_bpm) %>%
  select(bottom_norm, bcg_bpm)

bpm_ts <- RobustLinearReg::theil_sen_regression(bcg_bpm ~ bottom_norm, bpm_data)

bpm_pred <- tibble(bottom_norm = seq(0, 1, length.out = 100)) %>%
  mutate(bcg_bpm = predict(bpm_ts, newdata = .))

ggplot(bpm_data, aes(bottom_norm, bcg_bpm)) +
  geom_point() +
  geom_line(data = bpm_pred, color = "blue", size = 1.5) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Bottom phase",
       y = "Heartrate (bpm)") +
  expand_limits(y = 0) +
  theme_classic()

