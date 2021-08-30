library(shinybusy)

filter_thr0 <- c(1.5, 7.5)
filter_order0 <- 4
window0 <- 1.8
yaw_offset0 <- 0

n_pts <- 1e3
bw180905_53_10hz_sparse <- bw180905_53_10hz[floor(seq(1, nrow(bw180905_53_10hz), length.out = n_pts)), ]

update_results <- function(hyperparams) {
  show_modal_progress_circle(.10, "Filtering acceleration")

  window <- hyperparams$window
  fs <- 400
  fny <- fs / 2
  acc_filter <- signal::butter(hyperparams$filter_order,
                               hyperparams$filter_thr / fny,
                               type = "pass")
  apply_filter <- function(x) signal::filtfilt(acc_filter, x)

  rid_fun <- function(x, xout) {
    approx(x, bw180905_53_elg$region_id, xout, "constant", yleft = 0)$y
  }
  bw_400hz <- bw180905_53_400hz %>%
    mutate(
      across(c(surge, sway, heave), apply_filter, .names = "{.col}_filt"),
      rid_left = rid_fun(bw180905_53_elg$start, dt),
      rid_right = rid_fun(bw180905_53_elg$stop, dt) + 1,
      region_id = ifelse(rid_left == rid_right, rid_left, NA),
      jerk = njerk(cbind(surge_filt, sway_filt, heave_filt), fs),
      jerk_smooth = tma(jerk, window * fs)
    )  %>%
    select(-c(rid_left, rid_right))

  update_modal_progress(.40, "Finding heartbeats")

  bw_400hz_beats <- bw_400hz %>%
    filter(!is.na(region_id)) %>%
    group_by(region_id) %>%
    mutate(surge_range = local_range(surge_filt, window * fs),
           bcg_beat = find_beats_jerk(jerk_smooth, surge_filt, window, fs),
           bcg_bpm = bpm(bcg_beat, dt)) %>%
    ungroup() %>%
    select(dt, surge_range, bcg_beat, bcg_bpm)

  update_modal_progress(.80, "Wrapping up")

  bw_400hz <- bw_400hz %>%
    left_join(bw_400hz_beats, by = "dt")

  bw_10hz <- bw180905_53_10hz
  bw_10hz$bcg_beat <- FALSE
  bcg_idx <- approx(bw_10hz$dt,
                    seq_along(bw_10hz$bcg_beat),
                    bw_400hz$dt[bw_400hz$bcg_beat],
                    "constant")$y
  bw_10hz$bcg_beat[bcg_idx] <- TRUE

  remove_modal_spinner()

  list(bw_400hz = bw_400hz, bw_10hz = bw_10hz)
}

bw_dive_plot <- function(bw_10hz, depth_click, dive_click) {
  bw_10hz <- bw_10hz[floor(seq(1, nrow(bw_10hz), length.out = n_pts)), ]
  if (is.null(depth_click$x)) {
    return(NULL)
  }
  span <- 15 * 60 # 15 minutes
  t1 <- depth_click$x - span / 2
  t2 <- depth_click$x + span / 2
  t1_diff <- first(bw_10hz$dt) - t1
  if (t1_diff > 0) {
    t1 <- first(bw_10hz$dt)
    t2 <- t2 + t1_diff
  }
  t2_diff <- last(bw_10hz$dt) - t2
  if (t2_diff < 0) {
    t2 <- last(bw_10hz$dt)
    t1 <- t1 + t2_diff
  }

  region_visible <- t1 < bw180905_53_elg$stop & t2 > bw180905_53_elg$start
  depth_bound <- function(start, stop, side = c("lower", "upper")) {
    side = match.arg(side)
    d <- ifelse(side == "lower", -2, 2)
    f <- ifelse(side == "lower", min, max)
    f(bw_10hz$depth[bw_10hz$dt >= start & bw_10hz$dt <= stop]) + d
  }
  expand_regions <- function(dat, key) {
    tibble(x = c(dat$start, dat$start, dat$stop, dat$stop),
           y = c(dat$lower, dat$upper, dat$upper, dat$lower))
  }
  regions <- tibble(
    region_id = bw180905_53_elg$region_id[region_visible],
    start = bw180905_53_elg$start[region_visible],
    stop = bw180905_53_elg$stop[region_visible],
    lower = map2_dbl(start, stop, depth_bound, side = "lower"),
    upper = map2_dbl(start, stop, depth_bound, side = "upper")
  ) %>%
    group_by(region_id) %>%
    group_modify(expand_regions) %>%
    ungroup()

  dive_data <- filter(bw_10hz, between(dt, t1, t2))
  ggplot(dive_data, aes(dt, depth)) +
    geom_polygon(aes(x, y, group = region_id), regions,
                 color = "grey80", fill = "grey90") +
    geom_line(size = 0.2) +
    annotate_click(dive_click$x, dive_click$y) +
    scale_y_reverse() +
    coord_cartesian(xlim = c(t1, t2)) +
    theme_minimal()
}

bw_bcg_plot <- function(bw_400hz, dive_click) {
  if (is.null(dive_click$x)) {
    return(NULL)
  }

  ca_tz <- "Etc/GMT+7"
  t <- theme_minimal() +
    theme(axis.title.x = element_blank(),
          panel.grid.minor = element_blank())

  rid <- which(dive_click$x >= bw180905_53_elg$start &
                 dive_click$x <= bw180905_53_elg$stop)
  if (length(rid) == 0) {
    return(NULL)
  }
  bcg_sample <- filter(bw_400hz, region_id == rid)

  # A: Surge + LSR
  bcg_surge_lsr <- bcg_sample %>%
    rename(Surge = surge_filt, LSR = surge_range) %>%
    pivot_longer(c(Surge, LSR),
                 names_to = "var",
                 values_to = "val")

  p1 <- ggplot(bcg_surge_lsr, aes(dt, val, linetype = var)) +
    geom_line(size = 0.2) +
    geom_point(data = filter(bcg_surge_lsr, bcg_beat), color = "blue") +
    scale_x_datetime(date_labels = "%H:%M:%S") +
    scale_linetype_manual(values = c(Surge = "solid", LSR = "dashed")) +
    labs(y = bquote(Surge~(m~s^{-2}))) +
    t +
    theme(legend.position = "none")

  # B: BPM

  p2 <- ggplot(filter(bcg_sample, bcg_beat), aes(dt, bcg_bpm)) +
    geom_line() +
    geom_point(color = "blue") +
    scale_x_datetime(date_labels = "%H:%M:%S") +
    labs(y = "Heart rate (bpm)") +
    expand_limits(x = range(bcg_sample$dt), y = 0) +
    t

  # C: Sway, heave

  bcg_sway_heave <- bcg_sample %>%
    rename(Sway = sway_filt, Heave = heave_filt) %>%
    pivot_longer(c(Sway, Heave),
                 names_to = "axis",
                 values_to = "acceleration")

  p3 <- ggplot(bcg_sway_heave, aes(dt, acceleration)) +
    geom_line(size = 0.2) +
    geom_point(data = filter(bcg_sway_heave, bcg_beat), color = "blue") +
    scale_x_datetime(date_labels = "%H:%M:%S") +
    facet_grid(axis ~ .) +
    labs(y = bquote(Acc~(m~s^{-2}))) +
    t

  # D: Jerk

  p4 <- ggplot(bcg_sample, aes(dt, jerk_smooth)) +
    geom_line(size = 0.2) +
    geom_point(data = filter(bcg_sample, bcg_beat), color = "blue") +
    scale_x_datetime(date_labels = "%H:%M:%S") +
    labs(y = bquote(Jerk~(m~s^{-3}))) +
    expand_limits(y = 0) +
    t

  cowplot::plot_grid(p1, p2, p3, p4,
                     align = "v",
                     axis = "lr",
                     nrow = 4,
                     rel_heights = c(1.5, 0.75, 1, 1),
                     labels = "AUTO")
}

convert_clickx <- function(x) {
  as.POSIXct(x, origin = "1970-01-01", tz = lubridate::tz(bw180905_53_elg$start))
}

annotate_click <- function(x, y) {
  if (!is.null(x)) {
    annotate("point",
             x = x, y = y,
             size = 2, color = "red")
  } else {
    NULL
  }
}
