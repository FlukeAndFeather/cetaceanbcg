#' Find beats in BCG with local surge range cue
#'
#' @param surge Filtered surge signal
#' @param window_s Width of search window in seconds (0.25 s by default)
#' @param fs_hz Sampling frequency of surge in Hz (400 Hz by default)
#'
#' @return A logical vector the same length as surge indicating a heart beat
#' @export
find_beats_lsr <- function(surge, window_s = 0.25, fs_hz = 400) {
  window <- floor(window_s * fs)

  # Local range of surge
  surge_range <- local_range(surge, window)

  # Find local peaks in smoothed local range
  local_range_max <- RcppRoll::roll_max(surge_range, window, fill = NA)
  range_peaks <- surge_range == local_range_max

  # Align range peaks with surge peaks
  beats <- logical(length(range_peaks))
  i <- 1
  for (peak in which(range_peaks)) {
    i1 <- max(peak - window / 2, 1)
    i2 <- min(peak + window / 2, length(surge))
    b <- which.max(surge[i1:i2]) + i1 - 1
    beats[b] <- TRUE
    i <- i + 1
  }

  beats
}

#' Local range of signal
#'
#' @param x Original signal
#' @param k Window width
#'
#' @return Smoothed local range of x
#' @export
local_range <- function(x, k) {
  local_range_max <- RcppRoll::roll_max(x, k, fill = NA)
  local_range_min <- RcppRoll::roll_min(x, k, fill = NA)
  result <- local_range_max - local_range_min
  tma(result, k)
}

#' Triangular moving average
#'
#' @param x Original signal
#' @param k Window width
#'
#' @return Smoothed x
#' @export
tma <- function(x, k) {
  x %>%
    RcppRoll::roll_mean(k, fill = NA) %>%
    RcppRoll::roll_mean(k, fill = NA)
}

#' Find beats in BCG with jerk cue
#'
#' @param jerk Smoothed jerk signal
#' @param surge Filtered surge signal
#' @param window_s Width of search window in seconds (1.8 s by default)
#' @param fs_hz Sampling frequency of jerk and surge in Hz (400 Hz by default)
#'
#' @return
#' @export
find_beats_jerk <- function(jerk, mask, window_s = 1.8, fs_hz = 400) {
  jerk[!mask] <- 0
  window <- floor(window_s * fs_hz)

  # Find local peaks in jerk
  jerkpeaks <- pracma::findpeaks(jerk, minpeakdistance = window)
  jerk_hts <- jerkpeaks[, 1]
  jerk_pks <- jerkpeaks[, 2]
  jerk_prm <- peak_prominences(jerk, jerk_pks)

  # Find major peaks

  # Distance from largest peak in height/prominence space
  dist <- sqrt((max(jerk_hts) - jerk_hts)^2 + (max(jerk_prm) - jerk_prm)^2)
  # Keep all peaks up to knee in dist
  dist2 <- signal::sgolayfilt(sort(dist), p = 2, n = 25, m = 2)
  knee <- which.max(dist2)
  dist_thr <- sort(dist)[knee]
  major_peaks <- jerk_pks[dist < dist_thr]

  beats <- logical(length(jerk))
  beats[major_peaks] <- TRUE
  beats
}
