#' Find beats in BCG with local surge range cue
#'
#' @param surge Filtered surge signal
#' @param window_s Width of search window in seconds (0.25 s by default)
#' @param fs_hz Sampling frequency of surge in Hz (400 Hz by default)
#'
#' @return A logical vector the same length as surge indicating a heart beat
#' @export
find_beats_lsr <- function(surge, window_s = 0.25, fs_hz = 400) {
  window <- as.integer(window_s * fs)

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

find_beats_jerk <- function(jerk, window_s = 1.8, fs_hz = 10) {
  window <- as.integer(window_s * fs)
  jerk_smooth <- tma(jerk, window * 10)
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
