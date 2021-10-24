#' Find beats in BCG
#'
#' @param cue `[numeric]` signal used for heart beat cue.
#' @param fs_hz `[numeric(1)]` Sampling frequency of surge in Hz
#' @param minperiod `[numeric(1)]` Minimum period between beats (in seconds)
#'
#' @return `[logical]` same length as cue indicating heart beats
#' @export
find_beats <- function(cue, fs_hz, minperiod) {
  peaks <- pracma::findpeaks(cue, minpeakdistance = fs_hz * minperiod)
  proms <- peak_prominences(cue, peaks[, 2])

  # Heuristically filter peaks by distance (height, prominence) to highest peak
  # Skip if less than 50 peaks
  if (nrow(peaks) >= 50) {
    dist_max <- sqrt((max(peaks[, 1]) - peaks[, 1])^2 + (max(proms) - proms)^2)
    dist_dens <- stats::density(dist_max)
    dist_thr <- dist_dens$x[pracma::findpeaks(-dist_dens$y)[, 2]]
    is_beat <- peaks[, 2][dist_max <= dist_thr]
  } else {
    is_beat <- peaks[, 2]
  }

  beats <- logical(length(cue))
  beats[is_beat] <- TRUE
  beats
}

#' Apply bandpass filter to acceleration
#'
#' @param acc `[numeric(n,1)]` or `[numeric(n,3)]` Longitudinal acceleration.
#'   Either surge only or tri-axial.
#' @param fs `[numeric(1)]` sampling frequency in Hz
#' @param upper `[numeric(1)]` upper cutoff for bandpass filter (in Hz)
#'
#' @return `[numeric]` filtered acceleration
#'
#' @export
filter_acc <- function(acc, fs, upper) {
  fny <- fs / 2
  pass <- c(1.0, upper)
  bandpass <- signal::butter(5, pass / fny, type = "pass")
  if (is.matrix(acc)) {
    apply(acc, 2, function(y) signal::filtfilt(bandpass, y))
  } else {
    signal::filtfilt(bandpass, acc)
  }
}

#' Choose peaks to keep
#'
#' Uses a heuristic approach to keep the most prominent peaks
#'
#' @param x `[numeric]` peak prominences
#'
#' @return `[logical]`
choose_peaks <- function(x) {
  x_sorted <- sort(x)
  steps <- diff(x_sorted)
  thr <- if (max(steps) > mean(steps) + 2 * stats::sd(steps)) {
    x_sorted[which.max(steps)]
  } else {
    x_sorted[1]
  }
  x > thr
}


