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
find_beats_jerk <- function(jerk, surge, window_s = 1.8, fs_hz = 400){
  window <- floor(window_s * fs_hz)

  # Find local peaks in jerk
  jerkpeaks <- pracma::findpeaks(jerk, minpeakdistance = window)
  jerk_hts <- jerkpeaks[, 1]
  jerk_pks <- jerkpeaks[, 2]
  jerk_prm <- peak_prominences(jerk, jerk_pks)

  # Find major peaks
  if (length(jerk_hts) == 0) {
    return(logical(length(jerk)))
  } else if (length(jerk_hts) == 1) {
    major_peaks <- jerk_pks
  } else {
    kclusts <- kmeans(cbind(c(0, jerk_hts), c(0, jerk_prm)), centers = 2)
    clust_order <- apply(kclusts$centers, 2, diff)
    if (sign(clust_order[1]) != sign(clust_order[2])) {
      stop("Height and prominence thresholds don't agree.")
    }
    major_cluster <- which.max(kclusts$centers[, 1])
    major_peaks <- jerk_pks[kclusts$cluster[-1] == major_cluster]
  }

  # Align jerk peaks with surge peaks
  beats <- logical(length(jerk))
  i <- 1
  for (peak in major_peaks) {
    i1 <- max(peak - window, 1)
    i2 <- min(peak + window, length(surge))
    b <- which.max(surge[i1:i2]) + i1 - 1
    beats[b] <- TRUE
    i <- i + 1
  }

  beats
}
