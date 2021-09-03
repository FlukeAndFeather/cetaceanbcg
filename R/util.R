#' Norm of the jerk vector
#'
#' Uses Savitzky-Golay filtering to de-noise before differentiating acceleration
#'
#' @param A Tri-axial acceleration (n x 3 matrix)
#' @param fs Sampling rate (Hz)
#' @param p Order of Savitzky-Golay filter (3 by default)
#' @param n Window size of Savitzky-Golay filter (11 by default)
#'
#' @return Norm of the jerk vector of A
#' @export
njerk <- function(A, fs, p = 3, n = 11) {
  n <- n + 1 - n %% 2
  jerk <- apply(A, 2, function(axis) signal::sgolayfilt(axis, p, n, m = 1))
  apply(jerk, 1, function(row) sqrt(sum(row^2)))
}

#' Calculate beats-per-minute
#'
#' @param beats Logical vector indicating heart beats
#' @param dt POSIXct vector of timestamps
#'
#' @return Where `beats` is TRUE, the BPM. Otherwise NA.
#' @export
bpm <- function(beats, dt) {
  result <- rep(NA, length(beats))
  beats_i <- which(beats)
  beats_dt <- dt[beats_i]
  next_beat <- c(dt[beats_i[-1]], NA)
  result[beats] <- 1 / as.numeric(next_beat - beats_dt, unit = "mins")
  result
}

#' Find prominences of peaks
#'
#' @param x Vector of numeric values
#' @param peaks Indices of peak locations
#'
#' @return Prominences of peaks in `x` at indices `peaks`
#' @export
peak_prominences <- function(x, peaks) {
  # Peaks have to be in ascending order for finding adjacent valleys
  peaks2 <- sort(peaks)
  result <- numeric(length(peaks2))
  for (i in seq_along(peaks2)) {
    # Highest peak case
    if (x[peaks2[i]] == max(x[peaks2])) {
      result[i] <- x[peaks2[i]] - min(x, na.rm = TRUE)
      next
    }

    # Find closest, higher peaks and the valleys between
    higher <- which(x[peaks2] >= x[peaks2[i]])
    higher_left <- suppressWarnings(max(higher[higher < i]))
    if (is.infinite(higher_left)) {
      valley_left <- min(x[1:peaks2[i]], na.rm = TRUE)
    } else {
      valley_left <- min(x[peaks2[higher_left]:peaks2[i]], na.rm = TRUE)
    }
    higher_right <- suppressWarnings(min(higher[higher > i]))
    if (is.infinite(higher_right)) {
      valley_right <- min(x[peaks2[i]:length(x)], na.rm = TRUE)
    } else {
      valley_right <- min(x[peaks2[i]:peaks2[higher_right]], na.rm = TRUE)
    }

    result[i] <- x[peaks2[i]] - max(valley_left, valley_right)
  }
  # Re-order results to match input peaks order
  result[rank(peaks)]
}
