#' Norm of the jerk vector
#'
#' @param A Tri-axial acceleration (n x 3 matrix)
#' @param fs Sampling rate (Hz)
#'
#' @return Norm of the jerk vector of A
#' @export
njerk <- function(A, fs) {
  jerk <- (A[-1, ] - A[-nrow(A), ]) / fs
  jerk <- rbind(rep(NA, 3), jerk)
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
  result <- numeric(length(peaks))
  for (i in seq_along(peaks)) {
    # Highest peak case
    if (x[peaks[i]] == max(x[peaks])) {
      result[i] <- x[peaks[i]] - min(x, na.rm = TRUE)
      next
    }

    # Find closest, higher peaks and the valleys between
    higher <- which(x[peaks] >= x[peaks[i]])
    higher_left <- suppressWarnings(max(higher[higher < i]))
    if (is.infinite(higher_left)) {
      valley_left <- min(x[1:peaks[i]], na.rm = TRUE)
    } else {
      valley_left <- min(x[peaks[higher_left]:peaks[i]], na.rm = TRUE)
    }
    higher_right <- suppressWarnings(min(higher[higher > i]))
    if (is.infinite(higher_right)) {
      valley_right <- min(x[peaks[i]:length(x)], na.rm = TRUE)
    } else {
      valley_right <- min(x[peaks[i]:peaks[higher_right]], na.rm = TRUE)
    }

    result[i] <- x[peaks[i]] - max(valley_left, valley_right)
  }
  result
}
