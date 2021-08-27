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
