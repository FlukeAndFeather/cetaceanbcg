#' Estimate power spectral density
#'
#' @param x `[numeric]` signal
#' @param fs_hz `[numeric(1)]` sampling rate (Hz)
#'
#' @return `[spec]` see `psd::pspectrum`
#' @export
pspectrum <- function(x, fs_hz) {
  suppressMessages(psd::pspectrum(ts(x, frequency = fs_hz)))
}

#' Calculate signal-to-noise ratio from power spectral density
#'
#' @param psd `[spec]` power spectral density
#' @param signal_bpm `[numeric(2)]` signal band (bpm)
#'
#' @return
#' @export
s2n <- function(psd, signal_bpm) {
  signal_hz <- signal_bpm / 60
  is_signal <- between(psd$freq, signal_hz[1], signal_hz[2])
  is_noise <- !is_signal

  psignal <- pracma::trapz(psd$freq[is_signal], psd$spec[is_signal])
  pnoise <- pracma::trapz(psd$freq[is_noise], psd$spec[is_noise])

  psignal / pnoise
}
