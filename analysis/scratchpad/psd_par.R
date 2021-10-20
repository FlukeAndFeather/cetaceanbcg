library(cetaceanbcg)
library(doParallel)
library(tidyverse)

n <- as.integer(commandArgs(trailingOnly = TRUE))

# use the environment variable SLURM_NTASKS_PER_NODE to set
# the number of cores to use
if (Sys.getenv("SLURM_NTASKS_PER_NODE") == "") {
  ncores <- 1
} else {
  ncores <- Sys.getenv("SLURM_NTASKS_PER_NODE")
}
registerDoParallel(cores = ncores)

bw_400hz <- readRDS("analysis/data/derived_data/bw_400hz.rds")

# Signal band within these bounds
bw_bpm <- c(4, 8) / 60

bcg_psd <- function(low, width, bw400hz,
                    fs = 400, window = 1.8, bpm = c(4, 8), max_bpm = 64) {
  tryCatch({
    pass <- c(low, low + width)
    fny <- fs / 2
    bandpass_filter <- function(x) {
      signal::butter(4, pass / fny, type = "pass") %>%
        signal::filtfilt(x)
    }
    acc <- as.matrix(bw400hz[, c("surge", "heave", "sway")])
    acc_filt <- apply(acc, 2, bandpass_filter)

    if (sum(is.finite(acc_filt)) == 0) {
      return(tibble())
    }

    surge_filt <- acc[, 1]
    lsr <- local_range(surge_filt, window * fs)
    j = njerk(acc_filt, p = 4, n = window * fs)
    njerk_smooth = tma(j, window * fs)

    # PSD of surge, LSR, and jerk
    psds <- list(surge_filt, lsr, njerk_smooth) %>%
      map(~ ts(.x[!is.na(bw400hz$region_id)], frequency = 400)) %>%
      map(~ suppressMessages(psd::pspectrum(.x)))

    freq <- psds[[1]]$freq

    tibble(freq,
           surge = psds[[1]]$spec,
           lsr = psds[[2]]$spec,
           jerk = psds[[3]]$spec) %>%
      pivot_longer(surge:jerk,
                   names_to = "norm_fun",
                   values_to = "spec") %>%
      filter(freq * 60 <= max_bpm)
  }, error = function(e) {
    cat(sprintf("low = %0.3f; width = %0.3f; error = \"%s\"",
                low, width, as.character(e)),
        file = "log.txt",
        sep = "\n",
        append = TRUE)
    tibble()
  })
}

# Latin hypercube sampling
low <- 2^seq(-2, 2, length.out = n)
width <- 2^seq(1, 5, length.out = n)
set.seed(2034)
bcg_lhs <- tibble(
  low = sample(low, length(low), replace = FALSE),
  width = sample(width, length(width), replace = FALSE)
)

bcg_psd_results <- foreach(l = bcg_lhs$low, w = bcg_lhs$width,
                           .combine = bind_rows) %dopar% {
  psd_pars <- tibble(low = l, width = w)
  psd_result <- bcg_psd(l, w, bw_400hz)
  if (nrow(psd_result) == 0) {
    psd_pars
  } else {
    cbind(psd_pars, psd_result)
  }
}

saveRDS(bcg_psd_results, "analysis/data/derived_data/bcg_psd_results.rds")

bcg_psd_results %>%
  count(low, width)
