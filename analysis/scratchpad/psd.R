

# Signal band within these bounds
bw_bpm <- c(4, 8) / 60

bcg_psd <- function(low, width, bw400hz,
                    fs = 400, window = 1.8, bpm = c(4, 8)) {
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
                 values_to = "spec")
}

# Latin hypercube sampling
low <- 2^seq(-2, 2, length.out = 256)
width <- 2^seq(1, 5, length.out = 256)
bcg_lhs <- tibble(
  low = sample(low, length(low), replace = FALSE),
  width = sample(width, length(width), replace = FALSE)
)
pb <- progress::progress_bar$new(total = nrow(bcg_lhs))
bcg_psd_results <- bcg_lhs %>%
  group_by_all() %>%
  group_modify(function(x, y) {
    pb$tick()
    bcg_psd(y$low, y$width, bw_400hz)
  }) %>%
  ungroup()

calc_s2n <- function(freq, spec) {
  is_signal <- between(freq, bw_bpm[1], bw_bpm[2])
  is_noise <- !is_signal

  psignal = pracma::trapz(freq[is_signal], spec[is_signal])
  pnoise = pracma::trapz(freq[is_noise], spec[is_noise])

  psignal / pnoise
}

plot_bcg_psd <- function(dat, key, outdir) {
  is_signal <- between(dat$freq, bw_bpm[1], bw_bpm[2])
  is_noise <- !is_signal

  signal_noise <- dat %>%
    group_by(norm_fun) %>%
    summarize(s2n = calc_s2n(freq, spec),
              freq = max(freq),
              spec = max(spec))

  ggplot(dat, aes(freq * 60, spec)) +
    geom_ribbon(aes(ymin = -Inf, ymax = spec),
                filter(dat, is_signal),
                fill = "grey50") +
    geom_line() +
    geom_text(aes(label = sprintf("Signal:noise = %0.3f", s2n)),
              signal_noise,
              hjust = 1.1, vjust = 1.1) +
    scale_x_continuous(trans = "pseudo_log",
                       breaks = 2^seq(0, 15, by = 3)) +
    facet_grid(rows = vars(norm_fun), scales = "free_y") +
    labs(x = "Frequency (min^-1)",
         caption = sprintf("Passband = [%0.2f, %0.2f]",
                           key$low,
                           key$low + key$width)) +
    theme_classic()
}

psd_plots <- bcg_psd_results %>%
  group_by(low, width) %>%
  group_map(plot_bcg_psd)
psd_plots[[2]]

bcg_s2n <- bcg_psd_results %>%
  group_by(low, width, norm_fun) %>%
  summarize(s2n = calc_s2n(freq, spec), .groups = "drop") %>%
  arrange(-s2n)

bcg_s2n_interp <- expand_grid(low, width) %>%
  mutate(s2n = akima::interp(
    x = bcg_s2n$low[bcg_s2n$norm_fun == "jerk"],
    y = bcg_s2n$width[bcg_s2n$norm_fun == "jerk"],
    z = bcg_s2n$s2n[bcg_s2n$norm_fun == "jerk"],
    xo = !!low,
    yo = !!width,
    linear = TRUE,
    extrap = FALSE
  )$z %>% t() %>% as.vector()
)
bcg_s2n_interp %>%
  ggplot(aes(low, width, fill = s2n)) +
  geom_tile() +
  scale_x_continuous("Low (Hz)", trans = "log2") +
  scale_y_continuous("Width (Hz)", trans = "log2") +
  scale_fill_viridis_c() +
  theme_classic()
