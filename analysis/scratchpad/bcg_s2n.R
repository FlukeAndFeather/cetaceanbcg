library(cetaceanbcg)
library(tidyverse)

bcg_psd_results <- readRDS("analysis/data/derived_data/bcg_psd_results.rds")
n <- as.integer(readLines("analysis/data/derived_data/psd_par_n.txt"))

# "Signal" band (typical blue whale diving heartrates)
bw_bpm <- c(4, 8) / 60

# Calculate signal-to-noise ratio by integrating the PSD curve
calc_s2n <- function(freq, spec) {
  is_signal <- between(freq, bw_bpm[1], bw_bpm[2])
  is_noise <- !is_signal

  psignal = pracma::trapz(freq[is_signal], spec[is_signal])
  pnoise = pracma::trapz(freq[is_noise], spec[is_noise])

  psignal / pnoise
}

# Ranked signal-to-noise result by bandpass parameters
bcg_s2n <- bcg_psd_results %>%
  drop_na(norm_fun) %>%
  group_by(low, width, norm_fun) %>%
  summarize(s2n = calc_s2n(freq, spec), .groups = "drop") %>%
  arrange(-s2n)

# Parameter space, covered by sampling. Use interpolation to fill in gaps.
low <- 2^seq(-2, 2, length.out = n)
width <- 2^seq(1, 5, length.out = n)
interp_s2n <- akima::interp(
  x = log2(bcg_s2n$low[bcg_s2n$norm_fun == "jerk"]),
  y = log2(bcg_s2n$width[bcg_s2n$norm_fun == "jerk"]),
  z = bcg_s2n$s2n[bcg_s2n$norm_fun == "jerk"],
  xo = log2(low),
  yo = log2(width),
  linear = FALSE,
  extrap = FALSE
)
bcg_s2n_interp <- expand_grid(low, width) %>%
  mutate(i = match(log2(low), interp_s2n$x),
         j = match(log2(width), interp_s2n$y),
         s2n = interp_s2n$z[cbind(i, j)]) %>%
  select(-i, -j)

interp_s2n <- akima::interp(
  x = bcg_s2n$low[bcg_s2n$norm_fun == "jerk"],
  y = bcg_s2n$width[bcg_s2n$norm_fun == "jerk"],
  z = bcg_s2n$s2n[bcg_s2n$norm_fun == "jerk"],
  xo = low,
  yo = width,
  linear = FALSE,
  extrap = FALSE
)
bcg_s2n_interp <- expand_grid(low, width) %>%
  mutate(i = match(low, interp_s2n$x),
         j = match(width, interp_s2n$y),
         s2n = interp_s2n$z[cbind(i, j)]) %>%
  select(-i, -j)

# Plot signal-to-noise surface over parameter space
(s2n_plot <- bcg_s2n_interp %>%
  mutate(s2n_p = percent_rank(s2n)) %>%
  ggplot(aes(low, width, fill = s2n_p)) +
  geom_tile() +
  # scale_x_continuous("Low (Hz)", trans = "log2") +
  # scale_y_continuous("Width (Hz)", trans = "log2") +
  scale_fill_viridis_c() +
  theme_classic())

# Plot the PSD function for one parameter set
plot_bcg_psd <- function(low, width, dat) {
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
         caption = sprintf("Passband = [%0.2f, %0.2f]", low, low + width)) +
    theme_classic()
}

# Iterate over the BCG PSD results to generate plots
psd_plots <- bcg_psd_results %>%
  nest(dat = -c(low, width)) %>%
  mutate(plot = pmap(., plot_bcg_psd)) %>%
  select(-dat)

nearest_psd_plot <- function(new_low, new_width, psd_plots) {
  low_rank <- rank(c(new_low, psd_plots$low))
  width_rank <- rank(c(new_width, psd_plots$width))
  rank_dist <- sqrt((low_rank[1] - low_rank[-1])^2 + (width_rank[1] - width_rank[-1])^2)
  nearest <- which.min(rank_dist)
  psd_plots$plot[[nearest]]
}
