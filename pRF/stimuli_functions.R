
library(sommer)

fft_conv <- function(x, y, dt) {
  Re(fft(fft(x) * fft(y), inverse = TRUE)) * dt
}

convolve_stimuli <- function(STIMULI, HRF) {
  apply(STIMULI$stim_mat, 1, function(x) fft_conv(x, HRF$data$y, STIMULI$dt))
}

convolve_stimuli_data <- function(STIMULI, HRF) {
  freq <- unique(STIMULI$data$y[!is.na(STIMULI$data$y)])
  data <- tibble(t = rep(HRF$data$t, length(freq)), 
                 y = as.vector(STIMULI$conv), 
                 f = rep(freq, each = length(HRF$data$t)))
}

# plot --------------------------------------------------------------------

plot_stimuli <- function(STIMULI) {
  STIMULI$data %>%
    ggplot(aes(x = t, y = y, color = factor(y))) +
    scale_x_continuous(name = "Time (s)") +
    scale_y_continuous(name = "Frequency (Hz)", 
                       trans = "log10") +
    scale_color_manual(values = rev(jet.colors(STIMULI$n_freq)))+ 
    geom_rect(data = STIMULI$data %>% filter(is.na(y)), 
              mapping = aes(xmin = t, xmax = t+1, ymin = 0, ymax = Inf), 
              color = NA, fill = "gray95") +
    geom_point(shape = 15, size = 5) +
    theme_classic(base_size = 20) +
    theme(legend.position = "none")
}

plot_convolved <- function(STIMULI) {
  STIMULI$conv_data %>% 
    ggplot(aes(x = t, y = f, color = factor(f), alpha = y)) + 
    geom_point(shape = 15, size = 5) + 
    scale_x_continuous(name = "Time (s)") +
    scale_y_continuous(name = "Frequency (Hz)", trans = "log10") +
    scale_color_manual(values = rev(jet.colors(STIMULI$n_freq))) + 
    theme_classic(base_size = 20) + 
    theme(legend.position = "none")
}

