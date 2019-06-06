
delta <- function(t) {
  y <- numeric(length(t))
  y[1] <- 1
  return(y)
}

delta_params <- function(t) {
  list(t = t)
}

box_pulse <- function(n, t) {
  y <- numeric(length(t))
  n_indx <- ceiling(1 / (t[2] - t[1]) * n)
  y[1:n_indx] <- 1
  return(y)
}

box_params <- function(t) {
  list(n = 2, t = t)
}

boynton_hrf <- function(tau, delta, n, t) {
  t <- t - delta
  y = (t/tau) ^ (n-1) * exp(-t/tau) / (tau*factorial(n-1))
  y[t < 0] <- 0
  y <- y / max(y)
  return(y)
}

hrf_params <- function(t) {
  list(tau = 1.5, delta = 1.8, n = 3, t = t)
}

# updates -----------------------------------------------------------------

update_filter_data <- function(FILTER, params) {
  tibble(t = params$t, y = do.call(FILTER$func, params))
}

update_filter <- function(FILTER, PLOT, input) {
  FILTER$data <- update_filter_data(FILTER, FILTER$params)
  return(FILTER)
}

# plot --------------------------------------------------------------------

plot_filter <- function(FILTER, PLOT) {
  FILTER$data %>% 
    ggplot(aes(x = t, y = y)) +
    scale_x_continuous(name = "Time (s)", 
                       limits = PLOT$tlim, breaks = PLOT$breaks) +
    scale_y_continuous(name = "Filter Gain") +
    geom_line(size = 2) +
    theme_classic(base_size = 16)
}

