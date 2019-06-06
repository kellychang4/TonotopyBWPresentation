

boynton_hrf <- function(tau, delta, n, t) {
  t <- t - delta
  y = (t/tau) ^ (n-1) * exp(-t/tau) / (tau*factorial(n-1))
  y[t < 0] <- 0
  return(y)
}

plot_hrf <- function(HRF) {
  HRF$data %>% 
    ggplot(aes(x = t, y = y)) +
    scale_x_continuous(name = "Time (s)") +
    scale_y_continuous(name = "HRF\nResponse") + 
    geom_line(color = "black") + 
    theme_classic(base_size = 20)
}
