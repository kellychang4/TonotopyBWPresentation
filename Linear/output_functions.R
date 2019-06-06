
library(tidyverse)

create_stim <- function(data, t) {
  t_indx <- which.min( (t-data$t)^2 )
  y = numeric(length(t))
  y[t_indx] = data$intensity
  return(y)
}

create_stims <- function(data, t) {
  IMPULSE <- split(data, data$id)
  sapply(IMPULSE, function(x) create_stim(x, t))
}

fft_conv <- function(x, y, dt) {
  Re(fft(fft(x) * fft(y), inverse = TRUE)) * dt
}

create_convolve <- function(IMPULSE, FILTER, PLOT) {
  x_stim <- create_stims(IMPULSE$data, PLOT$t)
  OUTPUT <- apply(x_stim, 2, function(x)  fft_conv(x, FILTER$data$y, PLOT$dt))
}

# update ------------------------------------------------------------------

update_output <- function(IMPULSE, FILTER, PLOT) {
  response <- as_tibble(create_convolve(IMPULSE, FILTER, PLOT))
  data <- response %>% 
    mutate(Both = `1` + `2`, t = PLOT$t) %>% 
    gather(key = "id", value = "y", -t) %>% 
    mutate(lty = ifelse(id == "Both", "solid", "dotted"), 
           legn = factor(paste0(id, ".", lty), 
                         levels = c("Both.solid", "1.dotted", "2.dotted"), 
                         labels = c("Both", "1", "2")))
}

# plot --------------------------------------------------------------------

plot_output <- function(OUTPUT, PLOT, input) {
  output_data <- OUTPUT$data %>% 
    filter(id %in% input$output) %>% 
    mutate(legn = droplevels(legn))
  output_data %>% 
    ggplot(aes(x = t, y = y, color = legn, linetype = legn)) +
    scale_x_continuous(name = "Time (s)", 
                       limits = PLOT$tlim, breaks = PLOT$breaks) +
    scale_y_continuous(name = "Response Output") + 
    scale_color_manual(name = "Stimulus ID", 
                       labels = levels(output_data$legn),  
                       values = PLOT$color) +
    scale_linetype_manual(name = "Stimulus ID", 
                          labels = levels(output_data$legn), 
                          values = PLOT$lty) +
    geom_line(size = 2) + 
    theme_classic(base_size = 16) + 
    theme(legend.key.width = unit(0.05, "npc"))
}
