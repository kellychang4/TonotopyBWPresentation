
library(sommer)
color_map <- rev(jet.colors(101))

freq_color <- function(freq) {
  freq_range <- seq(log10(88), log10(8000), length.out = 101)
  color <- color_map[which.min((freq - freq_range)^2)]
}

# model -------------------------------------------------------------------

gaussian <- function(mu, sigma, x) {
  y = exp( -((x-mu)^2) / (2*sigma^2) )
  y = y / max(y)
}

# update ------------------------------------------------------------------

update_gaussian <- function(GAUSSIAN, VOXEL, STIMULI) {
  GAUSSIAN$params[VOXEL$fitted,] <- VOXEL$fit_params[VOXEL$fitted,]
  fitted_data <- as_tibble(VOXEL$model[, VOXEL$fitted])
  names(fitted_data) <- as.character(which(VOXEL$fitted))
  fitted_data <- fitted_data %>% 
    mutate(freq = STIMULI$freq) %>% 
    gather(key = "voxel", value = "y", -freq) %>% 
    mutate(voxel = factor(gsub("\\D", "", voxel)))
  GAUSSIAN$data <- GAUSSIAN$data %>% 
    filter(voxel %in% which(!VOXEL$fitted)) %>% 
    bind_rows(fitted_data) 
  return(GAUSSIAN)
}

# plot --------------------------------------------------------------------

calculate_param_data <- function(GAUSSIAN, VOXEL) {
  freq_range <- range(GAUSSIAN$data$freq)
  current <- ifelse(is.null(VOXEL$current), NA, VOXEL$current)
  mu <- unique(GAUSSIAN$params$mu[current])
  sigma <- unique(GAUSSIAN$params$sigma[current])
  data <- tibble(x = 10^(mu - sigma), y = 0.5, xend = 10^(mu + sigma), 
                 yend = 0.5, color = freq_color(mu), mu = 10^(mu)) %>% na.omit() %>% 
    mutate(x = max(x, min(freq_range)), xend = min(xend, max(freq_range)))
  return(data)
}

plot_gaussian <- function(GAUSSIAN, VOXEL) {
  freq_range <- unique(GAUSSIAN$data$freq)
  param_data <- calculate_param_data(GAUSSIAN, VOXEL)
  GAUSSIAN$data %>%
    filter(voxel == ifelse(is.null(VOXEL$current), NA, VOXEL$current)) %>%
    ggplot(aes(x = freq, y = y)) +
    scale_x_continuous(name = "Frequency (Hz)", trans = "log10", 
                       limits = range(freq_range)) +
    scale_y_continuous(name = "Selectivity", breaks = c(0, 0.5, 1)) +
    geom_density(color = NA, fill = "grey90", stat = "identity") +
    geom_vline(aes(xintercept = param_data$mu), size = 2, 
               color = param_data$color) +
    geom_segment(data = param_data, color = param_data$color, size = 2,
                 mapping = aes(x =x, y = y, xend = xend, yend = yend),
                 arrow = arrow(ends = "both", type = "closed",
                               length = unit(0.03, "npc"))) +
    theme_classic(base_size = 20) +
    coord_flip()
}
