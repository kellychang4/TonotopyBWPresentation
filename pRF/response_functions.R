
# update ------------------------------------------------------------------

update_response <- function(RESPONSE, STIMULI, GAUSSIAN, VOXEL) {
  model <- GAUSSIAN$data %>% filter(voxel == VOXEL$current) %>% arrange(freq)
  model_data <- tibble(t = VOXEL$t, y = scale(STIMULI$conv %*% model$y), 
                       voxel = factor(VOXEL$current), source = factor("Model")) 
  RESPONSE$data <- RESPONSE$data %>% 
    filter(source == "Measured") %>% 
    bind_rows(model_data) %>% 
    mutate(voxel = factor(voxel), 
           source = factor(source))
  return(RESPONSE)
}

# plot --------------------------------------------------------------------

plot_response <- function(RESPONSE, VOXEL) {
  RESPONSE$data %>%
    filter(voxel == ifelse(is.null(VOXEL$current), NA, VOXEL$current)) %>%
    ggplot(aes(x = t, y = y, color = source)) +
    scale_x_continuous(name = "Time (s)") +
    scale_y_continuous(name = "Normalized\nResponse") +
    scale_color_manual(name = "Source", values = c("black", "red")) +
    scale_shape_manual(name = "Source", values = c(16, 8)) + 
    geom_point() + geom_line() + 
    theme_classic(base_size = 20) + 
    theme(legend.position = "bottom")
}
