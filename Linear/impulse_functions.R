
# update ------------------------------------------------------------------

update_impulse <- function(input) {
  tibble(id = factor(1:2), t = c(input$stim_t1, input$stim_t2), 
         intensity = c(input$stim_y1, input$stim_y2))
}

# plot --------------------------------------------------------------------

plot_impulse <- function(IMPULSE, PLOT) {
  IMPULSE$data %>% 
    ggplot(aes(x = t, xend = t, y = 0, yend = intensity, color = id)) +
    scale_x_continuous(name = "Time (s)", 
                       limits = PLOT$tlim, breaks = PLOT$breaks) + 
    scale_y_continuous(name = "Stimulus Intensity") +
    scale_color_manual(name = "Stimulus ID", 
                       values = PLOT$color) + 
    geom_segment(size = 2) +
    geom_point(aes(y = intensity), size = 4) +
    theme_classic(base_size = 16)  + 
    theme(legend.key.width = unit(0.05, "npc"))
}