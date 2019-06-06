
initialize_plot <- function(PLOT, TIME) {
  PLOT$t      <- seq(TIME$tmin, TIME$tmax, TIME$dt)
  PLOT$breaks <- seq(TIME$tmin, TIME$tmax, 5)
  PLOT$tlim   <- c(TIME$tmin, TIME$tmax)
  PLOT$dt     <- TIME$dt
  return(PLOT)
}

initialize_filter_params <- function(filter, t) {
  switch(filter, 
         delta = delta_params(t), 
         box_pulse = box_params(t), 
         boynton_hrf = hrf_params(t) 
  )
}

initialize_filter <- function(FILTER, PLOT, input) {
  FILTER$func   <- input$filter
  FILTER$params <- initialize_filter_params(FILTER$func, PLOT$t)
  FILTER$data   <- update_filter_data(FILTER, FILTER$params)
  return(FILTER)
}

initialize_impulse <- function(IMPULSE, input) {
  IMPULSE$data <- update_impulse(input)
  return(IMPULSE)
}

initialize_output <- function(OUTPUT, IMPULSE, FILTER, PLOT) {
  OUTPUT$data <- update_output(IMPULSE, FILTER, PLOT)
  return(OUTPUT)
}