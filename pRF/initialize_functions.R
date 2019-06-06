

initialize_stimuli <- function(STIMULI, HRF) {
  STIMULI$dt        <- STIMULI$data$t[2] - STIMULI$data$t[1]
  STIMULI$conv      <- convolve_stimuli(STIMULI, HRF)
  STIMULI$conv_data <- convolve_stimuli_data(STIMULI, HRF)
  STIMULI$freq      <- unique(STIMULI$data$y[!is.na(STIMULI$data$y)])
  STIMULI$n_freq    <- length(STIMULI$freq)
  STIMULI$freq_log  <- log10(STIMULI$freq)
  return(STIMULI)
}

initialize_gaussian <- function(GAUSSIAN, VOXEL, STIMULI) {
  voxel_gauss <- apply(VOXEL$seeds, 1, function(x) {
    gaussian(x["mu"], x["sigma"], STIMULI$freq_log) } )
  GAUSSIAN$data <- as_tibble(voxel_gauss) %>% 
    mutate(freq = STIMULI$freq) %>% 
    gather(key = "voxel", value = "y", -freq) %>% 
    mutate(voxel = factor(gsub("\\D", "", voxel)))
  GAUSSIAN$params <- VOXEL$seeds
  return(GAUSSIAN) 
}

initialize_response <- function(RESPONSE, VOXEL) {
  RESPONSE$data <- as_tibble(VOXEL$bold) %>% 
    mutate(t = VOXEL$t) %>% 
    gather(key = "voxel", value = "y", -t) %>% 
    mutate(voxel = factor(gsub("\\D", "", voxel)), 
           source = "Measured") %>% 
    group_by(voxel) %>% mutate(y = scale(y)) %>% ungroup()
  return(RESPONSE)
}

initialize_voxel <- function(VOXEL, HRF, STIMULI) {
  VOXEL$args       <- create_voxel_args(VOXEL$n+1)
  VOXEL$indx       <- logical(VOXEL$n^2)
  VOXEL$t          <- HRF$data$t
  VOXEL$fitted     <- logical(VOXEL$n^2)
  VOXEL$fit_params <- tibble(mu = numeric(VOXEL$n^2), sigma = numeric(VOXEL$n^2))
  VOXEL$fit_corr   <- numeric(VOXEL$n^2) * NA
  VOXEL$model      <- matrix(numeric(VOXEL$n^2 * length(unique(STIMULI$freq))), 
                             ncol = VOXEL$n^2)
  VOXEL$seeds      <- generate_seeds(STIMULI, VOXEL$n^2)
  return(VOXEL)
}
