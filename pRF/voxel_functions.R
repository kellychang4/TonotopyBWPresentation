
library(sommer)
freq_map <- seq(log10(88), log10(8000), length.out = 101)
color_map <- rev(jet.colors(101))

color_voxel <- function(args) {
  lapply(args, function(x) do.call(rect, x))
}

outline_voxel <- function(n) {
  x0 <- rep(1, n)
  x1 <- rep(n, n)
  segments(x0, 1:n, x1, 1:n, lwd = 2) # horizontal
  segments(1:n, x0, 1:n, x1, lwd = 2) # vertical
}

create_voxel_args <- function(n) {
  xy <- expand.grid(x = 1:n, y = 1:n)
  x <- matrix(xy$x, nrow = n)
  y <- matrix(xy$y, nrow = n)
  xLeft   <- x[2:n, 1:(n-1)]
  yBottom <- y[2:n, 1:(n-1)]
  xRight  <- x[1:(n-1), 2:n]
  yTop    <- y[1:(n-1), 2:n]
  voxel_args <- mapply(function(x,y,z,a) 
  { list(list(xleft = x, ybottom = y, xright = z, ytop = a, col = NA, lwd = 0)) }, 
  xLeft, yBottom, xRight, yTop)
  return(voxel_args)
}

# update ------------------------------------------------------------------

generate_seeds <- function(STIMULI, n) {
  freq_range <- STIMULI$freq_log # log Hz
  seeds <- tibble(mu = runif(n, min = min(freq_range), max = max(freq_range)), 
                  sigma = runif(n, min = 0.01, max = 2))
}

update_voxel_indx <- function(VOXEL, input) {
  x_indx <- max(which(sapply(1:(VOXEL$n+1), function(x) x <= input$x)))
  y_indx <- max(which(sapply(1:(VOXEL$n+1), function(x) x <= input$y)))
  VOXEL$current <- VOXEL$n * (min(max(y_indx,1),VOXEL$n)-1) + min(max(x_indx, 1), VOXEL$n)
  VOXEL$indx[VOXEL$current] <- TRUE
  return(VOXEL)
}

update_voxel_args <- function(VOXEL) {
  fit_args <- VOXEL$args[[VOXEL$current]]
  fit_mu <- VOXEL$fit_params[VOXEL$current,]$mu
  fit_args$col <- color_map[which.min((freq_map - fit_mu)^2)]
  return(fit_args)
}

# fit ---------------------------------------------------------------------

save_fit_results <- function(VOXEL, STIMULI, fit_results) {
  current <- VOXEL$current 
  VOXEL$fitted[current] <- TRUE
  VOXEL$fit_params[current,] <- fit_results$par
  VOXEL$fit_corr[current] <- -fit_results$value
  VOXEL$model[,current] <- gaussian(fit_results$par["mu"], 
                                    fit_results$par["sigma"], 
                                    STIMULI$freq_log)
  VOXEL$args[[current]] <- update_voxel_args(VOXEL)
  return(VOXEL)
}

fit_voxel <- function(VOXEL, STIMULI) {
  freq <- STIMULI$freq_log # function of frequency
  par <- VOXEL$seeds[VOXEL$current,] # initialize seeds
  fit_results <- optim(par, error_fn, conv_mat = STIMULI$conv, 
                       freq = freq, actual = VOXEL$bold[,VOXEL$current],
                       lower = c(min(freq), 0.01), upper = c(max(freq), 2),
                       method = "L-BFGS-B", control = list(maxit = 1e4))
  VOXEL <- save_fit_results(VOXEL, STIMULI, fit_results)
  return(VOXEL)
}

error_fn <- function(par, conv_mat, freq, actual) {
  model <- gaussian(par["mu"], par["sigma"], freq)
  predicted <- conv_mat %*% model
  error <- -cor(actual, predicted)
}

# plot --------------------------------------------------------------------

plot_voxel <- function(VOXEL) {
  plot(1, NA, asp = 1, xlim = c(0.99, VOXEL$n+1.01), 
       ylim = c(0.99, VOXEL$n+1.01), axes = FALSE, xlab = "", ylab = "")
  if (any(VOXEL$indx & VOXEL$fitted)) 
  { color_voxel(VOXEL$args[VOXEL$indx & VOXEL$fitted]) }
  outline_voxel(VOXEL$n+1)
  if (!is.null(VOXEL$current)) { 
    if (!VOXEL$fitted[VOXEL$current]) {
      current_arg <- VOXEL$args[[VOXEL$current]]
      current_arg$lwd <- 5
      current_arg$col <- NA
      current_arg$border <- "red"
      do.call(rect, current_arg) 
    } 
  }  
}
