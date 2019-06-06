

app_directory <- file.path(".", "pRF")

helper_files <- c("initialize_functions.R", "response_functions.R", 
                  "gaussian_functions.R", "stimuli_functions.R", 
                  "voxel_functions.R", "hrf_functions.R")

lapply(helper_files, function(x) source(file.path(app_directory,x)))
