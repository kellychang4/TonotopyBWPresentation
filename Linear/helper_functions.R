
app_directory <- file.path(".", "Linear")

helper_files <- c("initialize_functions.R", "output_functions.R", 
                  "impulse_functions.R", "filter_functions.R")

lapply(helper_files, function(x) source(file.path(app_directory,x)))
