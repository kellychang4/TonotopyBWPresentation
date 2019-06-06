
rm(list = ls())
library(shiny)
library(sommer)
library(ggplot2)
library(tidyverse)

data_files <- c("pRF.RData", "hrf.RData", "stimuli.RData")
app_directory <- file.path(".", "pRF")
source(file.path(app_directory, "helper_functions.R"))
lapply(data_files, function(x) load(file.path(app_directory,x),.GlobalEnv))

# define ui.R
ui <- fluidPage(
  tags$head(
    tags$style("#stimuli    { margin-left: -16px; margin-top: 50px; }"), 
    tags$style("#hrf        { margin-left: -23px; margin-top: 25px; }"), 
    tags$style("#convolve   { margin-left: 180px; margin-top: 25px; }"), 
    tags$style("#convolved  { margin-left: -18px; margin-top: 60px; }"),
    tags$style("#gaussian   { margin-left: 50px;  margin-top: 60px; }"),
    tags$style("#fit_voxel  { margin-left: 123px; margin-top: 25px; }"),
    tags$style("#response   { margin-left: 60px;  margin-top: 52px; }"),
    tags$style("#voxel      { margin-left: 187px; margin-top: 0px; }")
  ),
  
  fluidRow(
    column(4, 
           conditionalPanel(
             condition = "input.convolve == 0", 
             verticalLayout(
               plotOutput("stimuli", width = "450px", height = "250px"), 
               plotOutput("hrf", width = "450px", height = "150px"), 
               actionButton("convolve", label = "Convolve Stimuli")
             )
           ), 
           conditionalPanel(
             condition = "input.convolve != 0",
             plotOutput("convolved", width = "450px", height = "450px")
           )
    ), # end of column 1
    
    column(2, verticalLayout(
      plotOutput("gaussian", width = "200px", height = "450px"),
      actionButton("fit_voxel", HTML("Fit pRF Model<br>to Current Voxel"))
    )), 
    
    column(6, 
           verticalLayout(
             plotOutput("response", width = "500px", height = "250px"), 
             plotOutput("voxel", click = "click_voxel", 
                        width = "300px", height = "300px")
           )
    ) # end of column 3    
  )
)

# server.r
server <- function(input, output, session) {
  STIMULI  <- reactiveValues(data = stimuli, stim_mat = stim_mat, dt = NULL, 
                             conv = NULL, conv_data = NULL, freq = NULL, 
                             n_freq = NULL, freq_log = NULL)
  HRF      <- reactiveValues(data = tibble(t = t_hrf, y = hrf))
  GAUSSIAN <- reactiveValues(data = NULL, params = NULL)
  RESPONSE <- reactiveValues(data = NULL)
  VOXEL    <- reactiveValues(bold = actual_bold, n = 3, t = NULL, args = NULL, 
                             color = NULL, indx = NULL, current = NULL, 
                             fitted = NULL, fit_params = NULL, fit_corr = NULL,
                             seeds = NULL, model = NULL)
  
  # intitialize variables
  initialize_variables <- reactive({
    STIMULI  <- initialize_stimuli(STIMULI, HRF)
    VOXEL    <- initialize_voxel(VOXEL, HRF, STIMULI)
    GAUSSIAN <- initialize_gaussian(GAUSSIAN, VOXEL, STIMULI)
    RESPONSE <- initialize_response(RESPONSE, VOXEL)
  })
  
  # call initialize variables once, and then destroy
  observeEvent(TRUE, {
    initialize_variables()
  }, once = TRUE)
  
  # voxel map click
  observeEvent(input$click_voxel, {
    VOXEL <- update_voxel_indx(VOXEL, input$click_voxel)
    RESPONSE <- update_response(RESPONSE, STIMULI, GAUSSIAN, VOXEL)
  })
  
  # observe fit model input
  observeEvent(input$fit_voxel, {
    if (input$convolve != 0) { # if conolved
      if (!is.null(VOXEL$current)) { if (!VOXEL$fitted[VOXEL$current]) {
        VOXEL <- fit_voxel(VOXEL, STIMULI)
        GAUSSIAN <- update_gaussian(GAUSSIAN, VOXEL, STIMULI)
        RESPONSE <- update_response(RESPONSE, STIMULI, GAUSSIAN, VOXEL) 
    }}}
  })
  
  output$stimuli <- renderPlot({
    plot_stimuli(STIMULI)
  }, width = 450, height = 250)
  
  output$hrf <- renderPlot({
    plot_hrf(HRF)
  }, width = 450, height = 150)
  
  output$convolved <- renderPlot({
    plot_convolved(STIMULI)
  }, width = 450, height = 450)
  
  output$gaussian <- renderPlot({
    plot_gaussian(GAUSSIAN, VOXEL)
  }, width = 200, height = 450)
  
  output$response <- renderPlot({
    plot_response(RESPONSE, VOXEL)
  }, width = 500, height = 250)
  
  output$voxel <- renderPlot({
    plot_voxel(VOXEL)
  }, width = 300, height = 300)
}

# run the application 
shinyApp(ui = ui, server = server)
