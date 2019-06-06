
rm(list = ls())
library(shiny)
library(tidyverse)
library(ggplot2)

app_directory <- file.path(".", "Linear")
source(file.path(app_directory, "helper_functions.R"))

# define ui.R
ui <- fluidPage(
  tags$head(
    tags$style("#impulse  { margin-left: 16px; }"), 
    tags$style("#filter   { margin-left: 9px; }"), 
    tags$style("#output { margin: auto; }"), 
    tags$style(".well { width: 400px; }")
  ),
  
  verticalLayout(
    
    sidebarLayout(
      mainPanel(plotOutput("impulse", width = "600px", height = "200px"), width = 7), 
      sidebarPanel(
        verticalLayout(
          fluidRow(column(6, numericInput("stim_t1", label = "Stimulus 1 Onset:", 
                                          value = 0, min = 0, max = 30, width = "135px")),  
                   column(6, numericInput("stim_y1", label = "Stimulus 1 Intensity:", 
                                          value = 1, min = 0, max = Inf, width = "135px"))), 
          fluidRow(column(6, numericInput("stim_t2", label = "Stimulus 2 Timing:", 
                                          value = NULL, min = 0, max = 30, width = "135px")),  
                   column(6, numericInput("stim_y2", label = "Stimulus 2 Intensity:", 
                                          value = NULL, min = 0, max = Inf, width = "135px")))
        ), width = 5
      ) 
    ), # first sidebar 
    
    sidebarLayout(
      mainPanel(plotOutput("filter", width = "600px", height = "200px"), width = 7), 
      sidebarPanel(width = 5, radioButtons("filter", label = "Filter Function", selected = "delta",
                                           choiceNames = c("Delta Function", "Box Function", "Hemodynamic Response Function"), 
                                           choiceValues = c("delta", "box_pulse", "boynton_hrf"), 
                                           width = "100%")
      )
    ), # second sidebar
    
    sidebarLayout(
      mainPanel(plotOutput("output", width = "600px", height = "200px"), width = 7), 
      sidebarPanel(width = 5, 
                   checkboxGroupInput("output", label = "Response Source(s):", selected = "1",
                                      choiceNames = c("Stimulus 1", "Stimulus 2", "Both"), 
                                      choiceValues = c("1", "2", "Both"))
      )
    ) # third sidebar
  )
)

# server.r
server <- function(input, output, session) {
  TIME    <- reactiveValues(tmin = 0, tmax = 30, dt = 0.01)
  PLOT    <- reactiveValues(t = NULL, breaks = NULL, tlim = NULL, dt = NULL, 
                            color = c("Both" = "black", "1" = "purple", "2" = "darkgoldenrod1"),
                            lty = c("Both" = "solid", "1" = "dotted", "2" = "dotted"))
  IMPULSE <- reactiveValues(data = NULL)
  FILTER  <- reactiveValues(data = NULL, func = NULL, params = NULL)
  OUTPUT  <- reactiveValues(data = NULL)
  
  # initialize variables
  observe({
    PLOT     <- initialize_plot(PLOT, TIME)
    IMPULSE  <- initialize_impulse(IMPULSE, input)
    FILTER   <- initialize_filter(FILTER, PLOT, input)
    OUTPUT   <- initialize_output(OUTPUT, IMPULSE, FILTER, PLOT)
  }, priority = Inf)
  
  output$impulse <- renderPlot({
    plot_impulse(IMPULSE, PLOT)
  }, width = 600, height = 200)
  
  output$filter <- renderPlot({
    plot_filter(FILTER, PLOT)
  }, width = 490, height = 200)
  
  output$output <- renderPlot({
    plot_output(OUTPUT, PLOT, input)
  }, width = 600, height = 200)
  
}

# run the application 
shinyApp(ui = ui, server = server)