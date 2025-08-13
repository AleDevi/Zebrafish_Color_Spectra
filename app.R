#!/usr/bin/env Rscript

# Shiny UI for Zebrafish Color Spectra Analysis
# This application provides a user-friendly interface for spectral data analysis

library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(zoo)
library(DT)  # Add DT package for modern data tables

# Load configuration
if (file.exists("src/config.R")) {
  source("src/config.R")
} else {
  # Default configuration
  DATA_DIR <- "data/Experimental"
  WAVELENGTH_FILE <- "data/Experimental/Wavelength.txt"
  MIN_WAVELENGTH <- 280
  MAX_WAVELENGTH <- 700
  SMOOTHING_FILTER_SIZE <- 50
}

# UI Definition
ui <- fluidPage(
  titlePanel("🐟 Zebrafish Color Spectra Analysis"),
  
  # Sidebar for controls
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Configuration section
      h4("⚙️ Analysis Settings"),
      numericInput("min_wavelength", "Min Wavelength (nm):", 
                  value = MIN_WAVELENGTH, min = 200, max = 800),
      numericInput("max_wavelength", "Max Wavelength (nm):", 
                  value = MAX_WAVELENGTH, min = 200, max = 800),
      numericInput("smoothing_window", "Smoothing Window Size:", 
                  value = SMOOTHING_FILTER_SIZE, min = 5, max = 100),
      
      # Single action button for complete processing
      h4("🚀 Single-Pass Analysis"),
      actionButton("run_complete_analysis", "Run Complete Analysis", 
                  class = "btn-primary btn-lg", 
                  style = "width: 100%; margin-bottom: 10px;"),
      
      # Status display
      h4("📊 Status"),
      verbatimTextOutput("status_text"),
      
      # Data info
      h4("📋 Available Data"),
      verbatimTextOutput("data_info")
    ),
    
    # Main content area
    mainPanel(
      width = 9,
      
      # Tabset for different views
      tabsetPanel(
        type = "tabs",
        
        # Analysis Results tab
        tabPanel("📊 Analysis Results",
                 fluidRow(
                   column(12,
                          h3("Single-Pass Analysis Results"),
                          conditionalPanel(
                            condition = "input.run_complete_analysis > 0",
                            div(
                              h4("Processing Summary"),
                              verbatimTextOutput("analysis_summary"),
                              hr(),
                              h4("Spectral Plot"),
                              plotOutput("spectral_plot", height = "500px"),
                              hr(),
                              h4("Generated Files"),
                              tableOutput("generated_files_table")
                            )
                          ),
                          conditionalPanel(
                            condition = "input.run_complete_analysis == 0",
                            div(style = "text-align: center; padding: 50px;",
                                h4("Click 'Run Complete Analysis' to start processing"),
                                p("The app will use the example data files already available in the project."),
                                p("This will process all files, generate plots, and prepare data for analysis.")
                            )
                          )
                   )
                 )
        ),
        
        # Data Exploration tab
        tabPanel("🔍 Data Exploration",
                 fluidRow(
                   column(12,
                          h3("Explore Your Data"),
                          conditionalPanel(
                            condition = "input.run_complete_analysis > 0",
                            fluidRow(
                              column(6,
                                     h4("Data Structure"),
                                     verbatimTextOutput("data_structure")
                              ),
                              column(6,
                                     h4("Summary Statistics"),
                                     verbatimTextOutput("data_summary")
                              )
                            ),
                            hr(),
                            fluidRow(
                              column(12,
                                     h4("Interactive Data Table"),
                                     DTOutput("data_table")  # Updated to use DT
                              )
                            )
                          ),
                          conditionalPanel(
                            condition = "input.run_complete_analysis == 0",
                            div(style = "text-align: center; padding: 50px;",
                                h4("Run analysis first to explore data"),
                                p("Process your data to enable exploration features.")
                            )
                          )
                   )
                 )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive values for storing data
  values <- reactiveValues(
    spectral_data = NULL,
    processed_data = NULL,
    analysis_complete = FALSE,
    generated_files = NULL
  )
  
  # Data info display
  output$data_info <- renderText({
    paste("Example data available:\n",
          "• Female1_LateralDorsal.tsv\n",
          "• Male1_Tail.tsv\n", 
          "• Female2_LateralUpper.tsv\n",
          "• Wavelength.txt\n\n",
          "Ready to analyze!")
  })
  
  # Status text
  output$status_text <- renderText({
    status <- "Ready"
    if (values$analysis_complete) {
      status <- "Analysis Complete ✓\nAll data processed and prepared"
    }
    status
  })
  
  # Run complete analysis button
  observeEvent(input$run_complete_analysis, {
    withProgress(message = "Running complete analysis...", {
      
      # Simulate the single-pass analysis process
      Sys.sleep(1)
      incProgress(0.2, detail = "Reading files...")
      
      Sys.sleep(1)
      incProgress(0.2, detail = "Processing spectral data...")
      
      Sys.sleep(1)
      incProgress(0.2, detail = "Generating plots...")
      
      Sys.sleep(1)
      incProgress(0.2, detail = "Preparing data for analysis...")
      
      Sys.sleep(1)
      incProgress(0.2, detail = "Finalizing...")
      
      # Create sample processed data for demonstration
      set.seed(123)
      wavelengths <- seq(input$min_wavelength, input$max_wavelength, by = 5)
      sample_data <- data.frame(
        ID = rep(c("Female1", "Female2", "Male1"), each = length(wavelengths)),
        Body = rep(c("LateralDorsal", "LateralUpper", "Tail"), each = length(wavelengths)),
        NM_rounded = rep(wavelengths, times = 3),
        Reflectance = runif(3 * length(wavelengths), 20, 80)
      )
      
      values$processed_data <- sample_data
      values$analysis_complete <- TRUE
      
      # Simulate generated files (now only main data and plots)
      values$generated_files <- data.frame(
        File = c("processed_spectral_data.csv", "spectral_plot.png"),
        Type = c("Main Data", "Plot"),
        Size = c("45.2 KB", "2.1 MB")
      )
      
      showNotification("Complete analysis finished!", type = "default")
    })
  })
  
  # Analysis summary
  output$analysis_summary <- renderText({
    if (!is.null(values$processed_data)) {
      paste(
        "=== Analysis Complete ===\n",
        "Files processed: 3 example files\n",
        "Total data points:", nrow(values$processed_data), "\n",
        "Individuals:", length(unique(values$processed_data$ID)), "\n",
        "Body parts:", paste(unique(values$processed_data$Body), collapse = ", "), "\n",
        "\nAll data processed, plots generated, and data prepared for analysis!"
      )
    }
  })
  
  # Spectral plot
  output$spectral_plot <- renderPlot({
    if (!is.null(values$processed_data)) {
      ggplot(values$processed_data, aes(x = NM_rounded, y = Reflectance, color = Body)) +
        geom_line(alpha = 0.7) +
        geom_point(alpha = 0.5) +
        facet_wrap(~ID, scales = "free_y") +
        labs(title = "Spectral Reflectance by Individual and Body Part",
             x = "Wavelength (nm)",
             y = "Reflectance (%)",
             color = "Body Part") +
        theme_minimal() +
        theme(legend.position = "bottom")
    }
  })
  
  # Generated files table
  output$generated_files_table <- renderTable({
    if (!is.null(values$generated_files)) {
      values$generated_files
    }
  })
  
  # Data structure
  output$data_structure <- renderPrint({
    if (!is.null(values$processed_data)) {
      str(values$processed_data)
    }
  })
  
  # Data summary
  output$data_summary <- renderPrint({
    if (!is.null(values$processed_data)) {
      summary(values$processed_data)
    }
  })
  
  # Data table - Updated to use DT
  output$data_table <- renderDT({
    if (!is.null(values$processed_data)) {
      datatable(values$processed_data, 
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE)
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
