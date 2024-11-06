# Load required packages
library(shiny)
library(bs4Dash)
library(ggplot2)
library(shinyWidgets)
library(dplyr)

# UI Function for Exploratory Data Exploration with Filters on the Right
exploratory_data_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidRow(
      column(
        width = 9,
        box(
          title = "Trip Data Plot",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput(ns("trip_plot"))
        )
      ),
      column(
        width = 3,
        box(
          title = "Filters",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          pickerInput(
            inputId = ns("driving_mode_filter"),
            label = "Driving Mode:",
            choices = NULL,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `select-all-text` = "Select All",
              `deselect-all-text` = "Deselect All",
              `none-selected-text` = "No selection"
            ),
            multiple = TRUE
          ),
          pickerInput(
            inputId = ns("district_filter"),
            label = "District:",
            choices = NULL,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `select-all-text` = "Select All",
              `deselect-all-text` = "Deselect All",
              `none-selected-text` = "No selection"
            ),
            multiple = TRUE
          )
        )
      )
    )
  )
}

# Server Function for Exploratory Data Exploration with Dropdown Filters
exploratory_data_server <- function(id, input, output, session, trip_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize filter options based on data
    observe({
      data <- trip_data()
      if (!is.null(data)) {
        # Populate driving mode options
        driving_modes <- unique(data$driving_mode)
        shinyWidgets::updatePickerInput(
          session, "driving_mode_filter",
          choices = driving_modes,
          selected = driving_modes
        )

        # Populate district options
        districts <- unique(data$destination_district)
        shinyWidgets::updatePickerInput(
          session, "district_filter",
          choices = districts,
          selected = districts
        )
      }
    })

    # Render plot with filters applied
    output$trip_plot <- renderPlot({
      data <- trip_data()
      if (is.null(data)) {
        return(NULL)
      }

      # Apply filters
      filtered_data <- data
      if (!is.null(input$driving_mode_filter) && length(input$driving_mode_filter) > 0) {
        filtered_data <- filtered_data[filtered_data$driving_mode %in% input$driving_mode_filter, ]
      }
      if (!is.null(input$district_filter) && length(input$district_filter) > 0) {
        filtered_data <- filtered_data[filtered_data$destination_district %in% input$district_filter, ]
      }

      # Plot filtered data
      ggplot(filtered_data, aes(x = destination_district)) +
        geom_bar() +  # No hardcoded color to allow dynamic theming
        labs(
          title = "Trips by Destination District",
          x = "Destination District",
          y = "Number of Trips"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })
}
