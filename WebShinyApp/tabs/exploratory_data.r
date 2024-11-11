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
exploratory_data_server <- function(id, input, output, session, datasets) {
  moduleServer(id, function(input, output, session) {
    # Access trip_data reactively
    trip_data <- datasets$trip_data

    # Use observeEvent or another reactive context to work with the data
    observeEvent(trip_data(), {
      data <- trip_data()
      if (!is.null(data)) {
        print(head(data))  # This will print when the data is available
      }
    })

    # Render a plot based on the data
    output$lisa_plot <- renderPlot({
      data <- trip_data()
      if (is.null(data)) {
        return(NULL)  # Handle case where data is missing
      }

      # Example plot using the data
      ggplot(data, aes(x = destination_district)) +
        geom_bar(fill = "steelblue") +  # Customize as needed
        labs(
          title = paste("LISA Plot using", input$cluster_method),
          x = "Destination District",
          y = "Number of Trips"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })
}
