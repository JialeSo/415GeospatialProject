# Load required packages
library(shiny)
library(bs4Dash)
library(ggplot2)

# UI function for LISA Analysis
lisa_analysis_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidRow(
      box(
        title = "LISA Analysis Filters",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        selectInput(ns("cluster_method"), "Clustering Method:", choices = c("Moran's I", "Geary's C")),
        sliderInput(ns("significance_level"), "Significance Level:", min = 0.01, max = 0.1, value = 0.05)
      ),
      fluidRow(
        valueBoxOutput(ns("totalTripsBox"), width = 12)
      ),
      box(
        title = "LISA Plot",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput(ns("lisa_plot"))
      )
    )
  )
}

# Server function for LISA Analysis
lisa_analysis_server <- function(id, input, output, session, datasets) {
  moduleServer(id, function(input, output, session) {
    # Access trip_data reactively
    trip_data <- datasets$trip_data

    # Create a value box showing the number of trips
    output$totalTripsBox <- renderValueBox({
      data <- trip_data()
      total_trips <- if (!is.null(data)) nrow(data) else 0  # Count rows if data is available
      
      valueBox(
        value = total_trips,
        subtitle = "Total Trips",
        color = "primary",
        icon = icon("route")
      )
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
