# tabs/exploratory_spatial.R

# UI function for Exploratory Spatial Data Exploration
exploratory_spatial_ui <- function(id) {
  tabItem(
    tabName = id,
    fluidRow(
      box(
        title = "Spatial Data Filters",
        width = 12,
        selectInput(NS(id, "region"), "Region:", choices = c("North", "South", "East", "West")),
        sliderInput(NS(id, "density"), "Density Threshold:", min = 0, max = 1, value = 0.5)
      ),
      box(
        title = "Spatial Plot",
        width = 12,
        plotOutput(NS(id, "spatial_plot"))
      )
    )
  )
}

# Server function for Exploratory Spatial Data Exploration
exploratory_spatial_server <- function(id, input, output, session) {
  moduleServer(id, function(input, output, session) {
    output$spatial_plot <- renderPlot({
      plot(rnorm(100), rnorm(100), col = input$region)
    })
  })
}
