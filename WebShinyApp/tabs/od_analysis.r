# tabs/od_analysis.R

# UI function for OD Analysis
od_analysis_ui <- function(id) {
  tabItem(
    tabName = id,
    fluidRow(
      box(
        title = "OD Analysis Filters",
        width = 12,
        selectInput(NS(id, "origin"), "Origin:", choices = c("Region 1", "Region 2", "Region 3")),
        selectInput(NS(id, "destination"), "Destination:", choices = c("Region 1", "Region 2", "Region 3")),
        sliderInput(NS(id, "time_range"), "Time Range:", min = 0, max = 24, value = c(6, 18))
      ),
      box(
        title = "OD Plot",
        width = 12,
        plotOutput(NS(id, "od_plot"))
      )
    )
  )
}

# Server function for OD Analysis
od_analysis_server <- function(id, input, output, session) {
  moduleServer(id, function(input, output, session) {
    output$od_plot <- renderPlot({
      plot(rnorm(100), rnorm(100), col = "blue")
    })
  })
}
