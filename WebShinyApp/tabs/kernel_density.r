# tabs/kernel_density.R

kernel_density_ui <- function(id) {
  tabItem(
    tabName = id,
    fluidRow(
      box(
        title = "Kernel Density Filters",
        width = 12,
        selectInput(NS(id, "kernel_type"), "Kernel Type:", choices = c("Gaussian", "Epanechnikov", "Quartic")),
        sliderInput(NS(id, "bandwidth"), "Bandwidth:", min = 0.1, max = 5, value = 1)
      ),
      box(
        title = "Kernel Density Plot",
        width = 12,
        plotOutput(NS(id, "kernel_density_plot"))
      )
    )
  )
}

# Server function for Kernel Density Analysis
kernel_density_server <- function(id, input, output, session) {
  moduleServer(id, function(input, output, session) {
    output$kernel_density_plot <- renderPlot({
      hist(rnorm(500, sd = input$bandwidth))
    })
  })
}
