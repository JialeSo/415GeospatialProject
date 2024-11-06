# tabs/datasets.R

# UI function for "Datasets"
datasets_ui <- function(id) {
  tabItem(
    tabName = id,
    fluidRow(
      box(
        title = "Datasets",
        width = 12,
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        DT::dataTableOutput(NS(id, "data_table"))
      )
    )
  )
}

# Server function for "Datasets"
datasets_server <- function(id, input, output, session) {
  moduleServer(id, function(input, output, session) {
    # Example dataset - Replace with your actual data
    example_data <- data.frame(
      ID = 1:10,
      Name = LETTERS[1:10],
      Value = rnorm(10)
    )
    
    output$data_table <- DT::renderDataTable({
      DT::datatable(example_data, options = list(pageLength = 5))
    })
  })
}
