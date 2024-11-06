library(shiny)
library(bs4Dash)
library(thematic)
library(waiter)
library(ggplot2)
library(dplyr)

thematic_shiny()

# Source all component files and tab modules
source("components/sidebar.R")
source("components/header.R")
source("components/footer.R")
source("tabs/project_overview.R")
source("tabs/datasets.R")
source("tabs/exploratory_data.R")
source("tabs/exploratory_spatial.R")
source("tabs/kernel_density.R")
source("tabs/lisa_analysis.R")
source("tabs/od_analysis.R")

# Define the main UI
ui <- dashboardPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")  # Include your custom CSS
  ),
  header = appHeader(),
  sidebar = appSidebar(),
  body = dashboardBody(
    tabItems(
      project_overview_ui("project_overview"),
      exploratory_data_ui("exploratory_data"),
      exploratory_spatial_ui("exploratory_spatial"),
      kernel_density_ui("kernel_density"),
      lisa_analysis_ui("lisa_analysis"),
      od_analysis_ui("od_analysis"),
      datasets_ui("datasets")
    )
  ),
  controlbar = NULL,  # Explicitly set controlbar to NULL
  footer = appFooter()  # Optional
)

# Define the main server function
server <- function(input, output, session) {
  # Load trip data (assuming `trip_data.rds` is present)
  trip_data <- reactive({
    readRDS("datasource/trip_data.rds")  # Adjust the file path as needed
  })

  # Call each tab module's server function
  project_overview_server("project_overview", input, output, session)
  exploratory_data_server("exploratory_data", input, output, session, trip_data)
  exploratory_spatial_server("exploratory_spatial", input, output, session)
  kernel_density_server("kernel_density", input, output, session)
  lisa_analysis_server("lisa_analysis", input, output, session)
  od_analysis_server("od_analysis", input, output, session)
  datasets_server("datasets", input, output, session)
}

# Run the application
shinyApp(ui = ui, server = server)
