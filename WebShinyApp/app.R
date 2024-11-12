library(shiny)
library(bs4Dash)
library(thematic)
library(waiter)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
library(treemap)

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
  # Load datasets reactively
  trip_data <- reactive({
    readRDS("datasource/trip_data.rds")  # Adjust the file path as needed
  })
  
  jakarta_village <- reactive({
    readRDS("datasource/jakarta_village.rds")  # Adjust the file path as needed
  })
  
  jakarta_poi_final <- reactive({
    readRDS("datasource/jakarta_poi_final.rds")  # Adjust the file path as needed
  })
  
  jakarta_district <- reactive({
    readRDS("datasource/jakarta_district.rds")  # Adjust the file path as needed
  })
  jakarta_district_population <- reactive({
    readRDS("datasource/jakarta_district_population")  # Adjust the file path as needed
  })
  jakarta_village_population <- reactive({
    readRDS("datasource/jakarta_village_population")  # Adjust the file path as needed
  })
  
  # Create a list to hold all datasets for easier passing
  datasets <- list(
    trip_data = trip_data,
    jakarta_village = jakarta_village,
    jakarta_poi_final = jakarta_poi_final,
    jakarta_district = jakarta_district,
    jakarta_district_population = jakarta_district_population,
    jakarta_village_population = jakarta_village_population
  )
  
  # Call each tab module's server function with the datasets as needed
  project_overview_server("project_overview", input, output, session)
  exploratory_data_server("exploratory_data", datasets)
  exploratory_spatial_server("exploratory_spatial", input, output, session)
  kernel_density_server("kernel_density", input, output, session)
  lisa_analysis_server("lisa_analysis", datasets)
  od_analysis_server("od_analysis", input, output, session)
  datasets_server("datasets", input, output, session)
}

# Run the application
shinyApp(ui = ui, server = server)
