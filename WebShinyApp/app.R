library(shiny)
library(bs4Dash)
library(thematic)
library(waiter)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
library(tmap)
library(shinyBS)
thematic_shiny()

# Source all component files and tab modules
source("components/sidebar.R")
source("components/header.R")
source("components/footer.R")
source("tabs/project_overview.R")
source("tabs/datasets.R")
source("tabs/exploratory_data.R")
source("tabs/kernel_density.R")
source("tabs/lisa_analysis.R")
source("tabs/od_analysis.R")

# Define the main UI
ui <- dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading dashboard..."), color = "#343a40"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")  # Include your custom CSS
  ),
  header = appHeader(),
  sidebar = appSidebar(),
  body = dashboardBody(
    tabItems(
      project_overview_ui("project_overview"),
      exploratory_data_ui("exploratory_data"),
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
  trip_data <- reactive({readRDS("datasource/trip_data.rds")})
  jakarta_village <- reactive({readRDS("datasource/jakarta_village.rds")})
  jakarta_poi_final <- reactive({readRDS("datasource/jakarta_poi_final.rds")})
  jakarta_district <- reactive({readRDS("datasource/jakarta_district.rds")})
  jakarta_district_population <- reactive({readRDS("datasource/jakarta_district_population.rds")})
  jakarta_village_population <- reactive({readRDS("datasource/jakarta_village_population.rds")})
  
  desire_line_district <- reactive({readRDS("datasource/desire_line_district.rds")})
  desire_line_village<- reactive({readRDS("datasource/desire_line_village.rds")})
  coefficients_long_district <- reactive({readRDS("datasource/coefficients_long_district.rds")})
  coefficients_long_village <- reactive({readRDS("datasource/coefficients_long_village.rds")})
  tripsDistrict <- reactive({readRDS("datasource/tripsDistrict.rds")})
  tripsVillage <- reactive({readRDS("datasource/tripsVillage.rds")})

  
  # Create a list to hold all datasets for easier passing
  datasets <- list(
    trip_data = trip_data,
    jakarta_village = jakarta_village,
    jakarta_poi_final = jakarta_poi_final,
    jakarta_district = jakarta_district,
    jakarta_district_population = jakarta_district_population,
    jakarta_village_population = jakarta_village_population,
    
    desire_line_district = desire_line_district,
    desire_line_village = desire_line_village,
    coefficients_long_district = coefficients_long_district,
    coefficients_long_village = coefficients_long_village,
    tripsDistrict = tripsDistrict,
    tripsVillage = tripsVillage
  )
  
  # Call each tab module's server function with the datasets as needed
  exploratory_data_server("exploratory_data", datasets)
  kernel_density_server("kernel_density", datasets)
  lisa_analysis_server("lisa_analysis", datasets)
  od_analysis_server("od_analysis", datasets)
  datasets_server("datasets", datasets)
}

# Run the application
shinyApp(ui = ui, server = server)