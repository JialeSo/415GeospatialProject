# tabs/datasets.R

# UI function for "Datasets"
datasets_ui <- function(id) {
  tabItem(
    tabName = id,
    jumbotron(
      title = "Explore the Datasets Used",
      lead = "Explore the datasets driving our geospatial analysis of ride-hailing demands in Jakarta!
      Our datasets include data on ride-hailing trips, point of interests, Jakarta district and township maps, as well as Jakarta's population census data. 
      These include CSV and .shp formatted files.",
      btnName = NULL
    ),
    fluidRow(
      box(
        title = "Trip Dataset",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        DT::dataTableOutput(NS(id, "trips_data_table"))
      )
    ),
    fluidRow(
      box(
        title = "Point of Interests Dataset",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        DT::dataTableOutput(NS(id, "poi_data_table"))
      )
    )
  )
}

# Server function for "Datasets"
datasets_server <- function(id, datasets) {
  moduleServer(id, function(input, output, session) {
    trip_data <- datasets$trip_data
    jakarta_poi_final <- datasets$jakarta_poi_final
    
    output$trips_data_table <- DT::renderDataTable({
      DT::datatable(trip_data(), 
                    options = list(
                      pageLength = 5,
                      scrollX = TRUE  # Enable horizontal scrolling
                    ))
    })
    
    output$poi_data_table <- DT::renderDataTable({
      DT::datatable(jakarta_poi_final(), 
                    options = list(
                      pageLength = 5,
                      scrollX = TRUE  # Enable horizontal scrolling
                    ))
    })
  })
}

