# Load required libraries
library(shiny)
library(bs4Dash)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(sf)
library(spatstat)
library(raster)
library(shinyBS)

# UI
od_analysis_ui <- function(id) {
  ns <- NS(id)
  
  # UI structure
  tabItem(
    tabName = id,
    tags$head(
      tags$style(HTML("
        .small-value-box .small-box {
          padding: 10px;
          font-size: 12px;
        }
        .small-value-box .small-box .icon {
          font-size: 24px;
        }
        .small-value-box .small-box .inner {
          font-size: 14px;
        }
        .box.maximized {
          height: 100% !important;
          width: 100% !important;
        }
      ")),
      tags$script(HTML("
        $(document).ready(function() {
          $('[data-toggle=\"tooltip\"]').tooltip(); 
        });
      "))
    ),
    fluidRow(
      box(
        title = "Filters and Parameters",
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        fluidRow(
          column(
            width = 6,
            h4("Filter Trip Dataset"),
            radioButtons(
              inputId = ns("level_of_analysis"),
              label = "Select Level of Analysis:",
              choices = c("All of Jakarta" = "all", "Single District" = "district"),
              selected = "all",
              inline = TRUE
            ),
            uiOutput(ns("conditional_input")),
            selectInput(
              ns("trip_type"), "Trip Type", 
              choices = c("Origin", "Destination"), 
              selected = "Origin"
            ),
            selectInput(
              ns("driving_mode"), "Driving Mode", 
              choices = c("Car and Motorcycle", "Car", "Motorcycle"), 
              selected = "Car and Motorcycle"
            ),
            pickerInput(
              ns("day_of_week"), "Day of Week", 
              choices = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                          "Friday", "Saturday", "Sunday"), 
              selected = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                          "Friday", "Saturday", "Sunday"),
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE, 
                size = 10,
                selectedTextFormat = "count > 3"
              )
            ),
            pickerInput(
              ns("time_cluster"), "Time Cluster", 
              choices = c("Morning Peak", "Morning Lull", "Afternoon Peak", "Afternoon Lull", 
                          "Evening Peak", "Evening Lull", "Midnight Peak", "Midnight Lull"), 
              selected = c("Morning Peak", "Morning Lull", "Afternoon Peak", "Afternoon Lull", 
                          "Evening Peak", "Evening Lull", "Midnight Peak", "Midnight Lull"),
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE, 
                size = 10,
                selectedTextFormat = "count > 3"
              )
            )
          ),
          column(
            width = 6,
            h4("OD Parameters"),
          ),
          column(
            width = 12,
            div(
              style = "display: flex; justify-content: flex-end;",
              actionButton(ns("apply_od_filter"), "Apply Filter", style = "background-color: #8BD3E6; width: 20%")
            )
          )
        )
      )
    ),

    fluidRow(
      box(
        title = "Destination Count Chart",
        width = 8,
        collapsible = TRUE,
        tmapOutput(ns("odMAP"))
      ),
    ),
  )
}


# Server
od_analysis_server <- function(id, datasets) {
  ns <- NS(id)
  reactive_data <- reactiveValues()
  moduleServer(id, function(input, output, session) {
    trip_data <- datasets$trip_data
    jakarta_village <- datasets$jakarta_village
    jakarta_district <- datasets$jakarta_district


    observe({
      reactive_data$district_choices <- jakarta_district() %>% distinct(district)
    })


    observeEvent(input$apply_od_filter, {      
      if (input$level_of_analysis == "all") {
        reactive_data$map <- jakarta_district()
      } else if (input$level_of_analysis == "district") {
        reactive_data$map <- jakarta_village() %>% filter(district %in% input$district)
      } else {
        reactive_data$map <- NULL
      }
    })

    output$conditional_input <- renderUI({
      if (input$level_of_analysis == "district") {
        selectizeInput(ns("district"), 
                      "Select District:", 
                      choices = reactive_data$district_choices, 
                      selected = NULL,
                      multiple = FALSE,
                      options = list(
                        placeholder = 'Please select a district'
                      ),
                      width = "100%")
      }
    })

  filtered_data <- eventReactive(input$apply_od_filter, {
      data <- trip_data()
      print("trip data")
      if (is.null(trip_data())) {
        print("trip_data is null")  # Check for null data
        return(NULL)
      }
      data <- trip_data()
      # Validate required inputs
      if (is.null(data) || length(input$day_of_week) == 0 || length(input$time_cluster) == 0) {
        return(NULL)
      }

      # Filter by driving mode
      if (input$driving_mode != "Car and Motorcycle") {
        data <- data %>%
          filter(driving_mode == tolower(input$driving_mode))
      } else {
        data <- data %>%
          filter(driving_mode %in% c("car", "motorcycle"))
      }

      # Handle level_of_analysis
      if (input$level_of_analysis == "all") {
        # No filtering by district or village when level_of_analysis is "all"
        if (input$trip_type == "Origin") {
          data <- data %>%
            filter(origin_day %in% input$day_of_week,
                  origin_time_cluster %in% input$time_cluster) 
        } else {  # Destination handling for "all"
          data <- data %>%
            filter(destination_day %in% input$day_of_week,
                  destination_time_cluster %in% input$time_cluster)
        }
      } else if (input$level_of_analysis == "district") {
        # Handle filtering for "district"
        if (input$trip_type == "Origin") {
          if (length(input$district) > 0) {
            data <- data %>%
              filter(origin_district %in% input$district,
                    origin_day %in% input$day_of_week,
                    origin_time_cluster %in% input$time_cluster)
          } else {
            return(NULL)  # No valid input for district
          }
        } else {  # Destination handling for "district"
          if (length(input$district) > 0) {
            data <- data %>%
              filter(destination_district %in% input$district,
                    destination_day %in% input$day_of_week,
                    destination_time_cluster %in% input$time_cluster)
          } else {
            return(NULL)  # No valid input for district
          }
        }
      } else {
        return(NULL)  # Invalid level_of_analysis
      }

      return(data)
    })

    od_map_data <- eventReactive(input$apply_od_filter, {
      od_trip_data <- filtered_data()
      odMAP <- reactive_data$map
      if (input$level_of_analysis == 'all') {
        od_data_fij  <- od_trip_data %>%
        filter(origin_district != "outside of jakarta" & destination_district != "outside of jakarta") %>%
        filter(origin_district != destination_district) %>% 
        count(origin_district, destination_district, name = "trip_count") %>%
        rename(origin = origin_district, destination = destination_district)

      jakarta_trip_counts <- od_trip_data %>%
        filter(origin_district != "outside of jakarta" & destination_district != "outside of jakarta") %>%
        filter(origin_district != destination_district) %>% 
        count(origin_district, destination_district, name = "trip_count")

       district_trip_totals <- jakarta_trip_counts %>%
        group_by(district = origin_district) %>%
        summarise(outgoing_trips = sum(trip_count)) %>%
        left_join(
          jakarta_trip_counts %>%
            group_by(district = destination_district) %>%
            summarise(incoming_trips = sum(trip_count)),
          by = "district"
        ) %>%
        mutate(
          outgoing_trips = coalesce(outgoing_trips, 0),  
          incoming_trips = coalesce(incoming_trips, 0), 
          total_trips = outgoing_trips + incoming_trips
        )

        final_map <- odMAP %>% left_join(district_trip_totals, by = c("district" = "district"))
        flow_lines <- od2line(flow = od_data_fij, zones = final_map, zone_code = "district")
      } else if ((input$level_of_analysis == 'district')) {

        od_data_fij  <- od_trip_data %>%
        filter(origin_village != "outside of jakarta" & destination_village != "outside of jakarta") %>%
        filter(origin_district %in% input$district & destination_district %in% input$district,) %>%
        filter(origin_village != destination_village) %>% 
        count(origin_village, destination_village, name = "trip_count") %>%
        rename(origin = origin_village, destination = destination_village)
        
        jakarta_trip_counts <- od_trip_data %>%
        filter(origin_village != "outside of jakarta" & destination_village != "outside of jakarta") %>%
        filter(origin_district %in% input$district & destination_district %in% input$district,) %>%
        filter(origin_village != destination_village) %>% 
        count(origin_village, destination_village, name = "trip_count")

       village_trip_totals <- jakarta_trip_counts %>%
        group_by(village = origin_village) %>%
        summarise(outgoing_trips = sum(trip_count)) %>%
        left_join(
          jakarta_trip_counts %>%
            group_by(village = origin_village) %>%
            summarise(incoming_trips = sum(trip_count)),
          by = "village"
        ) %>%
        mutate(
          outgoing_trips = coalesce(outgoing_trips, 0),  
          incoming_trips = coalesce(incoming_trips, 0), 
          total_trips = outgoing_trips + incoming_trips
        )
        final_map <- odMAP %>% left_join(village_trip_totals, by = c("village" = "village"))
        flow_lines <- od2line(flow = od_data_fij, zones = final_map, zone_code = "village")
      }
      
        list(flow_lines = flow_lines, final_map = final_map)
    })

    output$odMAP <- renderTmap({
      map_data <- od_map_data()  # Triggered only when apply_od_filter is pressed

      # Extract the map and raster data from the reactive result
      flow_lines <- map_data$flow_lines
      final_map <- map_data$final_map

      # Generate the tmap object
      flow_map <- tm_shape(final_map) +
        tm_polygons(
          col = "total_trips",                    # Color by total trips for each district
          palette = "YlOrRd",                     # Yellow-Orange-Red color palette
          title = "Total Trips",
          border.col = "grey",
          lwd = 0.5,
          popup.vars = c("Total Trips" = "total_trips")  # Show district and total trips in popup
        ) +
        tm_shape(flow_lines) +
        tm_lines(
          col = "trip_count",                     # Color by trip volume between OD pairs
          palette = "-Blues",                     # Blue shades for flow lines
          lwd = "trip_count",                     # Line width based on trip volume
          scale = 10,                              # Scale line width for better visualization
          title.col = "Trip Volume",
          alpha = 0.8,
          popup.vars = c("Origin" = "origin", "Destination" = "destination", "Trips" = "trip_count")  # Show OD and trip count in popup
        ) +
        tm_layout(
          title = "Interactive Origin-Destination Traffic Flow within Jakarta",
          legend.outside = TRUE,
          frame = FALSE
        )
    }) 


  })
}

