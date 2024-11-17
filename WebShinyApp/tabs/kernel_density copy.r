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
kernel_density_ui <- function(id) {
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
            h4("KDE Parameters"),
            tags$div(
              tags$label("Bandwidth Estimation Method (Sigma)"),
              selectInput(ns("bandwidth_method"), NULL,
                          choices = c("Diggle's Method" = "bw.diggle",
                                      "Pilot Density Method" = "bw.ppl",
                                      "Scott's Rule" = "bw.scott",
                                      "Cosslett and Van Loon's Rule" = "bw.CvL"),
                          selected = "bw.diggle")
            ),
            tags$div(
              tags$label("Apply Edge Correction"),
              switchInput(inputId = ns("edge_correction"), 
                          value = TRUE, 
                          onLabel = "On", 
                          offLabel = "Off",
                          size = "small")
            ),
            tags$div(
              tags$label("Kernel Type"),
              selectInput(ns("kernel_type"), NULL,
                          choices = c("Gaussian" = "gaussian", "Epanechnikov" = "epanechnikov", "Disc" = "disc", "Quartic" = "quartic"),
                          selected = "Gaussian")
            ),
            h4("Clark-Evans Test Parameters"),
            tags$div(
              tags$label("Correction Method"),
              selectInput(ns("correction_method"), NULL,
                          choices = c("None" = "none", "Guard Region" = "guard", "Cumulative Distribution Function" = "cdf"),
                          selected = "none")
            ),
            tags$div(
              tags$label("Alternative Hypothesis", 
                         tags$i(class = "fas fa-info-circle", title = "This is a native HTML tooltip explaining the alternative hypothesis.", 
                                style = "margin-left: 5px; cursor: pointer;")),
              selectInput(ns("alternative_hypothesis"), NULL,
                          choices = c("Clustered" = "clustered", "Regular" = "regular", "Two-Sided" = "two-sided"),
                          selected = "regular")
            ),
            tags$div(
              tags$label("Number of Simulations"),
              selectInput(ns("num_simulations"), NULL,
                          choices = c("30 Simulations" = 30, "100 Simulations" = 100, 
                                      "200 Simulations" = 200, "500 Simulations" = 500),
                          selected = 99)
            )
          ),
          column(
            width = 12,
            div(
              style = "display: flex; justify-content: flex-end;",
              actionButton(ns("apply_kde_filter"), "Apply Filter", style = "background-color: #8BD3E6; width: 20%")
            )
          )
        )
      )
    ),
    fluidRow(
      valueBoxOutput(ns("totalTripsBox"), width = 4),
      valueBoxOutput(ns("tripsWithinJakartaBox"), width = 4),
      valueBoxOutput(ns("tripsOutsideJakartaBox"), width = 4)
    ),
    fluidRow(
      box(
        title = "Destination Count Chart",
        width = 8,
        collapsible = TRUE,
        tmapOutput(ns("kdeMAP"))
      ),
      box(
        title = "Clark-Evans Test Table",
        width = 4,
        collapsible = TRUE,
        tableOutput(ns("clarkEvanTable"))
      )
    )
  )
}


# Server
kernel_density_server <- function(id, datasets) {
  ns <- NS(id)
  reactive_data <- reactiveValues()
  moduleServer(id, function(input, output, session) {
    trip_data <- datasets$trip_data
    jakarta_village <- datasets$jakarta_village
    jakarta_district <- datasets$jakarta_district

    observe({
      reactive_data$district_choices <- trip_data() %>% distinct(origin_district) %>% pull(origin_district) %>% unique()
      reactive_data$village_choices <- trip_data() %>% distinct(origin_village) %>% pull(origin_village) %>% unique()
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

    filtered_data <- eventReactive(input$apply_kde_filter, {
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
                  origin_time_cluster %in% input$time_cluster) %>%
            rename(day_of_week = origin_day, 
                  time_cluster = origin_time_cluster, 
                  location = origin_district, 
                  new_lat = origin_lat, 
                  new_lng = origin_lng)
        } else {  # Destination handling for "all"
          data <- data %>%
            filter(destination_day %in% input$day_of_week,
                  destination_time_cluster %in% input$time_cluster) %>%
            rename(day_of_week = destination_day, 
                  time_cluster = destination_time_cluster, 
                  location = destination_district, 
                  new_lat = destination_lat, 
                  new_lng = destination_lng)
        }
      } else if (input$level_of_analysis == "district") {
        # Handle filtering for "district"
        if (input$trip_type == "Origin") {
          if (length(input$district) > 0) {
            data <- data %>%
              filter(origin_district %in% input$district,
                    origin_day %in% input$day_of_week,
                    origin_time_cluster %in% input$time_cluster) %>%
              rename(day_of_week = origin_day, 
                    time_cluster = origin_time_cluster, 
                    location = origin_district, 
                    new_lat = origin_lat, 
                    new_lng = origin_lng)
          } else {
            return(NULL)  # No valid input for district
          }
        } else {  # Destination handling for "district"
          if (length(input$district) > 0) {
            data <- data %>%
              filter(destination_district %in% input$district,
                    destination_day %in% input$day_of_week,
                    destination_time_cluster %in% input$time_cluster) %>%
              rename(day_of_week = destination_day, 
                    time_cluster = destination_time_cluster, 
                    location = destination_district, 
                    new_lat = destination_lat, 
                    new_lng = destination_lng)
          } else {
            return(NULL)  # No valid input for district
          }
        }
      } else {
        return(NULL)  # Invalid level_of_analysis
      }

      return(data)
    })

    output$kdeMAP <- renderTmap({
      bandwidth_method <- input$bandwidth_method  # Bandwidth estimation method (e.g., bw.diggle)
      edge_correction <- input$edge_correction  # Edge correction toggle (TRUE or FALSE)
      kernel_type <- input$kernel_type  # Kernel type (e.g., Gaussian, Epanechnikov, etc.)

      # Convert the string to a function call for bandwidth selection dynamically
      bandwidth_func <- match.fun(bandwidth_method)
      kde_trip_data <- filtered_data() %>%
        st_as_sf(coords = c("new_lng", "new_lat"), crs = 6384)
      kde_map <- reactive_data$map 

      kde_map_owin <- as.owin(kde_map)
      trip_data_ppp <- as.ppp(kde_trip_data)
      trip_data_ppp <- trip_data_ppp[kde_map_owin]

      reactive_data$trip_data_ppp <- trip_data_ppp
      reactive_data$kde_map_owin <- kde_map_owin
      
      # Apply dynamic parameters to the density calculation
      trip_data_ppp_bw <- density(
        trip_data_ppp,
        sigma = bandwidth_func(trip_data_ppp),  # Dynamically select the bandwidth function
        edge = edge_correction,                 # Use dynamic edge correction
        kernel = tolower(kernel_type)           # Convert kernel type to lowercase as used in spatstat
      )

      trip_data_raster <- raster(trip_data_ppp_bw)
      projection(trip_data_raster) <- CRS("+init=EPSG:6348")

      map <- tm_shape(kde_map) +
        tm_polygons(
          col = NA,                               # No fill color for polygons
          border.col = "black",                   # Border color for district boundaries
          lwd = 1,                                # Line width for borders
          id = "district",
          alpha = 0.01
        ) +
        # Add the KDE raster layer with transparency
        tm_shape(trip_data_raster) +
        tm_raster(
          palette = "YlOrRd",                     # Color palette for density
          title = "Trip Origin Density",
          alpha = 0.8                             # Transparency for raster layer
        ) +
        tm_layout(
          title = "Kernel Density Estimation of Trip Origins",
          legend.outside = TRUE
        )
      map
    })

    output$clarkEvanTable <- renderTable({
      # Extract relevant values from clark_evans_result
      correction_method <- input$correction_method
      alternative_hypothesis <- input$alternative_hypothesis
      num_simulations <- input$num_simulations
      if (is.null(reactive_data$trip_data_ppp)) {
        return(data.frame(Message = "No data available for Clark-Evans test"))
      }

      # Perform the Clark-Evans test with dynamic inputs
      clark_evans_result <- clarkevans.test(
        reactive_data$trip_data_ppp,
        clipregion = reactive_data$kde_map_owin,
        correction = correction_method,
        alternative = alternative_hypothesis,
        nsim = num_simulations
      )

      observed_statistic <- if (length(clark_evans_result$statistic) > 1) {
        paste(round(clark_evans_result$statistic, 3), collapse = ", ")
      } else {
        round(clark_evans_result$statistic, 3)
      }
      
      p_value <- clark_evans_result$p.value
     

      alternative_hypothesis <- clark_evans_result$alternative
      edge_correction <- if ("No edge correction" %in% clark_evans_result$method) {
        "False"
      } else {
        "True"
      }
      method_description <- paste(clark_evans_result$method, collapse = ", ")
      
      # Create a formatted data frame
      result_df <- data.frame(
        Metric = c("Observed Clark-Evans Ratio (R)", 
                  "P-Value", 
                  "Alternative Hypothesis",
                  "Edge Correction", 
                  "Method"),
        Value = c(
          observed_statistic,
          p_value,
          alternative_hypothesis,
          edge_correction,
          method_description
        ),
        stringsAsFactors = FALSE
      )

      # Add a header row for display purposes
      result_with_header <- rbind(
        c("Clark-Evans Test", ""),  # Header row (spanning columns for display)
        colnames(result_df),        # Column names
        result_df                   # Data values
      )

      result_with_header
    }, rownames = FALSE, colnames = FALSE)
        
  })
}

