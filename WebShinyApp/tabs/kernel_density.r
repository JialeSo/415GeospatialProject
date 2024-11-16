# tabs/kernel_density.r

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
      # Custom CSS and JavaScript to ensure interactive plots resize on maximize
      tags$style(HTML("
        /* Custom styles for value boxes */
        .small-value-box .small-box {
          padding: 10px;  /* Reduce padding */
          font-size: 12px; /* Smaller font size */
        }
        .small-value-box .small-box .icon {
          font-size: 24px;  /* Adjust icon size */
        }
        .small-value-box .small-box .inner {
          font-size: 14px; /* Adjust text size */
        }

        /* Ensure the interactive plot takes full height and width */
        .box.maximized {
          height: 100% !important;
          width: 100% !important;
        }
      ")),
      # JavaScript to trigger resize of interactive plots on maximize
      tags$script(HTML("
        $(document).ready(function() {
          $('[data-toggle=\"tooltip\"]').tooltip(); 
        });
      "))
    ),
    
  fluidRow(
  box(
    title = "Filters and Parameters",
    width = 12,  # Full width of the tab
    collapsible = TRUE,
    collapsed = FALSE,
    
    # Nested fluidRow to create two columns of equal width (50% each)
    fluidRow(
      column(
        width = 6,  # 50% width
        # Level of Analysis
        h4("Filter Trip Dataset"),
        radioButtons(
          inputId = ns("level_of_analysis"),
          label = "Select Level of Analysis:",
          choices = c("District" = "district", "Village" = "village"),
          selected = "district",  # Default selection
          inline = TRUE  # Display options inline
        ),
        
        # Conditional UI based on Level of Analysis
        uiOutput(ns("conditional_input")),
        
        # Other Inputs
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
                      "Friday", "Saturday", "Sunday"),  # Select everything by default
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
                      "Evening Peak", "Evening Lull", "Midnight Peak", "Midnight Lull"),  # Select everything by default
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE, 
            size = 10,
            selectedTextFormat = "count > 3"
          )
        )
      ),
      
      column(
  width = 6,  # 50% width column
  
  # Header for KDE Parameters
  h4("KDE Parameters"),
  
  # Bandwidth (Sigma) Selection
  tags$div(
    tags$label("Bandwidth Estimation Method (Sigma)"),
    selectInput(ns("bandwidth_method"), NULL,
                choices = c("Diggle's Method" = "bw.diggle",
                            "Pilot Density Method" = "bw.ppl",
                            "Scott's Rule" = "bw.scott",
                            "Cosslett and Van Loon's Rule" = "bw.CvL"),
                selected = "bw.diggle")
  ),
  
  # Edge Correction Toggle using switchInput
  tags$div(
    tags$label("Apply Edge Correction"),
    switchInput(inputId = ns("edge_correction"), 
                value = TRUE, 
                onLabel = "On", 
                offLabel = "Off",
                size = "small")
  ),
  
  # Kernel Type Selection
  tags$div(
    tags$label("Kernel Type"),
    selectInput(ns("kernel_type"), NULL,
                choices = c("Gaussian" = "gaussian", "Epanechnikov" = "epanechnikov", "Disc" = "disc", "Quartic" = "quartic"),
                selected = "Gaussian")
  ),
  
  # Header for Clark-Evans Test Parameters
  h4("Clark-Evans Test Parameters"),  
  # Correction Method
  tags$div(
    tags$label("Correction Method"),
    selectInput(ns("correction_method"), NULL,
                choices = c("None" = "none", "Guard Region" = "guard", "Cumulative Distribution Function" = "cdf"),
                selected = "none")
  ),
  
  # Alternative Hypothesis
  tags$div(
    tags$label("Alternative Hypothesis", 
      tags$i(class = "fas fa-info-circle", title = "This is a native HTML tooltip explaining the alternative hypothesis.", 
             style = "margin-left: 5px; cursor: pointer;")),
    selectInput("alternative_hypothesis", NULL,
                choices = c("Clustered" = "clustered", "Regular" = "regular", "Two-Sided" = "two-sided"),
                selected = "regular")
  ),
  # Number of Simulations
  tags$div(
    tags$label("Number of Simulations"),
    selectInput(ns("num_simulations"), NULL,
                choices = c("30 Simulations" = 30, "100 Simulations" = 100, 
                            "200 Simulations" = 200, "500 Simulations" = 500),
                selected = 99)  # Set 99 as per your previous code if desired
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
    
    # Value Boxes with reduced size (without the class argument)
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
        tmapOutput(ns("kdeMAP"))  # This will display your chart
      ),
      box(
        title = "Destination Count Chart",
        width = 4,
        collapsible = TRUE,
        tableOutput(ns("clarkEvanTable"))  # This will display your chart
      )
      

    )
    
    ####################################
    #### Continue adding plots here ####
    ####################################
    
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
    jakarta_poi_final <- datasets$jakarta_poi_final
    jakarta_district_population <- datasets$jakarta_district_population
    jakarta_village_population <- datasets$jakarta_village_population

    observe({
      reactive_data$district_choices <- trip_data() %>% distinct(origin_district) %>% pull(origin_district) %>% unique()
      reactive_data$village_choices <- trip_data() %>% distinct(origin_village) %>% pull(origin_village) %>% unique()
      if (input$level_of_analysis == "district") {
        reactive_data$map <- jakarta_district()  # Set to jakarta_district data
      } else if (input$level_of_analysis == "village") {
        reactive_data$map <- jakarta_village()  # Set to jakarta_village data
      } else {
        reactive_data$map <- NULL  # Set to NULL if no valid level of analysis is selected
      }
    })

    output$conditional_input <- renderUI({
      if (input$level_of_analysis == "district") {
        pickerInput(ns("district"), "District", 
                    choices = reactive_data$district_choices, 
                    selected = reactive_data$district_choices,  # Select all districts by default in the server
                    multiple = TRUE,
                    options = pickerOptions(
                      actionsBox = TRUE, 
                      size = 10,
                      selectedTextFormat = "count > 3",
                      liveSearch = TRUE
                    ),
                    width = "100%")
      } else {
        pickerInput(ns("village"), "Village", 
                    choices = reactive_data$village_choices, 
                    selected = reactive_data$village_choices,  # Select all villages by default in the server
                    multiple = TRUE,
                    options = pickerOptions(
                      actionsBox = TRUE, 
                      size = 10,
                      selectedTextFormat = "count > 3",
                      liveSearch = TRUE
                    ),
                    width = "100%")
      }
    })

    # Filter data reactively based on input
    filtered_data <- reactive({
      # Fetch the trip data
      data <- trip_data()
      
      # Return NULL if any key inputs are missing
      if (is.null(data) || length(input$day_of_week) == 0 || length(input$time_cluster) == 0) {
        return(NULL)
      }
      
      # Filter by driving mode
      if (input$driving_mode != "Car and Motorcycle") {
        data <- data %>%
          filter(driving_mode == tolower(input$driving_mode))  # Ensure matching lowercase driving modes
      } else {
        data <- data %>%
          filter(driving_mode %in% c("car", "motorcycle"))
      }
      
      # Conditional filtering based on trip type and level of analysis

      if (input$trip_type == "Origin") {
        if (input$level_of_analysis == "village" && length(input$village) > 0) {
          data <- data %>%
            filter(origin_village %in% input$village) %>%
            filter(origin_day %in% input$day_of_week,
                  origin_time_cluster %in% input$time_cluster) %>%
            rename(day_of_week = origin_day, time_cluster = origin_time_cluster, location = origin_village, new_lat = origin_lat, new_lng = origin_lng)
        
        } else if (input$level_of_analysis == "district" && length(input$district) > 0) {
          data <- data %>%
            filter(origin_district %in% input$district) %>%
            filter(origin_day %in% input$day_of_week,
                  origin_time_cluster %in% input$time_cluster)  %>%
            rename(day_of_week = origin_day, time_cluster = origin_time_cluster, location = origin_district, new_lat = origin_lat, new_lng = origin_lng)
        } else {
          # If no valid input is provided, return NULL or empty data
          return(NULL)
        }
        
      } else {  # Handling for 'Destination' trip type
        if (input$level_of_analysis == "village" && length(input$village) > 0) {
          data <- data %>%
            filter(destination_village %in% input$village) %>%
            filter(destination_day %in% input$day_of_week,
                  destination_time_cluster %in% input$time_cluster) %>%
            rename(day_of_week = destination_day, time_cluster = destination_time_cluster, location = destination_village, new_lat = destination_lat, new_lng = destination_lng )

        } else if (input$level_of_analysis == "district" && length(input$district) > 0) {
          data <- data %>%
            filter(destination_district %in% input$district) %>%
            filter(destination_day %in% input$day_of_week,
                  destination_time_cluster %in% input$time_cluster)  %>%
            rename(day_of_week = destination_day, time_cluster = destination_time_cluster, location = destination_village, new_lat = destination_lat, new_lng = destination_lng)
        } else {
          # If no valid input is provided, return NULL or empty data
          return(NULL)
        }
      }
      return(data)
    })

    # Value boxes
    output$totalTripsBox <- renderValueBox({
      data <- filtered_data()
      total_trips <- if (!is.null(data) && nrow(data) > 0) {
        sum(data$num_of_trips, na.rm = TRUE)
      } else 0
      valueBox(value = HTML(paste("<b style='font-size: 24px;'>", total_trips, "</b>")),
               subtitle = "Total Trips", color = "lightblue", icon = icon("car"), width = 4)
    })
    
    # output$tripsWithinJakartaBox <- renderValueBox({
    #   data <- filtered_data()
    #   trips_within_jakarta <- if (!is.null(data) && nrow(data) > 0) {
    #     data %>%
    #       filter(location != "outside of jakarta") %>%
    #       summarise(total_trips = sum(num_of_trips, na.rm = TRUE)) %>%
    #       pull(total_trips)
    #   } else 0
      
    #   valueBox(
    #     value = HTML(paste("<b style='font-size: 24px;'>", trips_within_jakarta, "</b>")),
    #     subtitle = "Trips within Jakarta",
    #     color = "primary",
    #     icon = icon("city"),
    #     width = 4
    #   )
    # })
    
    # output$tripsOutsideJakartaBox <- renderValueBox({
    #   data <- filtered_data()
    #   trips_outside_jakarta <- if (!is.null(data) && nrow(data) > 0) {
    #     data %>%
    #       filter(location == "outside of jakarta") %>%
    #       summarise(total_trips = sum(num_of_trips, na.rm = TRUE)) %>%
    #       pull(total_trips)
    #   } else 0
      
    #   valueBox(
    #     value = HTML(paste("<b style='font-size: 24px;'>", trips_outside_jakarta, "</b>")),
    #     subtitle = "Trips Outside Jakarta",
    #     color = "warning",
    #     icon = icon("road"),
    #     width = 4
    #   )
    # })

    tmap_mode("view")
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
      projection(trip_data_raster) <- CRS("+init=EPSG:6384")

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
          alpha = 0.5                             # Transparency for raster layer
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
      print(reactive_data$trip_data_ppp)
      print(num_simulations)
      print(clark_evans_result)

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
    
    # Display filter criteria when applied
    observeEvent(input$apply_filter, {
      showModal(modalDialog(
        title = "Filter Applied",
        HTML(paste("<strong>Districts:</strong>", length(input$district), "selected", "<br>", 
                   "<strong>Villages:</strong>", length(input$village), "selected", "<br>", 
                   "<strong>Trip Type:</strong>", input$trip_type, "<br>",
                   "<strong>Driving Mode:</strong>", input$driving_mode, "<br>",
                   "<strong>Day of Week:</strong>", paste(input$day_of_week, collapse = ", "), "<br>",
                   "<strong>Time Cluster:</strong>", paste(input$time_cluster, collapse = ", "))),
        easyClose = TRUE,
        footer = NULL
      ))
      # Trigger a log message to track the action
      message("Filter applied: ", Sys.time())
    })
    
    
    ####################################
    #### Continue adding plots here ####
    ####################################
  })
}

