# tabs/od_analysis.r

# Load required libraries
library(shiny)
library(bs4Dash)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(sf)
library(leaflet)
library(tmap)

library(forcats)
library(gridExtra)
library(stplanr)
library(plotly)

# UI
od_analysis_ui <- function(id) {
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
        $(document).on('shown.bs.collapse', function(e) {
          if ($(e.target).hasClass('box')) {
            // Check if the box has been maximized
            var plotId = $(e.target).find('div[aria-labelledby]').attr('id').replace('box-', '');
            // Trigger a resize on the plot when maximized
            setTimeout(function() {
              var plotElement = $('#' + plotId).find('div.plotly');
              if (plotElement.length > 0) {
                Plotly.Plots.resize(plotElement[0]);
              }
            }, 100);
          }
        });
      "))
    ),
    
    fluidRow(
      # Combined Filter Box
      box(
        title = "Filters",
        width = 12,
        collapsible = TRUE, collapsed = TRUE,
        fluidRow(
          # Left Column: Filter Trip Data
          column(
            width = 6,
            pickerInput(ns("district"), "District", 
                        choices = NULL, 
                        selected = NULL,  
                        multiple = TRUE,
                        options = pickerOptions(
                          actionsBox = TRUE, 
                          size = 10,
                          selectedTextFormat = "count > 3",
                          liveSearch = TRUE
                        ),
                        width = "100%"),
            pickerInput(ns("village"), "Village", 
                        choices = NULL, 
                        selected = NULL,  
                        multiple = TRUE,
                        options = pickerOptions(
                          actionsBox = TRUE, 
                          size = 10,
                          selectedTextFormat = "count > 3",
                          liveSearch = TRUE
                        ),
                        width = "100%"),
            selectInput(ns("trip_type"), "Trip Type", 
                        choices = c("Origin", "Destination"), 
                        selected = "Origin"),
            selectInput(ns("driving_mode"), "Driving Mode", 
                        choices = c("Car and Motorcycle", "Car", "Motorcycle"), 
                        selected = "Car and Motorcycle"),
            pickerInput(ns("day_of_week"), "Day of Week", 
                        choices = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                    "Friday", "Saturday", "Sunday"), 
                        selected = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                     "Friday", "Saturday", "Sunday"),  # Select everything by default
                        multiple = TRUE,
                        options = pickerOptions(
                          actionsBox = TRUE, 
                          size = 10,
                          selectedTextFormat = "count > 3"
                        )),
            pickerInput(ns("time_cluster"), "Time Cluster", 
                        choices = c("Morning Peak", "Morning Lull", "Afternoon Peak", "Afternoon Lull", 
                                    "Evening Peak", "Evening Lull", "Midnight Peak", "Midnight Lull"), 
                        selected = c("Morning Peak", "Morning Lull", "Afternoon Peak", "Afternoon Lull", 
                                     "Evening Peak", "Evening Lull", "Midnight Peak", "Midnight Lull"),  # Select everything by default
                        multiple = TRUE,
                        options = pickerOptions(
                          actionsBox = TRUE, 
                          size = 10,
                          selectedTextFormat = "count > 3"
                        ))
          ),
          
          # Right Column: Filter OD Parameters
          column(
            width = 6,
            selectInput(ns("spatial_model"), "Type of Spatial Interaction Model",
                        choices = c("Origin-Constrained", "Destination-Constrained", "Doubly-Constrained"),
                        selected = "Origin-Constrained"),
            checkboxGroupInput(ns("push_pull"), "Push-Pull Factor Influence",
                               choices = c("Push Factors Only", "Pull Factors Only"),
                               selected = "Push Factors Only"),
            actionButton(ns("apply_filter"), "Apply Filter", style = "background-color: #8BD3E6; width: 100%")
          )
        )
      )
    ),
    
    
    # Value Boxes with reduced size (without the class argument)
    fluidRow(
      infoBoxOutput(ns("leastPopularPushFactorBox"), width = 4),  # Push factor with lowest coefficient
      infoBoxOutput(ns("mostPopularPullFactorBox"), width = 4),  # Pull factor with highest coefficient
      infoBoxOutput(ns("highestTripsLocationBox"), width = 4) # Location with highest trips
    ),
    
    
    ####################################
    #### Continue adding plots here ####
    ####################################
    
    # OD Map
    fluidRow(
      box(
        title = "Origin-Destination Desire Line Map",
        width = 12, 
        collapsible = FALSE, 
        status = "primary",
        solidHeader = TRUE,
        leafletOutput(ns("odMap"), height = "400px")
      )
    ),
    
    fluidRow(
      # Push-Pull Factor Analysis Box
      box(
        title = "Push-Pull Factor Analysis",
        width = 6,
        collapsible = TRUE,
        status = "primary",
        solidHeader = TRUE,
        plotOutput(ns("coefficientPlots"), height = "400px")
      ),
      # Heatmap of Time Cluster Box
      box(
        title = "Heatmap of Time Cluster",
        width = 6,
        collapsible = TRUE,
        status = "primary",
        solidHeader = TRUE,
        plotOutput(ns("heatmapTimeCluster"), height = "400px")
      )
    )
    ###
    
  )
}

# Server
od_analysis_server <- function(id, datasets) {
  moduleServer(id, function(input, output, session) {
    
    trip_data <- reactive({readRDS("datasource/trip_data.rds")})
    jakarta_poi_final <- reactive({readRDS("datasource/jakarta_poi_final.rds")})
    jakarta_district_population <- reactive({readRDS("datasource/jakarta_district_population.rds")})
    jakarta_village_population <- reactive({readRDS("datasource/jakarta_village_population.rds")})
    
    jakarta_district <- reactive({st_transform(datasets$jakarta_district(), crs = 4326)})
    jakarta_village <- reactive({st_transform(datasets$jakarta_village(), crs = 4326)})
    desire_line_district <- reactive({st_transform(datasets$desire_line_district(), crs = 4326)})
    desire_line_village <- reactive({st_transform(datasets$desire_line_village(), crs = 4326)})
    coefficients_long_district <- reactive({ readRDS("datasource/coefficients_long_district.rds")})
    coefficients_long_village <- reactive({ readRDS("datasource/coefficients_long_village.rds")})
    tripsDistrict <- reactive({ readRDS("datasource/tripsDistrict.rds")})
    tripsVillage <- reactive({ readRDS("datasource/tripsVillage.rds")})
    
    
    
    ################# POPULATE FILTERS ##########################
    # Populate "District" dropdown with unique values from the 'origin_district' column in trips_data()
    observe({
      district_choices <- trip_data() %>%
        distinct(origin_district) %>%
        pull(origin_district)
      
      updatePickerInput(session, "district", 
                        choices = district_choices, 
                        selected = district_choices)
    })
    
    # Populate "Village" dropdown based on selected "District" values
    observeEvent(input$district, {
      # If specific districts are selected, filter villages accordingly
      selected_districts <- if ("Select All" %in% input$district || length(input$district) == 0) {
        trip_data() %>%
          pull(origin_district)
      } else {
        input$district
      }
      
      village_choices <- trip_data() %>%
        filter(origin_district %in% selected_districts) %>%
        distinct(origin_village) %>%
        pull(origin_village)
      
      updatePickerInput(session, "village", 
                        choices = village_choices, 
                        selected = village_choices)  
    })
    
    # Filter data by user's input
    filtered_data <- eventReactive(input$apply_filter, {
      data <- trip_data()
      
      # If no filters are applied, return the entire dataset
      if (is.null(input$district) && is.null(input$village)) {
        return(data)
      }
      
      # Combine the checks for required inputs and show corresponding error modal
      # Initialize an empty vector to hold error messages
      error_messages <- character()
      
      # Check if required inputs are selected and add the respective error message
      if (length(input$district) == 0 && length(input$village) == 0) {
        error_messages <- c(error_messages, "Please select at least one district and village.")
      }
      
      if (length(input$day_of_week) == 0) {
        error_messages <- c(error_messages, "Please select at least one day of the week.")
      }
      
      if (length(input$time_cluster) == 0) {
        error_messages <- c(error_messages, "Please select at least one time cluster.")
      }
      
      # If there are any error messages, show a modal with all errors
      if (length(error_messages) > 0) {
        showModal(modalDialog(
          title = "Error",
          HTML(paste(error_messages, collapse = "<br/>")),  # Combine error messages with line breaks
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      }
      
      # Filter by driving mode
      if (input$driving_mode == "Car and Motorcycle") {
        data <- data %>%
          filter(driving_mode %in% c("car", "motorcycle"))
      }
      if (input$driving_mode == "Car") {
        data <- data %>%
          filter(driving_mode == "car")
      }
      if (input$driving_mode == "Motorcycle") {
        data <- data %>%
          filter(driving_mode == "motorcycle")
      }
      
      # Case 1: Filter based on selected "Origin" and selected Villages
      if (input$trip_type == "Origin" && length(input$village) > 0) {
        trips_village_origin <- data %>%
          filter(origin_village %in% input$village,
                 origin_day %in% input$day_of_week,
                 origin_time_cluster %in% input$time_cluster) %>%
          group_by(origin_village, origin_day, origin_time_cluster, driving_mode) %>%
          summarise(num_of_trips = n(), .groups = "drop") %>%
          left_join(jakarta_village(), by = c("origin_village" = "village")) %>%
          rename(location = origin_village,
                 day_of_week = origin_day,
                 time_cluster = origin_time_cluster) %>%
          dplyr::select(location, day_of_week, time_cluster, driving_mode, num_of_trips, geometry)
        print(trips_village_origin %>% filter(is.na(location)))
        return(trips_village_origin)
      }
      
      # Case 2: Filter based on selected "Destination" and selected Villages
      if (input$trip_type == "Destination" && length(input$village) > 0) {
        trips_village_dest <- data %>%
          filter(destination_village %in% input$village,
                 destination_day %in% input$day_of_week,
                 destination_time_cluster %in% input$time_cluster) %>%
          group_by(destination_village, destination_day, destination_time_cluster, driving_mode) %>%
          summarise(num_of_trips = n(), .groups = "drop") %>%
          left_join(jakarta_village(), by = c("destination_village" = "village")) %>%
          rename(location = destination_village,
                 day_of_week = destination_day,
                 time_cluster = destination_time_cluster) %>%
          dplyr::select(location, day_of_week, time_cluster, driving_mode, num_of_trips, geometry)
        return(trips_village_dest)
      }
      
      # Case 3: If "Origin" is selected and no Village selected (only District level)
      if (input$trip_type == "Origin" && length(input$village) == 0) {
        trips_district_origin <- data %>%
          filter(origin_district %in% input$district,
                 origin_day %in% input$day_of_week,
                 origin_time_cluster %in% input$time_cluster) %>%
          group_by(origin_district, origin_day, origin_time_cluster, driving_mode) %>%
          summarise(num_of_trips = n(), .groups = "drop") %>%
          left_join(jakarta_district(), by = c("origin_district" = "district")) %>%
          rename(location = origin_district,
                 day_of_week = origin_day,
                 time_cluster = origin_time_cluster) %>%
          dplyr::select(location, day_of_week, time_cluster, driving_mode, num_of_trips, geometry)
        return(trips_district_origin)
      }
      
      # Case 4: If "Destination" is selected and no Village selected (only District level)
      if (input$trip_type == "Destination" && length(input$village) == 0) {
        trips_district_dest <- data %>%
          filter(destination_district %in% input$district,
                 destination_day %in% input$day_of_week,
                 destination_time_cluster %in% input$time_cluster) %>%
          group_by(destination_district, destination_day, destination_time_cluster, driving_mode) %>%
          summarise(num_of_trips = n(), .groups = "drop") %>%
          left_join(jakarta_district(), by = c("destination_district" = "district")) %>%
          rename(location = destination_district,
                 day_of_week = destination_day,
                 time_cluster = destination_time_cluster) %>%
          dplyr::select(location, day_of_week, time_cluster, driving_mode, num_of_trips, geometry)
        return(trips_district_dest)
      }
      
      return(data)  # If no other conditions, return the data as is
    }, ignoreNULL = FALSE)  # This allows the default data to be used when the button is not clicked
    
    ############################################
    #### LEAST POPULAR PUSH FACTOR INFOBOX #####
    ############################################
    output$leastPopularPushFactorBox <- renderInfoBox({
      # Check whether user selected district or village level
      if (!is.null(input$district) && length(input$district) > 0) {
        data <- coefficients_long_district()  # District-level data
      } else if (!is.null(input$village) && length(input$village) > 0) {
        data <- coefficients_long_village()  # Village-level data
      } else {
        return(infoBox(
          title = "Least Popular Push Factor",
          value = "No Data Available",
          color = "danger",
          icon = icon("times-circle")
        ))
      }
      
      # Filter data based on the selected spatial model
      filtered_data <- data %>%
        filter(Model == input$spatial_model)  # Match the current spatial model
      
      # Find the least popular push factor
      least_popular <- filtered_data %>%
        arrange(Coefficient) %>%
        slice(1)  # Get the row with the lowest coefficient
      
      # Check if the least_popular entry exists
      if (nrow(least_popular) == 0 || is.null(least_popular$Category)) {
        return(infoBox(
          title = "Least Popular Push Factor",
          value = "No Data Available",
          color = "danger",
          icon = icon("times-circle")
        ))
      }
      
      # Render the infoBox
      infoBox(
        title = "Least Popular Push Factor",
        value = paste(least_popular$Category, ":", round(least_popular$Coefficient, 2)),
        color = "danger",
        icon = icon("arrow-down")
      )
    })
    
    ############################################
    #### MOST POPULAR PULL FACTOR INFOBOX ######
    ############################################
    output$mostPopularPullFactorBox <- renderInfoBox({
      # Check whether user selected district or village level
      if (!is.null(input$district) && length(input$district) > 0) {
        data <- coefficients_long_district()  # District-level data
      } else if (!is.null(input$village) && length(input$village) > 0) {
        data <- coefficients_long_village()  # Village-level data
      } else {
        return(infoBox(
          title = "Most Popular Pull Factor",
          value = "No Data Available",
          color = "warning",
          icon = icon("exclamation-circle")
        ))
      }
      
      # Filter data based on the selected spatial model
      filtered_data <- data %>%
        filter(Model == input$spatial_model)  # Match the current spatial model
      
      # Find the most popular pull factor
      most_popular <- filtered_data %>%
        arrange(desc(Coefficient)) %>%
        slice(1)  # Get the row with the highest coefficient
      
      # Check if the most_popular entry exists
      if (nrow(most_popular) == 0 || is.null(most_popular$Category)) {
        return(infoBox(
          title = "Most Popular Pull Factor",
          value = "No Data Available",
          color = "warning",
          icon = icon("exclamation-circle")
        ))
      }
      
      # Render the infoBox
      infoBox(
        title = "Most Popular Pull Factor",
        value = paste(most_popular$Category, ":", round(most_popular$Coefficient, 2)),
        color = "success",
        icon = icon("arrow-up")
      )
    })
    
    
    output$highestTripsLocationBox <- renderInfoBox({
      data <- filtered_data()  # Use filtered data for current user selections
      
      # Find the location with the highest number of trips
      highest_trips <- data %>%
        group_by(location) %>%
        summarise(total_trips = sum(num_of_trips, na.rm = TRUE)) %>%
        arrange(desc(total_trips)) %>%
        slice(1)
      
      infoBox(
        title = "Location with Most Trips",
        value = paste(highest_trips$location, ":", highest_trips$total_trips),
        color="gray",
        icon = icon("map-marker-alt")
      )
    })
    
    ##############################
    
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
    
    
    ############################################
    ########## RENDER OD MAP####################
    ############################################
    output$odMap <- renderLeaflet({
      # Initial rendering of the map
      leaflet() %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>% # Esri black-and-white basemap
        setView(lng = 106.8456, lat = -6.2088, zoom = 10) # Set initial view to Jakarta
    })
    
    observe({
      # Ensure trip data is loaded
      req(trip_data())
      
      # Populate district choices
      district_choices <- trip_data() %>%
        distinct(origin_district) %>%
        pull(origin_district)
      
      updatePickerInput(session, "district", 
                        choices = district_choices, 
                        selected = district_choices)
      
      # Populate village choices
      village_choices <- trip_data() %>%
        distinct(origin_village) %>%
        pull(origin_village)
      
      updatePickerInput(session, "village", 
                        choices = village_choices, 
                        selected = village_choices)
      
      # Populate origin_day choices
      day_choices <- trip_data() %>%
        distinct(origin_day) %>%
        pull(origin_day)
      
      updatePickerInput(session, "day_of_week", 
                        choices = day_choices, 
                        selected = day_choices)
      
      # Populate origin_time_cluster choices
      time_cluster_choices <- trip_data() %>%
        distinct(origin_time_cluster) %>%
        pull(origin_time_cluster)
      
      updatePickerInput(session, "time_cluster", 
                        choices = time_cluster_choices, 
                        selected = time_cluster_choices)
      
      # Populate driving_mode choices (car, motorcycle, etc.)
      driving_mode_choices <- trip_data() %>%
        distinct(driving_mode) %>%
        pull(driving_mode) %>%
        tolower()  # Ensure all modes are lowercase
      
      updateSelectInput(session, "driving_mode", 
                        choices = c("Car and Motorcycle", driving_mode_choices),
                        selected = "Car and Motorcycle")
    })
    
    
    
    observeEvent(input$apply_filter, {
      # Ensure trip data is available
      req(desire_line_district, desire_line_village)
      
      # Determine whether to use district or village level
      selected_level <- if (length(input$district) == 1) "village" else "district"
      
      # Select the dataset based on the selected level
      desire_lines <- if (selected_level == "district") {
        desire_line_district()
      } else {
        desire_line_village()
      }
      
      # Validate dataset
      if (is.null(desire_lines) || nrow(desire_lines) == 0) {
        leafletProxy("odMap") %>%
          clearShapes() %>%
          addPopups(0, 0, "No data available for the selected filters.")
        return()
      }
      
      # Apply filters and dynamically create the `location` column
      filtered_lines <- desire_lines %>%
        filter(
          # District or village filtering based on trip type and level
          if (selected_level == "district") {
            if (input$trip_type == "Origin") {
              origin_district %in% input$district
            } else {
              destination_district %in% input$district
            }
          } else {
            if (input$trip_type == "Origin") {
              origin_village %in% input$village
            } else {
              destination_village %in% input$village
            }
          },
          
          # Day of the week filtering (select all if no input)
          if (length(input$day_of_week) > 0) {
            origin_day %in% input$day_of_week
          } else {
            TRUE  # Select all rows
          },
          
          # Time cluster filtering (select all if no input)
          if (length(input$time_cluster) > 0) {
            origin_time_cluster %in% input$time_cluster
          } else {
            TRUE  # Select all rows
          },
          
          # Driving mode filtering (select all if no input)
          if (input$driving_mode == "Car and Motorcycle") {
            driving_mode %in% c("car", "motorcycle")
          } else if (!is.null(input$driving_mode)) {
            driving_mode == tolower(input$driving_mode)
          } else {
            TRUE  # Select all rows
          }
        ) %>%
        mutate(
          location = if (selected_level == "district") {
            if (input$trip_type == "Origin") origin_district else destination_district
          } else {
            if (input$trip_type == "Origin") origin_village else destination_village
          }
        )
      
      # Ensure filtered_lines is valid and not empty
      if (nrow(filtered_lines) == 0) {
        leafletProxy("odMap") %>%
          clearShapes() %>%
          addPopups(0, 0, "No trips match the selected filters.")
        return()
      }
      
      # Proceed with the map rendering
      spatial_layer <- if (selected_level == "district") {
        jakarta_district() %>%
          filter(district %in% input$district)
      } else {
        jakarta_village() %>%
          filter(village %in% input$village)
      }
      
      # Map rendering logic
      bounds <- st_bbox(spatial_layer) %>% as.numeric()
      trip_palette <- colorNumeric("viridis", domain = filtered_lines$num_of_trips)
      
      leafletProxy("odMap") %>%
        clearShapes() %>%
        addPolygons(
          data = spatial_layer,
          fillOpacity = 0.3,
          color = "black",
          popup = ~paste(if (selected_level == "district") "District:" else "Village:", 
                         if (selected_level == "district") district else village)
        ) %>%
        addPolylines(
          data = filtered_lines,
          color = ~trip_palette(num_of_trips),
          weight = ~log1p(num_of_trips),
          popup = ~paste(
            "Origin:", if (selected_level == "village") origin_village else origin_district, "<br>",
            "Destination:", if (selected_level == "village") destination_village else destination_district, "<br>",
            "Trips:", num_of_trips
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = trip_palette,
          values = filtered_lines$num_of_trips,
          title = "Trip Count",
          opacity = 1
        ) %>%
        fitBounds(lng1 = bounds[1], lat1 = bounds[2], lng2 = bounds[3], lat2 = bounds[4])
    })
    
    
    
    
    
    
    
    
    ############################################
    ########## RENDER PUSH-PULL BARPLOT ########
    ############################################
    
    output$coefficientPlots <- renderPlot({
      # Check whether user selected district or village level
      if (!is.null(input$district) && length(input$district) > 0) {
        data <- coefficients_long_district()
        plot_title <- "District-Level Coefficients"
      } else if (!is.null(input$village) && length(input$village) > 0) {
        data <- coefficients_long_village()
        plot_title <- "Village-Level Coefficients"
      } else {
        return(NULL)  # If neither is selected, return no plot
      }
      
      # Step 1: Generate plots for all models
      # Origin-Constrained Model
      ori_const_data <- data %>%
        filter(Model == "Origin_Constrained") %>%
        mutate(Category = fct_reorder(Category, Coefficient, .desc = TRUE))
      
      plot_origin <- ggplot(ori_const_data, aes(y = Category, x = Coefficient, fill = Coefficient)) +
        geom_bar(stat = "identity", fill = "#007BFF") +
        theme_minimal() +
        labs(
          title = "Origin-Constrained Model",
          x = "Coefficient",
          y = "POI Category"
        ) +
        theme(
          axis.text.y = element_text(angle = 0, hjust = 1),
          legend.position = "none"
        ) +
        scale_x_continuous(position = "top", limits = c(-max(abs(ori_const_data$Coefficient)), max(abs(ori_const_data$Coefficient))))
      
      # Destination-Constrained Model
      dest_const_data <- data %>%
        filter(Model == "Destination_Constrained") %>%
        mutate(Category = fct_reorder(Category, Coefficient, .desc = TRUE))
      
      plot_destination <- ggplot(dest_const_data, aes(y = Category, x = Coefficient, fill = Coefficient)) +
        geom_bar(stat = "identity", fill = "#007BFF") +
        theme_minimal() +
        labs(
          title = "Destination-Constrained Model",
          x = "Coefficient",
          y = "POI Category"
        ) +
        theme(
          axis.text.y = element_text(angle = 0, hjust = 1),
          legend.position = "none"
        ) +
        scale_x_continuous(position = "top", limits = c(-max(abs(dest_const_data$Coefficient)), max(abs(dest_const_data$Coefficient))))
      
      # Doubly-Constrained Model
      dbc_const_data <- data %>%
        filter(Model == "Doubly_Constrained") %>%
        mutate(Category = fct_reorder(Category, Coefficient, .desc = TRUE))
      
      plot_doubly <- ggplot(dbc_const_data, aes(y = Category, x = Coefficient, fill = Coefficient)) +
        geom_bar(stat = "identity", fill = "#007BFF", alpha = 0.7) +
        theme_minimal() +
        labs(
          title = "Doubly-Constrained Model",
          x = "Coefficient",
          y = "POI Category"
        ) +
        theme(
          axis.text.y = element_text(angle = 0, hjust = 1),
          legend.position = "none"
        ) +
        scale_x_continuous(position = "top", limits = c(-max(abs(dbc_const_data$Coefficient)), max(abs(dbc_const_data$Coefficient))))
      
      # Step 2: Render the plot based on user selection
      if (is.null(input$spatial_model) || input$spatial_model == "Origin-Constrained") {
        return(plot_origin)
      } else if (input$spatial_model == "Destination-Constrained") {
        return(plot_destination)
      } else if (input$spatial_model == "Doubly-Constrained") {
        return(plot_doubly)
      } else {
        # Default to Origin-Constrained if input is invalid
        return(plot_origin)
      }
    })
    
    ############################################
    ########## RENDER HEATMAP TIME CLUSTER######
    ############################################
    output$heatmapTimeCluster <- renderPlot({
      # Check whether user selected district or village level
      if (!is.null(input$district) && length(input$district) > 0) {
        data <- tripsDistrict()  # Use () for reactive objects
        level <- "district"
      } else if (!is.null(input$village) && length(input$village) > 0) {
        data <- tripsVillage()  # Use () for reactive objects
        level <- "village"
      } else {
        return(NULL)  # If neither is selected, return no plot
      }
      
      # Ensure origin_time_cluster is a factor with correct levels and labels
      data$origin_time_cluster <- factor(
        data$origin_time_cluster,
        levels = c("Midnight Peak", "Midnight Lull", 
                   "Morning Peak", "Morning Lull", 
                   "Afternoon Peak", "Afternoon Lull", 
                   "Evening Peak", "Evening Lull"),
        labels = c("Midnight Peak", "Midnight Lull", 
                   "Morning Peak", "Morning Lull", 
                   "Afternoon Peak", "Afternoon Lull", 
                   "Evening Peak", "Evening Lull")
      )
      
      # Customize plot titles and y-axis labels based on the level
      if (level == "district") {
        y_label <- "Destination District"
        plot_title <- "Heatmap of Trips Throughout the Time of Day (District Level)"
      } else {
        y_label <- "Origin Village"
        plot_title <- "Heatmap of Trips Throughout the Time of Day (Village Level)"
      }
      
      # Create the heatmap
      ggplot(data, aes(x = origin_time_cluster, y = origin_, fill = trips_count)) +
        geom_tile(color = "white") +  # Add white gridlines between tiles
        scale_fill_gradient(low = "white", high = "navy", name = "Trip Count") +  # Customize fill gradient
        scale_x_discrete(position = "top") +  # Place x-axis labels at the top
        labs(
          title = plot_title,
          x = "Time Cluster",
          y = y_label
        ) +
        theme_minimal() +  # Use a minimal theme for clarity
        theme(
          axis.text.x = element_text(hjust = 1),  # Rotate x-axis labels
          axis.text.y = element_text(size = 8),  # Adjust y-axis label size
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold")  # Center and format title
        )
    })
    
    #################################
  })
}