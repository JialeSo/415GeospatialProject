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
      infoBoxOutput(ns("totalTripsBox"), width = 4),
      infoBoxOutput(ns("tripsWithinJakartaBox"), width = 4),    
      infoBoxOutput(ns("tripsOutsideJakartaBox"), width = 4)
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
    
    observe({
      data <- tripsDistrict()  # Use tripsVillage() for villages if needed
      
      if (is.null(data)) {
        print("Data is NULL")
      } else {
        print(head(data))  # Print the first few rows
        print(summary(data))  # Print a summary of the dataset
        print("Unique values in origin_time_cluster:")
        print(unique(data$origin_time_cluster))
      }
    })
    
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
    
    # Update the value boxes with correct filtered data
    # Update the value boxes with correct filtered data
    output$totalTripsBox <- renderInfoBox({
      data <- filtered_data()
      
      # Sum the num_of_trips column if it exists and data has rows; otherwise, set to 0
      total_trips <- if (!is.null(data) && nrow(data) > 0) {
        sum(data$num_of_trips, na.rm = TRUE)
      } else 0
      
      infoBox(
        title = HTML(paste0("<h4 style='font-weight: bold; margin: 0;'>", total_trips, "</h4>")),
        value = HTML(paste0("<p style='font-size: 16px; font-weight: normal; margin: 0;'>", "Total Trips", "</p>")),
        color = "lightblue",
        icon = icon("car"),
        width = 4
      )
    })
    
    output$tripsWithinJakartaBox <- renderInfoBox({
      data <- filtered_data()
      
      trips_within_jakarta <- if (!is.null(data) && nrow(data) > 0) {
        data %>%
          filter(location != "outside of jakarta") %>%
          summarise(total_trips = sum(num_of_trips, na.rm = TRUE)) %>%
          pull(total_trips)
      } else 0
      
      infoBox(
        title = HTML(paste0("<h4 style='font-weight: bold; margin: 0;'>", trips_within_jakarta, "</h4>")),
        value = HTML(paste0("<p style='font-size: 16px; font-weight: normal; margin: 0;'>", "Trips within Jakarta", "</p>")),
        color = "primary",
        icon = icon("city"),
        width = 4
      )
    })
    
    output$tripsOutsideJakartaBox <- renderInfoBox({
      data <- filtered_data()
      
      trips_outside_jakarta <- if (!is.null(data) && nrow(data) > 0) {
        data %>%
          filter(location == "outside of jakarta") %>%
          summarise(total_trips = sum(num_of_trips, na.rm = TRUE)) %>%
          pull(total_trips)
      } else 0
      infoBox(
        title = HTML(paste0("<h4 style='font-weight: bold; margin: 0;'>", trips_outside_jakarta, "</h4>")),
        value = HTML(paste0("<p style='font-size: 16px; font-weight: normal; margin: 0;'>", "Trips Outside Jakarta", "</p>")),
        color = "warning",
        icon = icon("road"),
        width = 4
      )
    })
    
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
    # RENDER OD MAP 
    output$odMap <- renderLeaflet({
      # Initial rendering of the map
      leaflet() %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>% # Esri black-and-white basemap
        setView(lng = 106.8456, lat = -6.2088, zoom = 10) # Set initial view to Jakarta
    })
    
    observe({
      district_choices <- trip_data() %>%
        distinct(origin_district) %>%
        pull(origin_district)
      
      updatePickerInput(session, "district", 
                        choices = district_choices, 
                        selected = district_choices)
      
      # Populate and select all villages by default
      village_choices <- trip_data() %>%
        distinct(origin_village) %>%
        pull(origin_village)
      
      updatePickerInput(session, "village", 
                        choices = village_choices, 
                        selected = village_choices)
    })
    
    
    observe({
      # Determine the selected level (district or village)
      selected_level <- if (length(input$district) > 0) "district" else "village"
      desire_lines <- if (selected_level == "district") desire_line_district() else desire_line_village()
      spatial_layer <- if (selected_level == "district") jakarta_district() else jakarta_village()
      
      # Validate datasets
      if (is.null(spatial_layer) || nrow(spatial_layer) == 0 || 
          is.null(desire_lines) || nrow(desire_lines) == 0) {
        leafletProxy("odMap") %>%
          clearShapes() %>%
          addPopups(0, 0, "No data available for the selected filters")
        return()
      }
      
      # Filter OD lines to include only those involving selected districts or villages
      filtered_lines <- desire_lines
      if (selected_level == "district" && length(input$district) > 0) {
        filtered_lines <- desire_lines %>%
          filter(origin_district %in% input$district | destination_district %in% input$district)
      } else if (selected_level == "village" && length(input$village) > 0) {
        filtered_lines <- desire_lines %>%
          filter(origin_village %in% input$village | destination_village %in% input$village)
      }
      
      # Filter spatial layer to match the selected districts or villages
      filtered_layer <- spatial_layer
      if (selected_level == "village" && length(input$village) > 0) {
        filtered_layer <- spatial_layer %>% filter(village %in% input$village)
      } else if (selected_level == "district" && length(input$district) > 0) {
        filtered_layer <- spatial_layer %>% filter(district %in% input$district)
      }
      
      # Calculate bounding box for zooming
      bounds <- st_bbox(filtered_layer) %>%
        as.numeric()
      
      # Update the Leaflet map
      leafletProxy("odMap") %>%
        clearShapes() %>% # Clear existing shapes
        addPolygons(
          data = filtered_layer,
          fillOpacity = 0.3,
          color = "black",
          popup = ~paste(
            if (selected_level == "district") "District:" else "Village:",
            if (selected_level == "district") district else village
          )
        ) %>%
        addPolylines(
          data = filtered_lines,
          color = ~colorNumeric("viridis", num_of_trips)(num_of_trips),
          weight = ~log1p(num_of_trips),
          popup = ~paste(
            "Origin:", if (selected_level == "village") origin_village else origin_district, "<br>",
            "Destination:", if (selected_level == "village") destination_village else destination_district, "<br>",
            "Trips:", num_of_trips
          )
        ) %>%
        fitBounds(lng1 = bounds[1], lat1 = bounds[2], lng2 = bounds[3], lat2 = bounds[4]) # Zoom to bounds
    })
    
    
    
    
    
    
    
    # RENDER PUSH-PULL BARPLOT
    # Function to generate plots for coefficients
    generate_plot <- function(data, title) {
      ggplot(data, aes(y = fct_reorder(Category, Coefficient), x = Coefficient)) +
        geom_bar(stat = "identity", fill = "#007BFF") +
        theme_minimal() +
        labs(
          title = title,
          x = "Coefficient",
          y = "POI Category"
        ) +
        theme(
          axis.text.y = element_text(angle = 0, hjust = 1),
          legend.position = "none"
        )
    }
    
    # Render coefficient plots dynamically based on user's input
    output$coefficientPlots <- renderPlot({
      library(gridExtra) # To arrange multiple plots
      
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
      
      # Step 1: Reorder categories and create plots
      # Origin-Constrained Model
      ori_const_data <- data %>%
        filter(Model == "Origin_Constrained") %>%
        mutate(Category = fct_reorder(Category, Coefficient, .desc = TRUE))
      
      plot_origin <- ggplot(ori_const_data, aes(y = Category, x = Coefficient, fill = Coefficient)) +
        geom_bar(stat = "identity", fill = "#007BFF") + # Fixed blue color
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
        geom_bar(stat = "identity", fill = "#007BFF") + # Fixed blue color
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
        geom_bar(stat = "identity", fill = "#007BFF", alpha = 0.7) + # Fixed blue color
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
      
      # Arrange the three plots vertically
      grid.arrange(plot_origin, plot_destination, plot_doubly, ncol = 1)
    })
    
    ##
    
    # RENDER TIME CLUSTER HEATMAP
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
          axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
          axis.text.y = element_text(size = 8),  # Adjust y-axis label size
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold")  # Center and format title
        )
    })
    
    ##
  })
}