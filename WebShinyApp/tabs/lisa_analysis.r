# tabs/lisa_analysis.r

# Load required libraries
library(shiny)
library(bs4Dash)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(sf)

# UI for the LISA Analysis tab
lisa_analysis_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = id,
    fluidRow(
      # Left Tab: Filter Trip Data
      box(
        title = "Filter Trip Data",
        width = 6,
        collapsible = TRUE, collapsed = TRUE,
        pickerInput(ns("district"), "District", 
                    choices = NULL, 
                    selected = NULL,  # Select all districts by default in the server
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
                    selected = NULL,  # Select all villages by default in the server
                    multiple = TRUE,
                    options = pickerOptions(
                      actionsBox = TRUE, 
                      size = 10,
                      selectedTextFormat = "count > 3",
                      liveSearch = TRUE
                    ),
                    width = "100%"),
        selectInput(ns("trip_type"), "Trip Type", 
                    choices = c("Origin and Destination", "Origin", "Destination"), 
                    selected = "Origin and Destination"),
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
                    )),
        actionButton(ns("apply_filter"), "Apply Filter", style = "background-color: #8BD3E6; width: 100%")
      ),
      
      # Right Tab: Filter LISA Parameters
      box(
        title = "Filter LISA Parameters",
        width = 6,
        collapsible = TRUE, collapsed = TRUE,
        selectInput(ns("mapping_feature"), "Mapping Feature", 
                    choices = c("Overall Number of Trips", "Number of Trips per Capita", "Number of Trips per POI"), 
                    selected = "Overall Number of Trips"),
        selectInput(ns("contiguity_method"), "Contiguity Method", 
                    choices = c("Queen", "Rook"), selected = "Queen"),
        radioButtons(ns("result_type"), "Statistical Significance",
                     choices = c("All Results" = "all", "Statistically Significant Only" = "significant"),
                     selected = "all"),
        sliderInput(ns("num_simulation"), "Number of Simulations", min = 40, max = 100, value = 40, step = 10),
        actionButton(ns("apply_lisa_filter"), "Apply Filter", style = "background-color: #8BD3E6; width: 100%")
      )
    ),
    
    fluidRow(
      valueBoxOutput(ns("totalTripsBox"), width = 4),
      valueBoxOutput(ns("tripsWithinJakartaBox"), width = 4),
      valueBoxOutput(ns("tripsOutsideJakartaBox"), width = 4)
    ),
    
    fluidRow(
      box(
        title = "LISA Plot",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        dataTableOutput(ns("filtered_data_table"))
      )
    )
  )
}

# Server for LISA Analysis
lisa_analysis_server <- function(id, datasets) {
  moduleServer(id, function(input, output, session) {
    trip_data <- datasets$trip_data
    jakarta_village <- datasets$jakarta_village
    jakarta_district <- datasets$jakarta_district
    jakarta_poi_final <- datasets$jakarta_poi_final
    jakarta_district_population <- datasets$jakarta_district_population
    jakarta_village_population <- datasets$jakarta_village_population
    
    # Populate "District" dropdown with unique values from the 'origin_district' column in trips_data()
    observe({
      district_choices <- trip_data() %>%
        distinct(origin_district) %>%
        pull(origin_district)
      
      updatePickerInput(session, "district", 
                        choices = district_choices, 
                        selected = district_choices)  # Select all districts by default
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
                        selected = village_choices)  # Select all villages by default
    })
    
    # Filter data by user's input
    filtered_data <- eventReactive(input$apply_filter, {
      data <- trip_data()
      
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
          select(location, day_of_week, time_cluster, driving_mode, num_of_trips, geometry)
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
          select(location, day_of_week, time_cluster, driving_mode, num_of_trips, geometry)
        return(trips_village_dest)
      }
      
      # Case 3: If "Origin and Destination" is selected and Villages are selected
      if (input$trip_type == "Origin and Destination" && length(input$village) > 0) {
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
          select(location, day_of_week, time_cluster, driving_mode, num_of_trips, geometry)
        return(trips_village_origin)
        # # Combine both origin and destination data
        # trips_combined <- data %>%
        #   filter(
        #     (origin_village %in% input$village | destination_village %in% input$village),
        #     (origin_day %in% input$day_of_week | destination_day %in% input$day_of_week),
        #     (origin_time_cluster %in% input$time_cluster | destination_time_cluster %in% input$time_cluster)
        #   ) %>%
        #   bind_rows(
        #     data %>%
        #       filter(origin_village %in% input$village) %>%
        #       mutate(village = origin_village,
        #              day_of_week = origin_day,
        #              time_cluster = origin_time_cluster)
        #   ) %>%
        #   bind_rows(
        #     data %>%
        #       filter(destination_village %in% input$village) %>%
        #       mutate(village = destination_village,
        #              day_of_week = destination_day,
        #              time_cluster = destination_time_cluster)
        #   ) %>%
        #   group_by(village, day_of_week, time_cluster, driving_mode) %>%
        #   summarise(num_of_trips = n(), .groups = "drop") %>%
        #   left_join(jakarta_village(), by = c("village" = "village")) %>%
        #   rename(
        #     location = village,
        #     day_of_week = day_of_week,
        #     time_cluster = time_cluster
        #   ) %>%
        #   filter(!is.na(location)) %>%
        #   select(location, day_of_week, time_cluster, driving_mode, num_of_trips, geometry)
        # 
        # return(trips_combined)
      }
      
      # Case 4: If "Origin" is selected and no Village selected (only District level)
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
          select(location, day_of_week, time_cluster, driving_mode, num_of_trips, geometry)
        return(trips_district_origin)
      }
      
      # Case 5: If "Destination" is selected and no Village selected (only District level)
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
          select(location, day_of_week, time_cluster, driving_mode, num_of_trips, geometry)
        return(trips_district_dest)
      }
      
      # Case 6: If "Origin and Destination" is selected and no Village selected (only District level)
      if (input$trip_type == "Origin and Destination" && length(input$village) == 0) {
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
          select(location, day_of_week, time_cluster, driving_mode, num_of_trips, geometry)
        return(trips_district_origin)
        # # similar logic for combining origin and destination trips at district level
        # trips_combined_district <- data %>%
        #   filter(
        #     (origin_district %in% input$district | destination_district %in% input$district),
        #     (origin_day %in% input$day_of_week | destination_day %in% input$day_of_week),
        #     (origin_time_cluster %in% input$time_cluster | destination_time_cluster %in% input$time_cluster)
        #   ) %>%
        #   bind_rows(
        #     data %>%
        #       filter(origin_district %in% input$district) %>%
        #       mutate(district = origin_district,
        #              day_of_week = origin_day,
        #              time_cluster = origin_time_cluster)
        #   ) %>%
        #   bind_rows(
        #     data %>%
        #       filter(destination_district %in% input$district) %>%
        #       mutate(district = destination_district,
        #              day_of_week = destination_day,
        #              time_cluster = destination_time_cluster)
        #   ) %>%
        #   group_by(district, day_of_week, time_cluster, driving_mode) %>%
        #   summarise(num_of_trips = n(), .groups = "drop") %>%
        #   left_join(jakarta_district(), by = c("district" = "district")) %>%
        #   rename(
        #     location = district,
        #     day_of_week = day_of_week,
        #     time_cluster = time_cluster
        #   ) %>%
        #   filter(!is.na(location)) %>%
        #   select(location, day_of_week, time_cluster, driving_mode, num_of_trips, geometry)
        # 
        # return(trips_combined_district)
      }
      
      return(data)  # If no other conditions, return the data as is
    }, ignoreNULL = FALSE)  # This allows the default data to be used when the button is not clicked
    
    
    # Update the value boxes with correct filtered data
    output$totalTripsBox <- renderValueBox({
      data <- filtered_data()
      
      # Sum the num_of_trips column if it exists and data has rows; otherwise, set to 0
      total_trips <- if (!is.null(data) && nrow(data) > 0) {
        sum(data$num_of_trips, na.rm = TRUE)
      } else 0
      
      valueBox(
        value = HTML(paste("<b style='font-size: 24px;'>", total_trips, "</b>")),
        subtitle = "Total Trips",
        color = "lightblue",
        icon = icon("route"),
        width = 4
      )
    })
    
    output$tripsWithinJakartaBox <- renderValueBox({
      data <- filtered_data()
      
      trips_within_jakarta <- if (!is.null(data) && nrow(data) > 0) {
        data %>%
          filter(location != "outside of jakarta") %>%
          summarise(total_trips = sum(num_of_trips, na.rm = TRUE)) %>%
          pull(total_trips)
      } else 0
      
      valueBox(
        value = HTML(paste("<b style='font-size: 24px;'>", trips_within_jakarta, "</b>")),
        subtitle = "Trips within Jakarta",
        color = "primary",
        icon = icon("city"),
        width = 4
      )
    })
    
    output$tripsOutsideJakartaBox <- renderValueBox({
      data <- filtered_data()

      trips_outside_jakarta <- if (!is.null(data) && nrow(data) > 0) {
        data %>%
          filter(location == "outside of jakarta") %>%
          summarise(total_trips = sum(num_of_trips, na.rm = TRUE)) %>%
          pull(total_trips)
      } else 0
      
      valueBox(
        value = HTML(paste("<b style='font-size: 24px;'>", trips_outside_jakarta, "</b>")),
        subtitle = "Trips Outside Jakarta",
        color = "warning",
        icon = icon("road"),
        width = 4
      )
    })
    
    
    # Plot the density of the 'num_of_trips' column
    output$filtered_data_table <- renderDataTable({
      data <- filtered_data()
      
      # Plot num_of_trips density using ggplot2
      DT::datatable(data, options = list(pageLength = 10))
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
  })
}
