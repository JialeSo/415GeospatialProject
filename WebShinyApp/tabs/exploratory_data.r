# tabs/exploratory_data.r

# Load required libraries
library(shiny)
library(bs4Dash)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(sf)

# UI
exploratory_data_ui <- function(id) {
  ns <- NS(id)
  
  # UI structure
  tabItem(
    tabName = id,
    tags$head(
      # Custom CSS to minimize value box size
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
      "))
    ),
    
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
    
    # Value Boxes with reduced size (without the class argument)
    fluidRow(
      valueBoxOutput(ns("totalTripsBox"), width = 4),
      valueBoxOutput(ns("tripsWithinJakartaBox"), width = 4),
      valueBoxOutput(ns("tripsOutsideJakartaBox"), width = 4)
    ),
    
    # Reduced Height Plot Boxes
    fluidRow(
      box(
        title = "Most Popular Locations",
        width = 4,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        maximizable = TRUE,
        plotOutput(ns("top_5_locations"), height = "250px")
      ),
      box(
        title = "Least Popular Locations",
        width = 4,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        maximizable = TRUE,
        plotOutput(ns("bottom_5_locations"), height = "250px")
      )
    ),
    
    fluidRow(
      box(
        title = "Popularity of POIs",
        width = 4,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        maximizable = TRUE,
        plotOutput(ns("top_poi_categories"), height = "250px")
      ),
      box(
        title = "Spread of Trips by Time",
        width = 4,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        maximizable = TRUE,
        plotOutput(ns("num_trips_plot"), height = "250px")
      ),
      box(
        title = "Trips Distribution by Location",
        width = 4,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        maximizable = TRUE,
        tmapOutput(ns("trips_choropleth"), height = "250px")
      )
    )
  )
}


# Server
exploratory_data_server <- function(id, datasets) {
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
        print(glimpse(trips_village_origin))
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
        icon = icon("car"),
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
    
    
    # Plot bar chart of most popular location
    output$top_5_locations <- renderPlot({
      top_locations_data <- filtered_data() %>%
        group_by(location, driving_mode) %>%
        summarise(total_trips = sum(num_of_trips), .groups = "drop") %>%
        group_by(location) %>%
        summarise(total_trips_location = sum(total_trips), .groups = "drop") %>%
        arrange(desc(total_trips_location)) %>%
        slice_head(n = 5) %>%
        left_join(filtered_data() %>%
                    group_by(location, driving_mode) %>%
                    summarise(total_trips = sum(num_of_trips), .groups = "drop"), 
                  by = "location")
      
      # Convert driving_mode to a factor explicitly
      top_locations_data$driving_mode <- factor(top_locations_data$driving_mode, levels = c("car", "motorcycle"))
      
      ggplot(top_locations_data, aes(x = total_trips, y = reorder(location, total_trips_location), fill = driving_mode)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = total_trips), 
                  position = position_stack(vjust = 0.5), 
                  color = "black", 
                  size = 3) +
        scale_fill_manual(values = c("car" = "#A8DADC", "motorcycle" = "#EBA3B4"), 
                          labels = c("Car", "Motorcycle")) +  # Set labels for legend
        labs(
          title = "Most Popular Locations",
          x = "Number of Trips",
          y = "Location",
          fill = "Driving Mode"
        ) +
        theme_minimal() +
        theme(
          legend.position = "top"
        )
    })
    
    # Plot bar chart of least popular locations
    output$bottom_5_locations <- renderPlot({
      # Prepare the data for least popular locations
      least_locations_data <- filtered_data() %>%
        group_by(location, driving_mode) %>%
        summarise(total_trips = sum(num_of_trips), .groups = "drop") %>%
        group_by(location) %>%
        summarise(total_trips_location = sum(total_trips), .groups = "drop") %>%
        arrange(desc(total_trips_location)) %>%  # Sort by least to most trips
        slice_tail(n = 5) %>%  # Get the bottom 5 least popular locations
        left_join(filtered_data() %>%
                    group_by(location, driving_mode) %>%
                    summarise(total_trips = sum(num_of_trips), .groups = "drop"), 
                  by = "location")
      
      # Convert 'driving_mode' to a factor explicitly
      least_locations_data$driving_mode <- factor(least_locations_data$driving_mode, levels = c("car", "motorcycle"))
      
      # Create the plot
      ggplot(least_locations_data, aes(x = total_trips, y = reorder(location, total_trips_location), fill = driving_mode)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = total_trips), 
                  position = position_stack(vjust = 0.5), 
                  color = "black", 
                  size = 3) +
        scale_fill_manual(values = c("car" = "#A8DADC", "motorcycle" = "#EBA3B4"), 
                          labels = c("Car", "Motorcycle")) +  # Set labels for legend
        labs(
          title = "Least Popular Locations",
          x = "Number of Trips",
          y = "Location",
          fill = "Driving Mode"
        ) +
        theme_minimal() +
        theme(
          legend.position = "top"
        )
    })
    
    output$top_poi_categories <- renderPlot({
      # Ensure the necessary data is available
      req(jakarta_poi_final(), filtered_data())  # Ensure that data is available
      
      # Group the data by district or village, and then by category to count the total number of POIs
      poi_data <- if (is.null(input$village)) {
        jakarta_poi_final() %>%
          st_drop_geometry() %>%  # Remove geometry column
          filter(district %in% input$district) %>%  # Filter by selected district
          group_by(category) %>%  # Group by category
          summarise(count = n(), .groups = "drop")  # Count total POIs per category in the selected district
      } else {
        jakarta_poi_final() %>%
          st_drop_geometry() %>%  # Remove geometry column
          filter(village %in% input$village) %>%  # Filter by selected village
          group_by(category) %>%  # Group by category
          summarise(count = n(), .groups = "drop")  # Count total POIs per category in the selected village
      }
      
      # Print to inspect the result
      print(poi_data)
      
      # Ensure no missing categories or counts
      poi_data <- poi_data %>% filter(!is.na(category) & !is.na(count))
      
      # Expanded pastel blue colors for 9 categories
      pastel_blues <- c("#A6C8FF", "#80BFFF", "#66B2FF", "#4D99FF", "#3385FF", 
                        "#1A73E8", "#0066CC", "#005BB5", "#004999")
      
      # Plot the static treemap
      treemap(poi_data, 
              index = "category",  # Group by category
              vSize = "count",  # Size of the blocks based on POI count
              vColor = "count",  # Color blocks based on POI count
              draw = TRUE,  # Draw the treemap
              palette = pastel_blues,
              border.col = "white",  # Set border color to white
              border.lw = 0.5,  # Reduce the border line width
              title = paste("Distribution of POI Categories in", 
                            ifelse(is.null(input$village), "District", "Village")))
    })
    
    # Plot the number of trips for each day of week and time cluster
    output$num_trips_plot <- renderPlot({
      # Ensure the necessary data is available
      req(jakarta_poi_final(), filtered_data())  # Ensure that data is available
      
      # Group the data by day_of_week and time_cluster, then count the total trips
      trip_data <- filtered_data() %>%
        group_by(day_of_week, time_cluster) %>%  # Group by day_of_week and time_cluster
        summarise(total_trips = n(), .groups = "drop")  # Count the total number of trips for each combination
      
      # Print to inspect the result
      print(trip_data)
      
      # Order the 'time_cluster' variable according to the specified order
      trip_data$time_cluster <- factor(trip_data$time_cluster, 
                                       levels = c("Morning Peak", "Morning Lull", 
                                                  "Afternoon Peak", "Afternoon Lull", 
                                                  "Evening Peak", "Evening Lull", 
                                                  "Midnight Peak", "Midnight Lull"))
      
      # Plot the stacked bar graph
      ggplot(trip_data, aes(x = day_of_week, y = total_trips, fill = time_cluster)) +
        geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
        geom_text(aes(label = total_trips), 
                  position = position_stack(vjust = 0.5), 
                  color = "black", 
                  size = 3) +
        scale_fill_brewer(palette = "Blues") +  # Apply the Blues color palette
        labs(title = "Number of Trips by Time",
             x = "Day of Week",
             y = "Total Number of Trips",
             fill = "Time Cluster") +
        theme_minimal() +  # Use a minimal theme
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
    })
    
    # Display choropleth map
    output$trips_choropleth <- renderTmap({
      # Ensure data is available
      req(filtered_data(), jakarta_district)
      
      # Aggregate trips by location, excluding "outside of Jakarta" entries and handle missing geometries
      trip_data <- filtered_data() %>%
        filter(location != "outside of Jakarta") %>%  # Exclude "outside of Jakarta" location
        group_by(location) %>%
        summarise(
          total_trips = sum(as.numeric(num_of_trips), na.rm = TRUE),  # Convert to numeric and sum
          geometry = first(geometry)  # Take the first geometry for each location
        ) %>%
        filter(!st_is_empty(geometry)) %>%  # Remove rows with empty geometries
        st_as_sf()  # Convert to sf if not already an sf object
      
      # Add a 'label' column for hover text, showing location and total_trips with HTML formatting
      trip_data <- trip_data %>%
        mutate(label = paste(location, ": ", total_trips, " trips"))  # Bold the location name
      
      # Plot interactive map
      tm_shape(trip_data) +
        tm_polygons(
          col = "total_trips",
          palette = "Blues",
          border.col = "black",
          lwd = 0.5,
          id = "label"  # Show the label on hover
        ) +
        tm_text("location", size = 0.6, col = "black") +  # Add text for all locations
        tm_basemap("OpenStreetMap")
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

