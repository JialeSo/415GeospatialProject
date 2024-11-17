# tabs/lisa_analysis.r

# Load required libraries
library(shiny)
library(bs4Dash)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(sf)
library(spdep) 
library(sfdep) 
library(knitr)
library(kableExtra)
lisa_analysis_ui <- function(id) {
  ns <- NS(id)
  
  # UI structure
  tabItem(
    tabName = id,
    jumbotron(
      title = "Local Spatial Autocorrelation",
      lead = "Uncover patterns of clustering and outliers in ride-hailing trips with Local Moran’s I. This page reveals areas of significant clustering (low-low and high-high) or unusual outliers (low-high and high-low), 
      helping you explore how ride-hailing trips are distributed across Jakarta. 
      Use the 'feature mapping' filters to analyse trips by per capita rates or in relation to the number of Points of Interest (POIs) for deeper insights.",
      btnName = NULL
    ),
    tags$head(
      # Custom CSS and JavaScript
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
        $(document).on('shown.bs.collapse', function(e) {
          if ($(e.target).hasClass('box')) {
            var plotId = $(e.target).find('div[aria-labelledby]').attr('id').replace('box-', '');
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
        title = "Filter Options",
        width = 12,
        collapsible = TRUE, collapsed = TRUE,
        fluidRow(
          column(
            width = 6,
            h4("Filter Trip Data"),
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
            selectInput(ns("trip_type"), "Trip Type", choices = c("Origin", "Destination"), selected = "Origin"),
            selectInput(ns("driving_mode"), "Driving Mode", choices = c("Car and Motorcycle", "Car", "Motorcycle")),
            pickerInput(ns("day_of_week"), "Day of Week", 
                        choices = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                    "Friday", "Saturday", "Sunday"), 
                        selected = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                     "Friday", "Saturday", "Sunday"),
                        multiple = TRUE,
                        options = pickerOptions(actionsBox = TRUE, size = 10, selectedTextFormat = "count > 3")),
            pickerInput(ns("time_cluster"), "Time Cluster", 
                        choices = c("Morning Peak", "Morning Lull", "Afternoon Peak", "Afternoon Lull", 
                                    "Evening Peak", "Evening Lull", "Midnight Peak", "Midnight Lull"), 
                        selected = c("Morning Peak", "Morning Lull", "Afternoon Peak", "Afternoon Lull", 
                                     "Evening Peak", "Evening Lull", "Midnight Peak", "Midnight Lull"),
                        multiple = TRUE,
                        options = pickerOptions(actionsBox = TRUE, size = 10, selectedTextFormat = "count > 3"))
          ),
          column(
            width = 6,
            h4("Filter LISA Parameters"),
            selectInput(ns("mapping_feature"), "Mapping Feature", 
                        choices = c("Overall Number of Trips", "Number of Trips per Capita", "Number of Trips per POI")),
            selectInput(ns("contiguity_method"), "Contiguity Method", choices = c("Queen", "Rook")),
            radioButtons(ns("result_type"), "Statistical Significance", 
                         choices = c("All Results" = "all", "Statistically Significant Only" = "statistically significant")),
            sliderInput(ns("num_simulation"), "Number of Simulations", min = 40, max = 100, value = 40, step = 10)
          )
        ),
        actionButton(ns("apply_filters"), "Apply Filters", style = "background-color: #8BD3E6; width: 100%")
      )
    ),
    
    fluidRow(
      infoBoxOutput(ns("low_low_box"), width = 3),
      infoBoxOutput(ns("high_low_box"), width = 3),
      infoBoxOutput(ns("low_high_box"), width = 3),
      infoBoxOutput(ns("high_high_box"), width = 3)
    ),
    
    
    fluidRow(
      column(
        width = 8,
        box(
          title = "Local Autospatial Correlation of Trips in Jakarta",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          height = "500px",
          tmapOutput(ns("lisa_plot"), height = "100%")
        )
      ),
      box(
        title = "Distribution Count of LISA Categories",
        width = 4,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        height = "250px",
        plotlyOutput(ns("lisa_bar_chart"), height = "100%")
      )
    )
  )
}


# Server
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
    # Define village_choices as a reactive expression
    village_choices <- reactive({
      selected_districts <- if ("Select All" %in% input$district || length(input$district) == 0) {
        trip_data() %>%
          pull(origin_district)
      } else {
        input$district
      }
      
      trip_data() %>%
        filter(origin_district %in% selected_districts) %>%
        distinct(origin_village) %>%
        pull(origin_village)
    })
    observeEvent(input$district, {
      updatePickerInput(
        session, 
        "village", 
        choices = village_choices(), 
        selected = village_choices()  # Select all villages by default
      )
    })
    
    
    # Filter data by user's input
    filtered_data <- eventReactive(input$apply_filters, {
      data <- trip_data()
      # Combine the checks for required inputs and show corresponding error modal
      error_messages <- character()
      
      # Check if required inputs are selected and add the respective error message
      if (length(village_choices()) != length(input$village) && length(input$district > 1)){
        error_messages <- c(error_messages, "Please select all villages in the district(s). If analysing by district-level, please
                            ensure all districts are selected.")
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
    
    # Display filter criteria when applied
    observeEvent(input$apply_filters, {
      showModal(modalDialog(
        title = "Filter Applied",
        HTML(paste("<h5><strong>Trip Data</strong></h5>",
                   "<strong>Districts:</strong>", length(input$district), "selected", "<br>", 
                   "<strong>Villages:</strong>", length(input$village), "selected", "<br>", 
                   "<strong>Trip Type:</strong>", input$trip_type, "<br>",
                   "<strong>Driving Mode:</strong>", input$driving_mode, "<br>",
                   "<strong>Day of Week:</strong>", paste(input$day_of_week, collapse = ", "), "<br>",
                   "<strong>Time Cluster:</strong>", paste(input$time_cluster, collapse = ", "), "<br><br>",
                   "<h5><strong>LISA Parameters</strong></h5>",
                   "<strong>Mapping Feature:</strong>", input$mapping_feature, "<br>", 
                   "<strong>Contiguity Method:</strong>", input$contiguity_method, "<br>",
                   "<strong>Statistical Significance:</strong>", input$result_type, "<br>",
                   "<strong>Number of Simulations:</strong>", input$num_simulation, "<br>"))),
        #easyClose = TRUE,
        #footer = modalButton("Close")
      )
    })
    

    
    # For when filter is applied
    lisa_data <- eventReactive(input$apply_filters, {
      req(filtered_data(), jakarta_poi_final())  # Ensure filtered data is available
      print(paste("LISA FILTERED: ", input$result_type))
      # Step 1: Group and summarize data by location
      if(input$mapping_feature == "Overall Number of Trips"){
        lisa_data <- filtered_data() %>%
          filter(location != "outside of jakarta") %>%  # Exclude 'outside of jakarta'
          group_by(location) %>%
          summarize(
            num_of_trips = sum(num_of_trips, na.rm = TRUE),
            geometry = first(geometry),
            .groups = "drop"
          ) %>%
          st_as_sf()
      } 
      if(input$mapping_feature == "Number of Trips per Capita") {
        if(!is.null(input$village)){
          pop_data <- jakarta_village_population() %>%
            filter(village != "outside of jakarta") %>% 
            group_by(village) %>%
            summarise(population = sum(total_population), .groups = "drop") %>%
            st_drop_geometry() %>%
            dplyr::select(location = village, population) 
        } else{
          pop_data <- jakarta_village_population() %>%
            filter(district != "outside of jakarta") %>% 
            group_by(district) %>%
            summarise(population = sum(total_population), .groups = "drop") %>%
            st_drop_geometry() %>%
            dplyr::select(location = district, population)
        }
        
        lisa_data <- filtered_data() %>%
          filter(location != "outside of jakarta") %>%  # Exclude 'outside of jakarta'
          group_by(location) %>%
          summarize(
            total_trips = sum(num_of_trips, na.rm = TRUE),
            geometry = first(geometry),
            .groups = "drop"
          ) %>%
          left_join(pop_data, by = c("location" = "location")) %>%
          mutate(
            num_of_trips = if_else(population > 0, total_trips / population, 0)  # Calculate trips per capita, set to 0 if total_population is 0
          ) %>%
          st_as_sf()
      } 
      
      if(input$mapping_feature == "Number of Trips per POI") {
        if(!is.null(input$village)){
          poi_data <- jakarta_poi_final() %>%
            filter(village != "outside of jakarta") %>% 
            group_by(village) %>%
            summarise(num_of_poi = n(), .groups = "drop") %>%
            st_drop_geometry() %>%
            dplyr::select(location = village, num_of_poi) 
        } else{
          poi_data <- jakarta_poi_final() %>%
            filter(district != "outside of jakarta") %>% 
            group_by(district) %>%
            summarise(num_of_poi = n(), .groups = "drop") %>%
            st_drop_geometry() %>%
            dplyr::select(location = district, num_of_poi)
        }
        
        lisa_data <- filtered_data() %>%
          filter(location != "outside of jakarta") %>%  # Exclude 'outside of jakarta'
          group_by(location) %>%
          summarize(
            total_trips = sum(num_of_trips, na.rm = TRUE),
            geometry = first(geometry),
            .groups = "drop"
          ) %>%
          left_join(poi_data, by = c("location" = "location")) %>%
          mutate(
            num_of_trips = if_else(!is.na(num_of_poi), total_trips / num_of_poi, 0)  # Calculate trips per POI, set to 0 if num_of_poi is 0
          ) %>%
          st_as_sf()
      } 
      
      # Step 2: Set contiguity method based on user input (Queen or Rook)
      contiguity_method <- ifelse(input$contiguity_method == "Queen", TRUE, FALSE)
      
      # Create spatial contiguity and weight matrices
      nb <- st_contiguity(lisa_data$geometry, queen = contiguity_method)
      wt <- st_weights(nb, style = "W", allow_zero = TRUE)
      
      # Step 3: Attach neighborhood and weight matrices to the data
      wm <- lisa_data %>%
        mutate(nb = nb,
               wt = wt,
               .before = 1)
      
      # Step 4: Calculate local Moran's I for spatial association
      lisa <- wm %>% 
        mutate(local_moran = local_moran(num_of_trips, nb, wt, 
                                         zero.policy = TRUE, nsim = input$num_simulation),
               .before = 1) %>%
        unnest(local_moran)
      
      # Step 5: Prepare data for plotting with labels
      lisa_all <- lisa %>%
        mutate(label = paste("Location:", location, "| LISA:", mean))  # Add label for all results
      
      lisa_significant <- lisa %>%
        filter(p_ii_sim < 0.05) %>%  # Filter for statistically significant results
        mutate(label = paste("Location:", location, "| LISA:", mean))  # Label for significant results only
      
      # Convert both to sf (Simple Features) format for spatial plotting
      lisa_all_sf <- st_as_sf(lisa_all) %>% st_transform(crs = 6384)
      lisa_significant_sf <- st_as_sf(lisa_significant) %>% st_transform(crs = 6384)
      
      # Return the datasets for plotting
      return(list(all = lisa_all_sf, significant = lisa_significant_sf, result_type = input$result_type))
    }, ignoreNULL = FALSE)
    
    
    # Render LISA plot using renderTmap for spatial data
    output$lisa_plot <- renderTmap({
      print(paste("LISA PLOT: ", input$result_type))
      

      
      # Conditional plotting based on input$result_type
      if (lisa_data()$result_type == "statistically significant") {
        base_map <- tm_shape(lisa_data()$all) +
          tm_polygons(id = "", col = "lightgray") +  # Neutral base map color
          tm_borders(col = "black", alpha = 0.6)  # Add borders
        lisa_significant <- lisa_data()$significant
        base_map + tm_shape(lisa_significant) + 
          tm_polygons("mean", 
                      palette = c("lightblue1", "#ec9a64", "green3", "#d21b1c"),
                      title = "Significant LISA Classification",
                      id = "label")  # Show labels for significant areas
      } else {
        base_map <- tm_shape(lisa_data()$all) +
          tm_polygons(id = "", col = "lightgray") +  # Neutral base map color
          tm_borders(col = "black", alpha = 0.6)  # Add borders
        lisa_all <- lisa_data()$all
        tm_shape(lisa_all) + 
          tm_polygons("mean", 
                      palette = c("lightblue1", "#ec9a64", "green3", "#d21b1c"),
                      title = "Overall LISA Classification",
                      id = "label")  # Show labels for all areas
      }
    })
    
    # New: Generate bar chart for LISA results
    output$lisa_bar_chart <- renderPlotly({
      req(lisa_data()) 
      
      # Check if the result_type is "statistically significant"
      if (lisa_data()$result_type == "statistically significant") {
        bar_data <- lisa_data()$significant %>%
          group_by(mean) %>%
          summarise(total_count = n(), .groups = "drop") %>%
          st_drop_geometry()  # Drop geometry here
      } else {
        bar_data <- lisa_data()$all %>%
          group_by(mean) %>%
          summarise(total_count = n(), .groups = "drop") %>%
          st_drop_geometry()  # Drop geometry here
      }
      
      # Plot the total count of each 'mean' category using ggplot
      p <- ggplot(bar_data, aes(x = mean, y = total_count, fill = mean, text = paste("Mean: ", mean, "<br>Total Count: ", total_count))) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("lightblue1", "#ec9a64", "green3", "#d21b1c")) +  # Set the custom colors
        labs(title = NULL,
             x = "LISA Category",
             y = "Total Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none")  # Remove the legend
      
      # Convert to interactive plot using ggplotly
      ggplotly(p, tooltip = "text")
    })
    
    # Update the value boxes with correct filtered data
    output$low_low_box <- renderInfoBox({
      req(lisa_data()) 
      if (lisa_data()$result_type == "statistically significant") {
        data <- lisa_data()$significant %>%
          group_by(mean) %>%
          summarise(total_count = n(), .groups = "drop") %>%
          st_drop_geometry()
      } else {
        data <- lisa_data()$all %>%
          group_by(mean) %>%
          summarise(total_count = n(), .groups = "drop") %>%
          st_drop_geometry()
      }
      
      low_low <- data %>%
        filter(mean == 'Low-Low') %>%
        summarise(sum_total_count = sum(total_count, na.rm = TRUE)) %>%
        pull(sum_total_count)
      
      infoBox(
        title = HTML(paste0("<h4 style='font-weight: bold; margin: 0;'>", low_low, "</h4>")),
        value = HTML(paste0("<p style='font-size: 16px; font-weight: normal; margin: 0;'>", "Low-Low", "</p>")),
        color = "lightblue",
        icon = icon("thumbs-up"),
        width = 3
      )
    })
    
    output$high_low_box <- renderInfoBox({
      req(lisa_data()) 
      if (lisa_data()$result_type == "statistically significant") {
        data <- lisa_data()$significant %>%
          group_by(mean) %>%
          summarise(total_count = n(), .groups = "drop") %>%
          st_drop_geometry()
      } else {
        data <- lisa_data()$all %>%
          group_by(mean) %>%
          summarise(total_count = n(), .groups = "drop") %>%
          st_drop_geometry()
      }
      
      high_low <- data %>%
        filter(mean == 'High-Low') %>%
        summarise(sum_total_count = sum(total_count, na.rm = TRUE)) %>%
        pull(sum_total_count)
      
      infoBox(
        title = HTML(paste0("<h4 style='font-weight: bold; margin: 0;'>", high_low, "</h4>")),
        value = HTML(paste0("<p style='font-size: 16px; font-weight: normal; margin: 0;'>", "High-Low", "</p>")),
        color = "warning",
        icon = icon("times-circle"),
        width = 3
      )
    })
    
    output$low_high_box <- renderInfoBox({
      req(lisa_data()) 
      if (lisa_data()$result_type == "statistically significant") {
        data <- lisa_data()$significant %>%
          group_by(mean) %>%
          summarise(total_count = n(), .groups = "drop") %>%
          st_drop_geometry()
      } else {
        data <- lisa_data()$all %>%
          group_by(mean) %>%
          summarise(total_count = n(), .groups = "drop") %>%
          st_drop_geometry()
      }
      
      low_high <- data %>%
        filter(mean == 'High-High') %>%
        summarise(sum_total_count = sum(total_count, na.rm = TRUE)) %>%
        pull(sum_total_count)
      
      infoBox(
        title = HTML(paste0("<h4 style='font-weight: bold; margin: 0;'>", low_high, "</h4>")),
        value = HTML(paste0("<p style='font-size: 16px; font-weight: normal; margin: 0;'>", "Low-High", "</p>")),
        color = "success",
        icon = icon("check-circle"),
        width = 3
      )
    })
    
    output$high_high_box <- renderInfoBox({
      req(lisa_data()) 
      if (lisa_data()$result_type == "statistically significant") {
        data <- lisa_data()$significant %>%
          group_by(mean) %>%
          summarise(total_count = n(), .groups = "drop") %>%
          st_drop_geometry()
      } else {
        data <- lisa_data()$all %>%
          group_by(mean) %>%
          summarise(total_count = n(), .groups = "drop") %>%
          st_drop_geometry()
      }
      
      high_high <- data %>%
        filter(mean == 'High-High') %>%
        summarise(sum_total_count = sum(total_count, na.rm = TRUE)) %>%
        pull(sum_total_count)
      
      infoBox(
        title = HTML(paste0("<h4 style='font-weight: bold; margin: 0;'>", high_high, "</h4>")),
        value = HTML(paste0("<p style='font-size: 16px; font-weight: normal; margin: 0;'>", "High-High", "</p>")),
        color = "danger",
        icon = icon("exclamation-triangle"),
        width = 3
      )
    })
  })
}