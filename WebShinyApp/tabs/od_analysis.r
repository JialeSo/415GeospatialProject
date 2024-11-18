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
library(stplanr)
library(sp)
library(reshape2)
library(tidyverse)
library(RColorBrewer)
library(viridis)

# UI
od_analysis_ui <- function(id) {
  ns <- NS(id)
  
  # UI structure
  tabItem(
    tabName = id,
    jumbotron(
      title = "Origin Destination Analysis",
      lead = "Discover why people travel between different areas of Jakarta using origin-destination analysis. Utilize push-pull factor analysis and spatial interaction models to understand the motivations and connections behind travel demand.",
      btnName = NULL
    ),
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
            selectInput(
              ns("spatial_model"), 
              "Type of Spatial Interaction Model",
              choices = c("Origin-Constrained", "Destination-Constrained", "Doubly-Constrained"),
              selected = "Origin-Constrained"
            ),
            radioButtons(
              inputId = ns("factors"), 
              label = "Factors to Include:",
              choices = c(
                "Push & Pull Factors" = "push_pull", 
                "Push Factors Only" = "push_only", 
                "Pull Factors Only" = "pull_only"
              ),
              selected = "push_pull",
              inline = TRUE
            ),
            sliderInput(
              inputId = ns("trip_volume"), 
              label = "Filter by Trip Volume:", 
              min = 1, 
              max = 400, 
              value = c(25, 400), 
            )
    
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
      infoBoxOutput(ns("highest_push_box"), width = 4),
      infoBoxOutput(ns("highest_pull_box"), width = 4),
      infoBoxOutput(ns("highest_trips_box"), width = 4)
    ),

    fluidRow(
      box(
        title = "Origin-Destination Ride-hailing Flow",
        width = 7,
        collapsible = TRUE,
        tmapOutput(ns("odMAP"))
      ),
      box(
        title = "Push Pull Factor Analysis",
        width = 5,
        collapsible = TRUE,
        plotOutput(ns("coeff_summary"))
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
    jakarta_poi_final <- datasets$jakarta_poi_final


    # jakarta_village_sp <- as(jakarta_village, "Spatial")
    # village_dist <- spDists(jakarta_village_sp, longlat = FALSE)
    # village_dist_df <- as.data.frame(village_dist)
    # rownames(village_dist) <- jakarta_village$village
    # colnames(village_dist) <- jakarta_village$village
    # village_rounded_distances <- village_dist_df[1:263, 1:263] %>%
    # mutate(across(everything(), round))

    observe({
      jakarta_poi_final_data <- jakarta_poi_final() %>% st_drop_geometry
      jakarta_district_data <- jakarta_district()
      jakarta_trip_data <- trip_data()

      jakarta_district_sp <- as(jakarta_district_data, "Spatial")
      district_dist <- spDists(jakarta_district_sp, longlat = FALSE)
      district_dist_df <- as.data.frame(district_dist)
      rownames(district_dist) <- jakarta_district_data$district
      colnames(district_dist) <- jakarta_district_data$district
      district_rounded_distances <- district_dist_df[1:44, 1:44] %>%
      mutate(across(everything(), round))
      
      DistrictDistPair <- melt(district_dist) %>%
      rename(dist = value)

      DistrictDistPair$dist <- ifelse (DistrictDistPair$dist == 0, 
                              2000, DistrictDistPair$dist) 
      DistrictDistPair <- DistrictDistPair %>% 
        rename(origin=Var1, 
              destination =Var2) %>% 
        mutate(across(c(origin, destination), as.factor))
      categories <- unique(jakarta_poi_final_data$category)
      for (cat in categories) {
        # Count occurrences at origin for the current category
        origin_counts <- jakarta_poi_final_data %>%
          filter(category == cat) %>%
          group_by(district) %>%
          summarise(count = n()) %>%
          rename(!!paste0("origin_", cat) := count)
        
        # Count occurrences at destination for the current category
        destination_counts <- jakarta_poi_final_data %>%
          filter(category == cat) %>%
          group_by(district) %>%
          summarise(count = n()) %>%
          rename(!!paste0("destination_", cat) := count)
        
        # Join counts to DistrictDistPair
        DistrictDistPair <- DistrictDistPair %>%
          left_join(origin_counts, by = c("origin" = "district")) %>%
          left_join(destination_counts, by = c("destination" = "district"))
      }

      # Replace NA values with 0 (optional, if you want counts to be 0 instead of NA)
      DistrictDistPair <- DistrictDistPair %>%
        mutate(across(starts_with("origin_"), ~ coalesce(., 0))) %>%
        mutate(across(starts_with("destination_"), ~ coalesce(., 0))
        )
        
      DistrictDistPairColNames <- colnames(DistrictDistPair)[4:19]
      DistrictDistPair[DistrictDistPairColNames] <- lapply(
        DistrictDistPair [DistrictDistPairColNames],
        function(x) log(as.numeric(as.character(x)) + 1)
      )

      reactive_data$DistrictDistPair <- DistrictDistPair
    })

    observe({
      reactive_data$district_choices <- jakarta_district() %>% distinct(district)
    })

    # Update the slider range and step based on the OD data
    observe({
      # Extract trip volumes dynamically from the dataset
      trip_volumes <- reactive({
        req(filtered_data())  # Ensure filtered data is available
        trip_data <- filtered_data()
        
        # Calculate trip counts for each OD pair
        trip_counts <- trip_data %>%
          count(origin_district, destination_district, name = "trip_count")
        
        # Cap the max trip volume to 400 for consistent filtering
        max_trip_volume <- min(max(trip_counts$trip_count, na.rm = TRUE), 400)
        
        list(min = 0, max = max_trip_volume)
      })
      
      # Dynamically update the slider input
      updateSliderInput(
        session = session,
        inputId = "trip_volume",
        min = 0,
        max = 400,
        value = c(25,400),  # Default to full range
      )
    })


    # Filter the map data according to Trip Filter Input 
    observeEvent(input$apply_od_filter, {      
      if (input$level_of_analysis == "all") {
        reactive_data$map <- jakarta_district()
      } else if (input$level_of_analysis == "district") {
        reactive_data$map <- jakarta_village() %>% filter(district %in% input$district)
      } else {
        reactive_data$map <- NULL
      }
    })

    observeEvent(input$trip_type, {
      # Update the spatial_model input when trip_type is changed to "Destination"
      if (input$trip_type == "Destination") {
        updateSelectInput(
          session,
          inputId = "spatial_model",
          selected = "Destination-Constrained"
        )
      } else if (input$trip_type == "Origin") {
        # Optionally, default to "Origin-Constrained" if the trip type is "Origin"
        updateSelectInput(
          session,
          inputId = "spatial_model",
          selected = "Origin-Constrained"
        )
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


      # filter by trip volume (slider)
      data <- data %>%
        group_by(origin_district, destination_district) %>%
        mutate(trip_count = n()) %>%
        ungroup() %>%
        filter(
          trip_count >= max(input$trip_volume[1], 25),  # Ensure minimum is at least 1
          trip_count <= input$trip_volume[2]
        )
      

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

    coeffencient_data <- eventReactive(input$apply_od_filter, {
      # Load necessary data
      DistrictDistPair_Data <- reactive_data$DistrictDistPair
      jakarta_trip_data <- filtered_data()

      # Add trips_count column
      DistrictDistPair_Data$trips_count <- apply(DistrictDistPair_Data, 1, function(row) {
        sum(jakarta_trip_data$origin_district == row[['origin']] & 
            jakarta_trip_data$destination_district == row[['destination']])
      })

      # Fit models
      origSIM_district <- glm(
        trips_count ~ origin + destination_Cultural_Attractions + destination_Essentials + 
                      destination_Facilities_Services + destination_Offices_Business + 
                      destination_Others + destination_Restaurants_Food + 
                      destination_Shops + destination_Recreation_Entertainment + 
                      dist - 1,  
        family = poisson(link = "log"),
        data = DistrictDistPair_Data,
        na.action = na.exclude
      )

      destSIM_district <- glm(
        trips_count ~ destination + origin_Cultural_Attractions + origin_Essentials + 
                      origin_Facilities_Services + origin_Offices_Business + 
                      origin_Others + origin_Restaurants_Food + 
                      origin_Shops + origin_Recreation_Entertainment + 
                      dist - 1,  
        family = poisson(link = "log"),
        data = DistrictDistPair_Data,
        na.action = na.exclude
      )

      dbcSIM_district <- glm(
        trips_count ~ origin_Cultural_Attractions + origin_Essentials + 
                      origin_Facilities_Services + origin_Offices_Business + 
                      origin_Others + origin_Restaurants_Food + 
                      origin_Shops + origin_Recreation_Entertainment + destination_Cultural_Attractions + 
                      destination_Essentials + destination_Facilities_Services + 
                      destination_Offices_Business + destination_Others + 
                      destination_Restaurants_Food + destination_Shops + 
                      destination_Recreation_Entertainment + dist - 1, 
        family = poisson(link = "log"),
        data = DistrictDistPair_Data,
        na.action = na.exclude
      )

      # Return models for further use
      list(origSIM_district = origSIM_district, destSIM_district = destSIM_district, dbcSIM_district = dbcSIM_district)
    })

    output$coeff_summary <- renderPlot({
      results <- coeffencient_data()

      # Determine the coefficients based on the selected spatial model
      coefficients_district_df <- switch(
        input$spatial_model,
        "Origin-Constrained" = {
          coef_data <- coef(results$origSIM_district)[c(
            "destination_Cultural_Attractions", "destination_Essentials",
            "destination_Facilities_Services", "destination_Offices_Business",
            "destination_Others", "destination_Restaurants_Food",
            "destination_Shops", "destination_Recreation_Entertainment"
          )]
          data.frame(
            Category = c("Cultural_Attractions", "Essentials", "Facilities_Services",
                        "Offices_Business", "Others", "Restaurants_Food",
                        "Shops", "Recreation_Entertainment"),
            Coefficient = coef_data
          )
        },
        "Destination-Constrained" = {
          coef_data <- coef(results$destSIM_district)[c(
            "origin_Cultural_Attractions", "origin_Essentials",
            "origin_Facilities_Services", "origin_Offices_Business",
            "origin_Others", "origin_Restaurants_Food",
            "origin_Shops", "origin_Recreation_Entertainment"
          )]
          data.frame(
            Category = c("Cultural_Attractions", "Essentials", "Facilities_Services",
                        "Offices_Business", "Others", "Restaurants_Food",
                        "Shops", "Recreation_Entertainment"),
            Coefficient = coef_data
          )
        },
        "Doubly-Constrained" = {
          coef_data <- coef(results$dbcSIM_district)[c(
            "origin_Cultural_Attractions", "origin_Essentials",
            "origin_Facilities_Services", "origin_Offices_Business",
            "origin_Others", "origin_Restaurants_Food",
            "origin_Shops", "origin_Recreation_Entertainment",
            "destination_Cultural_Attractions", "destination_Essentials",
            "destination_Facilities_Services", "destination_Offices_Business",
            "destination_Others", "destination_Restaurants_Food",
            "destination_Shops", "destination_Recreation_Entertainment"
          )]
          data.frame(
            Category = c("Cultural_Attractions", "Essentials", "Facilities_Services",
                        "Offices_Business", "Others", "Restaurants_Food",
                        "Shops", "Recreation_Entertainment",
                        "Cultural_Attractions (Dest)", "Essentials (Dest)", 
                        "Facilities_Services (Dest)", "Offices_Business (Dest)",
                        "Others (Dest)", "Restaurants_Food (Dest)",
                        "Shops (Dest)", "Recreation_Entertainment (Dest)"),
            Coefficient = coef_data
          )
        }
      )

      filtered_data <- switch(
        input$factors,
        "push_pull" = coefficients_district_df,  # Show all coefficients
        "push_only" = coefficients_district_df %>% filter(Coefficient < 0),  # Show only negative coefficients
        "pull_only" = coefficients_district_df %>% filter(Coefficient > 0)   # Show only positive coefficients
      )

      # Reorder categories based on coefficients for better visualization
      filtered_data <- filtered_data %>%
        mutate(Category = fct_reorder(Category, Coefficient, .desc = TRUE))

      # Generate the plot
      ggplot(filtered_data, aes(y = Category, x = Coefficient, fill = Coefficient)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        scale_fill_viridis(option = "D", direction = -1, guide = "none") +
        labs(
          title = paste(input$spatial_model, "Model (", input$factors, ")", sep = " "),
          x = "Coefficient",
          y = "POI Category"
        ) +
        theme(
          axis.text.y = element_text(hjust = 1, size = 10),
          axis.text.x = element_text(size = 10),
          legend.position = "none"
        ) +
        scale_x_continuous(
          position = "top", 
          limits = c(-max(abs(coefficients_district_df$Coefficient)),
                    max(abs(coefficients_district_df$Coefficient)))
        )
    })

    #LOGIC FOR INFO BOXES
    output$highest_push_box <- renderInfoBox({
      req(coeffencient_data())  # Ensure coefficients data is available
      results <- coeffencient_data()

      # Get coefficients for the selected spatial model
      coefficients_district_df <- switch(
        input$spatial_model,
        "Origin-Constrained" = coef(results$origSIM_district),
        "Destination-Constrained" = coef(results$destSIM_district),
        "Doubly-Constrained" = coef(results$dbcSIM_district)
      )

      # Convert coefficients to a data frame
      coefficients_district_df <- as.data.frame(coefficients_district_df)
      colnames(coefficients_district_df) <- "Coefficient"
      coefficients_district_df <- tibble::rownames_to_column(coefficients_district_df, "Category")

      # Filter for push factors (negative coefficients)
      push_factors <- coefficients_district_df %>%
        filter(Coefficient < 0) %>%
        arrange(Coefficient) %>%
        slice(1)  

      formatted_category <- ifelse(
        grepl("^origin", push_factors$Category),
        paste0("(Origin) ", tools::toTitleCase(sub("origin", "", push_factors$Category))),
        ifelse(
          grepl("^destination", push_factors$Category),
          paste0("(Destination) ", tools::toTitleCase(sub("destination", "", push_factors$Category))),
          push_factors$Category
        )
      )

      # If no push factors exist, handle gracefully
      if (nrow(push_factors) == 0) {
        return(infoBox(
          title = "Highest Push Factor",
          value = "No push factors found",
          icon = icon("times-circle"),
          color = "warning",
          fill = TRUE
        ))
      }

      # Render info box
      infoBox(
        title = HTML(paste0("<h4 style='font-weight: bold; margin: 0;'>Coefficient: ", round(push_factors$Coefficient, 2), "</h4>")),
        value = HTML(paste0(
          "<p style='font-size: 16px; font-weight: normal; margin: 0;'>",
          "Most Propulsive Area:<br>",
          "<b>", formatted_category, "</b>",
          "</p>"
        )),
        color = "warning",
        icon = icon("arrow-down"),
      )
    })




    output$highest_pull_box <- renderInfoBox({
      req(coeffencient_data())  # Ensure coefficients data is available
      results <- coeffencient_data()

      # Get coefficients for the selected spatial model
      coefficients_district_df <- switch(
        input$spatial_model,
        "Origin-Constrained" = coef(results$origSIM_district),
        "Destination-Constrained" = coef(results$destSIM_district),
        "Doubly-Constrained" = coef(results$dbcSIM_district)
      )

      # Convert coefficients to a data frame
      coefficients_district_df <- as.data.frame(coefficients_district_df)
      colnames(coefficients_district_df) <- "Coefficient"
      coefficients_district_df <- tibble::rownames_to_column(coefficients_district_df, "Category")

      # Filter for pull factors (positive coefficients)
      pull_factors <- coefficients_district_df %>%
        filter(Coefficient > 0) %>%
        arrange(desc(Coefficient)) %>%
        slice(1)  

      formatted_category <- ifelse(
        grepl("^origin", pull_factors$Category),
        paste0("(Origin) ", tools::toTitleCase(sub("origin", "", pull_factors$Category))),
        ifelse(
          grepl("^destination", pull_factors$Category),
          paste0("(Destination) ", tools::toTitleCase(sub("destination", "", pull_factors$Category))),
          pull_factors$Category
        )
      )

      # If no pull factors exist, handle gracefully
      if (nrow(pull_factors) == 0) {
        return(infoBox(
          title = "Highest Pull Factor",
          value = "No pull factors found",
          icon = icon("times-circle"),
          color = "success",
        ))
      }

      # Render info box
      infoBox(
        title = HTML(paste0("<h4 style='font-weight: bold; margin: 0;'>Coefficient: ", round(pull_factors$Coefficient, 2), "</h4>")),
        value = HTML(paste0(
          "<p style='font-size: 16px; font-weight: normal; margin: 0;'>",
          "Most Attractive Area:<br>",
          "<b>", formatted_category, "</b>",
          "</p>"
        )),
        color = "success",
        icon = icon("arrow-up"),
      )
    })


    output$highest_trips_box <- renderInfoBox({
      trip_data <- filtered_data()
      
      # Calculate the trip data with the highest trip count
      highest_trip <- trip_data %>%
        group_by(origin_district, destination_district) %>%
        summarise(trip_count = n(), .groups = "drop") %>%
        arrange(desc(trip_count)) %>%
        slice(1)  #
      
      # Capitalize the first letter of each word for origin and destination
      origin_district <- tools::toTitleCase(tolower(highest_trip$origin_district))
      destination_district <- tools::toTitleCase(tolower(highest_trip$destination_district))
      
      # Render the info box
      infoBox(
        title = HTML(paste0("<h4 style='font-weight: bold; margin: 0;'>", highest_trip$trip_count, " trips</h4>")),
        value = HTML(paste0(
          "<p style='font-size: 16px; font-weight: normal; margin: 0;'>",
          "Origin: ", origin_district, "<br>",
          "Destination: ", destination_district,
          "</p>"
        )), 
        icon = icon("car"),
        color = "primary"
      )
    })







  })
}

