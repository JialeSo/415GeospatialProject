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
library(spNetwork)


# UI
kernel_network_density_ui <- function(id) {
  ns <- NS(id)
  
  # UI structure
  tabItem(
    tabName = id,
    jumbotron(
      title = "Network Kernel Density Estimation",
      lead = "(NKDE). Unlike traditional KDE, NKDE considers road networks to highlight trip densities along actual paths traveled. This page reveals hotspots and sparse areas with precision, helping you understand urban mobility in the context of road infrastructure. Adjust kernel types, bandwidth, and filtering options to explore trip distributions and identify trends linked to Points of Interest (POIs), road intersections, or transportation hubs across Jakarta.
       Click on Apply Filter to Start Analysing.",
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
        collapsed = TRUE,
        status = 'primary',
        fluidRow(
          column(
            width = 6,
            h4("Filter Trip Dataset Network"),
            uiOutput(ns("selectdistrict")),
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
            h4("NKDE Parameters"),
             # Lixel Size
            tags$div(
              tags$label("Lixel Size"),
              sliderInput(ns("lixel_size"), NULL, min = 500, max = 1000, value = 700, step = 50)
            ),

            # Minimum Distance for Lixels
            tags$div(
              tags$label("Minimum Distance for Lixels"),
              sliderInput(ns("mindist"), NULL, min = 250, max = 500, value = 375, step = 25)
            ),

            # Bandwidth
            tags$div(
              tags$label("Bandwidth (bw)"),
              sliderInput(ns("bandwidth"), NULL, min = 100, max = 1000, value = 200, step = 50)
            ),

            # Kernel Type
            tags$div(
              tags$label("Kernel Type"),
              selectInput(ns("kernel_type"), NULL, choices = c("quartic", "triangle", "epanechnikov", "gaussian", "tricube", "uniform"), selected = "quartic")
            ),

            # Method Type
            tags$div(
              tags$label("Method"),
              selectInput(ns("method_type"), NULL, choices = c("simple", "discrete", "continuous"), selected = "simple")
            ),

          h4("Network Constraint Analysis"),
            # Method Type
            tags$div(
              tags$label("Confidence Level"),
              selectInput(ns("confidence_level"), NULL,  choices = c("99%" = 0.01, "95%" = 0.05, "90%" = 0.10), selected = 0.05)
            ),

            tags$div(
              tags$label("Simulation"),
              sliderInput(ns("nsim"),  NULL, min = 40, max = 100, value = 50, step = 10),
            ),


          ),
          column(
            width = 12,
            div(
              style = "display: flex; justify-content: flex-end;",
              actionButton(ns("apply_nkde_filter"), "Apply Filter", style = "background-color: #8BD3E6; width: 20%")
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
        title = "Network Kernel Density Map",
        width = 8,
        collapsible = TRUE,
        maximizable = TRUE,
        tmapOutput(ns("kdeMAP"))
      ),
      box(
        title = "Spatial Function Chart",
        width = 4,
        collapsible = TRUE,
        plotOutput(ns("kdePlot"))
      )
    )
  )
}


# Server
kernel_network_density_server <- function(id, datasets) {
  ns <- NS(id)
  reactive_data <- reactiveValues()
  moduleServer(id, function(input, output, session) {
    trip_data <- datasets$trip_data
    jakarta_village <- datasets$jakarta_village
    jakarta_district <- datasets$jakarta_district
    jakarta_roads <- datasets$jakarta_roads

    observe({
      reactive_data$district_choices <- jakarta_district() %>% distinct(district)
    })

    observeEvent(input$apply_nkde_filter, {      
      reactive_data$roads <-  jakarta_roads() %>% filter(district %in% input$district)
    })

     showLoadingModal <- function() {
      showModal(modalDialog(
        title = "Processing",
        div(
          style = "text-align: center;",
          tags$div(
            class = "spinner-border text-primary", role = "status",
            tags$span(class = "sr-only", "Loading...")
          ),
          tags$p("Please be patient while loading!", style = "margin-top: 10px; font-weight: bold;")
        ),
        footer = NULL,
        easyClose = FALSE
      ))
    }

    # Remove modal after computation
    removeLoadingModal <- function() {
      removeModal()
    }


    output$selectdistrict <- renderUI({
        selectizeInput(ns("district"), 
                      "Select District:", 
                      choices = reactive_data$district_choices, 
                      selected = NULL,
                      multiple = FALSE,
                      options = list(
                        placeholder = 'Please select a district'
                      ),
                      width = "100%")
    })

    
    filtered_data <- eventReactive(input$apply_nkde_filter, {
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

      # Handle trip type filtering and renaming based on trip type
      if (input$trip_type == "Origin") {
        data <- data %>%
          filter(origin_day %in% input$day_of_week,
                origin_time_cluster %in% input$time_cluster,
                origin_district %in% input$district) %>%
          rename(day_of_week = origin_day, 
                time_cluster = origin_time_cluster, 
                location = origin_district, 
                new_lat = origin_lat, 
                new_lng = origin_lng)
      } else if (input$trip_type == "Destination") {
        data <- data %>%
          filter(destination_day %in% input$day_of_week,
                destination_time_cluster %in% input$time_cluster,
                destination_district %in% input$district) %>%
          rename(day_of_week = destination_day, 
                time_cluster = destination_time_cluster, 
                location = destination_district, 
                new_lat = destination_lat, 
                new_lng = destination_lng)
      }
      return(data)
    })


    # Reactive computation for densities and lixelization, triggered by the Apply Filter button
    density_computation <- eventReactive(input$apply_nkde_filter, {
      showLoadingModal() # Show loading modal
      on.exit(removeLoadingModal()) # Ensure modal is removed when computation finishes
      kde_trip_data <- filtered_data()
      kde_trip_data <- kde_trip_data %>%
        st_as_sf(coords = c("new_lng", "new_lat"), crs = 6384)

    kde_roads <- jakarta_roads() %>%
      filter(district %in% input$district) %>%
      st_simplify(dTolerance = 10)


      # Check if data is valid
      if (is.null(kde_trip_data) || nrow(kde_trip_data) == 0 || 
          is.null(kde_roads) || nrow(kde_roads) == 0) {
        print("kde_trip_data or kde_roads is NULL or empty")
        return(NULL)
      }

      # Lixelization and density calculation using dynamic parameters
      print("Processing lixels...")
      lixels <- lixelize_lines(kde_roads, input$lixel_size, mindist = input$mindist)
      samples <- lines_center(lixels)
      densities <- nkde(
        kde_roads, 
        events = kde_trip_data,
        w = rep(1, nrow(kde_trip_data)),
        samples = samples,
        kernel_name = input$kernel_type,  # Using dynamic kernel type
        bw = input$bandwidth,             # Using dynamic bandwidth
        div = "bw", 
        method = input$method_type,       # Using dynamic method type
        digits = 1, 
        tol = 1,
        grid_shape = c(1,1), 
        max_depth = 8,
        agg = 3,
        sparse = TRUE,
        verbose = FALSE
      )
      print("Density calculation complete")

      # Scale and assign densities
      scaled_densities <- densities * nrow(kde_trip_data) * 1000
      samples$densities <- scaled_densities
      lixels$densities <- scaled_densities

      list(lixels = lixels, samples = samples, kde_trip_data = kde_trip_data)
    })

    # Render the map, triggered by density computation
    output$kdeMAP <- renderTmap({
      result <- density_computation()
      if (is.null(result)) return(NULL)  # Exit if data is not valid

      kde_trip_data <- result$kde_trip_data
      lixels <- result$lixels
      
      # Filter out lixels with densities <= 0.00
      lixels <- lixels %>%
        filter(densities > 0)  # Only keep rows where densities > 0

      # Render the map with pop-ups and tooltips
      tmap_mode('view')
      tm_shape(lixels) +
        tm_lines(
          col = "densities", 
          lwd = 3,
          palette = "YlOrRd", 
          popup.vars = c("Road Name" = "name", "Density" = "densities"),
          tooltip.vars = c("Road Name" = "name")  # Display road name on hover
        ) +
        tm_shape(kde_trip_data) +
        tm_dots(size = 0.001) +
        tm_view(
          set.zoom.limits = c(12, 20),  # Minimum and maximum zoom levels
          bbox = st_bbox(lixels)       # Dynamically focus on the bounding box of filtered lixels
        )
    })
      
      kfun_result <- eventReactive(input$apply_nkde_filter, {
        kde_trip_data <- filtered_data()  # Assuming filtered_data() is defined elsewhere
        kde_trip_data <- kde_trip_data %>%
          st_as_sf(coords = c("new_lng", "new_lat"), crs = 6384)
        kde_roads <- jakarta_roads() %>% filter(district %in% input$district)  # Assuming jakarta_roads() is defined elsewhere
        nsim_value <- as.numeric(input$nsim)
        conf_int_value <- as.numeric(input$confidence_level)

        # Compute the chosen function
         kfun_trip_data <- kfunctions(
          kde_roads, 
          kde_trip_data,
          start = 0, 
          end = 1000, 
          step = 50, 
          width = 50, 
          nsim = nsim_value,  # Dynamic nsim value from the slider
          resolution = 50,
          verbose = FALSE, 
          conf_int = conf_int_value  # Dynamic confidence interval
        )
        kfun_trip_data  # Return the result
      })

      # Render the plot when kfun_result is computed
      output$kdePlot <- renderPlot({
        kfun_res <- kfun_result()

        # Check if plotk exists in the result
        if (!is.null(kfun_res$plotk)) {
          print(kfun_res$plotk)  # Display the plot
        } else {
          plot.new()
          text(0.5, 0.5, "Plot object not found in kfun_result output", cex = 1.2)
        }
      })
    

    })
}

