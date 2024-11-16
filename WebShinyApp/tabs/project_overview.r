# tabs/project_overview.R

# UI function for "Project Overview"
project_overview_ui <- function(id) {
  tabItem(
    tabName = id,
    fluidRow(
      box(
        title = "Project Overview",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        p("The primary objective of this project is to provide stakeholders, including urban planners, policymakers, and ride-hailing companies, with insights into traffic flows and mobility trends."),
        p("By leveraging geospatial analysis techniques such as Kernel Density Estimation (KDE), Local Indicators of Spatial Association (LISA), and Origin-Destination (OD) analysis, this tool highlights significant patterns that could inform the improvement of urban planning and traffic management.")
      )
    ),
    fluidRow(
      box(
        title = "Key Functionalities",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        h4("1. Kernel Density Estimation (KDE)"),
        tags$ul(
          tags$li("Purpose: KDE helps visualize hotspots of high ride-hailing activity across Jakarta. This heatmap representation identifies areas with a high concentration of origin and destination points."),
          tags$li("Features:"),
          tags$ul(
            tags$li("Interactive map layer showcasing density variations."),
            tags$li("Toggle options to filter KDE analysis by time clusters (e.g., morning peak, evening lull)."),
            tags$li("Layer control for differentiating between origins and destinations.")
          ),
          tags$li("Potential Use Cases: Pinpointing high-demand zones to optimize resource allocation during different time periods.")
        ),
        h4("2. Local Indicators of Spatial Association (LISA)"),
        tags$ul(
          tags$li("Purpose: LISA identifies statistically significant clusters and outliers within the spatial data to reveal how districts relate to one another in terms of trip generation and attraction."),
          tags$li("Features:"),
          tags$ul(
            tags$li("Visualization of clusters (e.g., high-high, low-low) and outliers (e.g., high-low)."),
            tags$li("Map interaction to drill down into specific districts for detailed insights.")
          ),
          tags$li("Potential Use Cases: Detecting areas that act as hubs or bottlenecks and evaluating their spatial influence on surrounding regions.")
        ),
        h4("3. Origin-Destination (OD) Analysis"),
        tags$ul(
          tags$li("Purpose: The OD analysis tracks flows between origin and destination districts, illustrating key movement patterns and the factors influencing them."),
          tags$li("Features:"),
          tags$ul(
            tags$li("Interactive desire line maps segmented by variables such as time cluster, weather conditions, and vehicle type (car or motorcycle)."),
            tags$li("Hover functionality to display district-level details, including centroid locations and trip counts."),
            tags$li("Comparative analysis tools for understanding how POIs and population density affect trip generation.")
          ),
          tags$li("Use Cases: Supporting decision-making to improve accessibility and mitigate congestion by adjusting ride-hailing services in response to demand patterns.")
        )
      )
    ),
    fluidRow(
      box(
        title = "User Experience",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        tags$ul(
          tags$li("Interactive Filtering: Users can select variables such as time cluster or vehicle type for further analysis."),
          tags$li("Dynamic Visualizations: EDA and supporting graphs provide comprehensive insights into spatial and temporal trends.")
        )
      )
    )
  )
}

# Server function for "Project Overview"
project_overview_server <- function(id, input, output, session) {
  moduleServer(id, function(input, output, session) {
    # No specific server logic needed yet
  })
}

