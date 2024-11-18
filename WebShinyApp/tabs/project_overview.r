# UI function for "Project Overview"
image_path <- "maps.png"


project_overview_ui <- function(id) {
  tabItem(
    tabName = id,
    fluidRow(
      # Top Section: Jumbotron with Text and Image
      tags$div(
        class = "jumbotron",
        style = "background-color: #f8f9fa; padding: 40px; border-radius: 10px; margin-bottom: 20px;",
        fluidRow(
          column(
            width = 6,
            style = "padding-right: 20px; align-self: center;, justify-content: start;",
            h1("Ever wondered where Jakarta's busiest rides start and end?", 
               style = "font-size: 28px; margin-bottom: 20px;"),
            p(
              "Uncover the secrets of travel hotspots and peak times in a few clicks. This tool provides valuable insights for stakeholders such as urban planners, policymakers, and ride-hailing companies.",
              style = "font-size: 18px; line-height: 1.6;"
            )
          ),
          column(
            width = 6,
            style = "display: flex; justify-content: center; align-items: center;",  # Center the image
            tags$img(
              src = image_path,
              style = "max-width: 300px; border-radius: 10px;"
            )
          )
        )
      )
    ),
    fluidRow(
      # Middle Section: Functional Areas
      column(
        width = 4,
        box(
          title = "Kernel Density Estimate",
          width = 12,
          solidHeader = FALSE,
          p(
            "Analyze areas of spatial concentration by number of ride-hailing trips and availability of POIs in Jakarta. Visualize potential hotspots and overall trip distribution patterns along road networks."
          )
        )
      ),
      column(
        width = 4,
        box(
          title = "Local Indicators of Spatial Autocorrelation",
          width = 12,
          solidHeader = FALSE,
          p(
            "Identify specific areas of significant clustering or outliers based on the number of ride-hailing trips using Local Moran’s I. Provide insights into spatial disparities in travel demand."
          )
        )
      ),
      column(
        width = 4,
        box(
          title = "Origin Destination Analysis",
          width = 12,
          solidHeader = FALSE,
          p(
            "Understand movement patterns while highlighting the flow intensity, direction, and connectivity between different areas within Jakarta with Spatial Interaction Modelling."
          )
        )
      )
    ),
    fluidRow(
      # Bottom Section: Future Work
      column(
        width = 12,
        p(
          tags$strong("Future Work"),
          style = "font-size: 20px; text-align: left; margin-top: 20px;"
        ),
        tags$ol(
          tags$li("Incorporating NKDE Analysis, with the addition of public transportation commuter data."),
          tags$li("Modeling Geographical Accessibility to quantify ease of access at popular POIs.")
        )
      )
    )
  )
}
