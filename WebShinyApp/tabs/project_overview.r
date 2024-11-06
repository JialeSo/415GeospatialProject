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
        "This section provides an overview of the project, including its goals, scope, methodology, and any relevant background information. Use this section to introduce your project to users."
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
