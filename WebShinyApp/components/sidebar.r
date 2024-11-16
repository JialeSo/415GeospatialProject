# components/sidebar.R

logo_path <- "logo.png"
appSidebar <- function() {
  dashboardSidebar(
    fixed = TRUE,
    skin = "light",
    status = "primary",
    id = "sidebar",
    collapsible = TRUE,  # Enables dynamic expansion/collapsing

  
    # Optional Welcome Message
    sidebarUserPanel(
      image = logo_path,  # Replace with your image URL if desired
      name = "Welcome Onboard!"
    ),
    
    # Sidebar Menu with Custom Margins for Headers
    sidebarMenu(
      id = "current_tab",
      
      # Introduction Section with Margin
      div(style = "margin-top: 15px;", sidebarHeader("Introduction")),
      menuItem("Project Overview", tabName = "project_overview", icon = icon("book")),
      
      # Analysis Section with Margin
      div(style = "margin-top: 15px;", sidebarHeader("Analysis")),
      menuItem("Exploratory Data Analysis", tabName = "exploratory_data", icon = icon("chart-line")),
      menuItem("Not in Use", tabName = "exploratory_spatial", icon = icon("globe")),
      menuItem("KDE Analysis", tabName = "kernel_density", icon = icon("chart-area")),
      menuItem("LISA Analysis", tabName = "lisa_analysis", icon = icon("map-marked-alt")),
      menuItem("OD Analysis", tabName = "od_analysis", icon = icon("route")),
      
      # Data Section with Margin
      div(style = "margin-top: 15px;", sidebarHeader("Data")),
      menuItem("Datasets", tabName = "datasets", icon = icon("table"))
    )
  )
}
