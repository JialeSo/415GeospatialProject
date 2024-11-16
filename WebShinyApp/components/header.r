
logo_path <- "logo.png"


appHeader <- function() {
  dashboardHeader(
    title = dashboardBrand(
      title = "STREET SURFERS",
      color = "primary",
      href = "#",
      image = logo_path,
      opacity = 0.8
    ),
    fixed = TRUE
  )
}
