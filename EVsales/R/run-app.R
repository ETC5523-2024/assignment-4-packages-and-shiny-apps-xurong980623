#' Run the EV Sales Shiny App
#'
#' This function launches the EV Sales shiny application from the package.
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_bar labs theme_minimal
#' @importFrom plotly ggplotly
#' @export
run_app <- function() {
  app_dir <- system.file("evsales-app", package = "EVsales")
  shiny::runApp(app_dir, display.mode = "normal")
}
