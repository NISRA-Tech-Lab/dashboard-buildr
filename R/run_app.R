#' Run Dashboard BuildR
#'
#' Launch the Dashboard BuildR Shiny application in the default web browser.
#'
#' @return No return value. Launches the Shiny application.
#' @export
run_dashboard_buildr <- function() {
  
  shiny::addResourcePath(
    "buildr-assets",
    system.file(
      "www",
      package = "dashboardBuildR"
    )
  )
  
  app <- shiny::shinyApp(
    ui = app_ui(),
    server = app_server
  )
  
  shiny::runApp(
    app,
    launch.browser = TRUE
  )
}