#' Run Dashboard BuildR
#'
#' Launch the Dashboard BuildR Shiny application in the default web browser.
#'
#' @return No return value. Launches the Shiny application.
#' @export
run_dashboard_buildr <- function() {
  
  if (Sys.getenv("RSTUDIO") == "1") {
    
    cat(
      "\n\nLaunching NISRA Dashboard BuildR in Browser...\n\n",
      "Press Esc to quit\n"
    )
    
  } else {
    
    cat(
      "\n\nLaunching NISRA Dashboard BuildR in Browser...\n\n",
      "Press Ctrl + C to quit\n"
    )
  }
  
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