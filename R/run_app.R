#' Launch Penguin Species Explorer
#'
#' Launch the interactive Shiny application.
#'
#' @return Opens the Shiny application.
#' @export
run_app <- function() {
  
  app_dir <- system.file(
    "shiny",
    package = "penguinExplorer",
    mustWork = TRUE
  )
  
  shiny::runApp(app_dir)
}