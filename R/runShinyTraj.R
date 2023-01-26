#' Run Shiny app from local R environment
#'
#' Opens an interactive Shiny GUI
#'
#'
#'
#' @import shiny
#' @import shinythemes
#'
#' @export
runShinyTraj <- function() {
  appDir <- system.file("shinyApp", package = "trajMods")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `trajMods`.", call. = FALSE)
  }

  shiny::runApp(paste0(appDir, "/app.R") , display.mode = "normal")
}

