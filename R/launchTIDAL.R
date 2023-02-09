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
launchTIDAL <- function() {
  appDir <- system.file("TIDALapp", package = "TIDAL")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `TIDAL`.", call. = FALSE)
  }

  shiny::runApp(paste0(appDir, "/app.R") , display.mode = "normal")
}

