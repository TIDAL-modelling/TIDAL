#' Run Shiny app from local R environment
#'
#' Opens an interactive Shiny GUI
#'
#' "display" can be either "default" which opens as a pop out window in R studio or "browser" which opens in the browser.
#'
#'
#' @import shiny
#' @import shinythemes
#'
#' @export
launchTIDAL <- function(display = "default") {
  appDir <- system.file("TIDALapp", package = "TIDAL")

  if (appDir == "") {
    stop("Could not find directory. Try re-installing `TIDAL`.", call. = FALSE)
  } else {
    if (display == "default") {
      shiny::runApp(paste0(appDir, "/app.R"), display.mode = "normal")
    } else if (display == "browser") {
      shiny::runApp(paste0(appDir, "/app.R"), launch.browser = TRUE)
    } else {
      stop("Invalid value for 'display'. Please choose either 'default' or 'browser'.", call. = FALSE)
    }
  }
}
