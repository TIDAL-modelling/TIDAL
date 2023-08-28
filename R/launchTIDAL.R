#' Run Shiny app from local R environment
#'
#' Opens an interactive Shiny GUI.
#'
#' @param display Character vector. Can be either "default" or "browser". Default is "default".
#'   "default" opens the Shiny app in a pop-out window in RStudio, while "browser" opens it in a web browser.
#'
#' @import shiny
#' @import shinythemes
#'
#' @export
#' @examples
#' \dontrun{# Launch Shiny app with default display
#' launchTIDAL()
#'
#' # Launch Shiny app with browser display
#' launchTIDAL(display = "browser")
#' }
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
