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
#'
#' You can also manually open a browser and copy and paste the URL after "Listening on" into the address bar. Eg. for "Listening on http://127.0.0.1:4484" copy and paste "http://127.0.0.1:4484" into your browser, eg. in Chrome or Firefox. Even though you are using a browser the app is still running locally on your computer, the same as if the pop-out window was launched from RStudio.
#'
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
