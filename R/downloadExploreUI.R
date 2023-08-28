#' UI to download results from explore data page
#'
#'
#' @keywords internal
#' @export
downloadExploreUI <- function(id, label = "data") {
  ns <- NS(id)
  tagList(
    p("When you have run the model, please download a pdf report with the descriptive statistics, model results and plot."),
    p('Note the download button will only appear once you have clicked "Run Model" in the side panel.'),
    br(),
    uiOutput(ns("buttonHere"))
  )
}
