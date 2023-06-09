#' UI to download results from interaction variable page
#' @import broom.mixed
#' @import lme4
#' @import dplyr
#' @import ggplot2
#' @import data.table
#' @import shinyjs
#' @import tidyr
#'
#' @noRd
#' @keywords internal
#' @export
downloadCondUI <- function(id, label = "data") {
  ns <- NS(id)
  tagList(
    p("When you have run the model with your interaction variable of choice, please download a pdf report with the descriptive statistics, model results and plot."),
    p('Note the download button will only appear once you have clicked "Run Model" in the side panel.'),
    br(),
    uiOutput(ns("buttonHere"))
  )
}
