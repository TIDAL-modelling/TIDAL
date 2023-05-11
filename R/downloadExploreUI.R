#' UI to download results from explore data page
#'
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
downloadExploreUI <- function(id, label = "data") {
  ns <- NS(id)
  tagList(
    p("When you have run your model, please click below to generate a pdf pop out with the descriptive statistics, model results and plot. Then save this pdf to your computer."),
    br(),
    downloadButton(ns("downloadReport"), "Open summary report")
  )
}
