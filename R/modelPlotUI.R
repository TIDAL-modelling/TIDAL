#' Plot model - data exploration page
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
modelPlotUI <- function(id, label = "Model Plot") {
  ns <- NS(id)

  tagList(
    plotOutput(ns("mainPlot")),
    downloadButton(ns("downloadReport"), "Download Results")
  )
}

