#' Plot model - data exploration page
#'
#' @import broom.mixed
#' @import lme4
#' @import dplyr
#' @import ggplot2
#' @import data.table
#' @import shinyjs
#' @import tidyr
#' @import shinycssloaders
#'
#' @noRd
#' @keywords internal
#' @export
modelPlotUI <- function(id, label = "Model Plot") {
  ns <- NS(id)

  tagList(
    p("Plot of model (note covariates are set to 0)"),
    withSpinner(plotOutput(ns("mainPlot")), proxy.height = "100px")
  )
}

