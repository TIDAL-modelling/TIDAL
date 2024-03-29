#' Plot model - data exploration page
#'
#' @import broom.mixed
#' @import lme4
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @import shinycssloaders
#'
#' @keywords internal
#' @export
modelPlotUI <- function(id, label = "Model Plot") {
  ns <- NS(id)

  tagList(
    p("Plot of model (note covariates are set to 0)"),
    checkboxInput(ns("plotCheckbox"), "Do you want an overlay of the descriptive plot?", TRUE, width = '100%'),
    withSpinner(plotOutput(ns("mainPlot")), proxy.height = "100px")
  )
}

