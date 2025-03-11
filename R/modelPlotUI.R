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
    p("Plot of Model: (Continuous covariates are averaged across the values of that covariate within the sample. Categorical covariates are set to their lowest level by default.)"),
    checkboxInput(ns("plotCheckbox"), "Do you want an overlay of the descriptive plot?", TRUE, width = '100%'),
    withSpinner(plotOutput(ns("mainPlot")), proxy.height = "100px")
  )
}

