#' Split model by variable
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
modelCondPlotUI <- function(id, label = "Model Condition Run") {
  ns <- NS(id)

  tagList(
    htmlOutput(ns("form")),
    plotOutput(ns("modelCondPlot"))
    )
}

