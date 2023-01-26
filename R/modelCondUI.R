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
modelCondUI <- function(id, label = "Model Condition Run") {
  ns <- NS(id)

  tagList(
    selectInput(ns("condition"), "Select the condition to split trajectory on.", choices = c("CRP_quartile", "IL6_quartile")),
    plotOutput(ns("modelCondPlot")))
}

