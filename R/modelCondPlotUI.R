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
  tabsetPanel(
    tabPanel("Model Results",
  tagList(
    htmlOutput(ns("form")),
    h4("Fixed Effects"),
    tableOutput(ns("modelStatsFixed")),
    h4("Random Effects"),
    tableOutput(ns("modelStatsRandom"))
    )
    ),
    tabPanel("Plot",
    plotOutput(ns("modelCondPlot"))
    )
  )
}

