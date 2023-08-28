#' UI for individual trajectories
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
importantAgeUI <- function(id, label = "Model") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  tabPanel("Output",
  fluidRow(column(12, h4("Age at Maximum Symptoms"))),
  fluidRow(column(12, textOutput(ns("maxSymText")))),
  fluidRow(column(12, align="center", withSpinner(plotOutput(ns("maxSymPlot") , width = "50%", height = 300), proxy.height = "100px")))
  )

}


