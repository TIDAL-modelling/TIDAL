#' UI for individual trajectories
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
importantAgeUI <- function(id, label = "Model") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  tagList(
          h4("Age at Peak Velocity"),
          textOutput(ns("peakVelText")),
          plotOutput(ns("peakVelPlot")),
          h4("Age at Maximum Symptoms"),
          textOutput(ns("maxSymText")),
          plotOutput(ns("maxSymPlot"))
         )
}


