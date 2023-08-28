#' Display score at an age for the data exploration page
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
datExAltUI <- function(id, label = "Model") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  tagList(
    uiOutput(ns("selectAge")),
    withSpinner(plotOutput(ns("plot")), proxy.height = "100px"),
    tableOutput(ns("table"))
  )
}

