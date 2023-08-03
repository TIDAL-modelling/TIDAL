#' Show fixed and random effects
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
modelResultsUI <- function(id, label = "Model") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  tagList(
    withSpinner(htmlOutput(ns("formulaText")), proxy.height = "100px"),
    br(),
    htmlOutput(ns("warning")),
    br(),
    h4("Number of observations and groups"),
    textOutput(ns("Ndims")),
    br(),
    h4("Fixed Effects"),
    withSpinner(tableOutput(ns("modelStatsFixed")), proxy.height = "100px"),
    withSpinner(htmlOutput(ns("interFixed")), proxy.height = "100px"),
    h4("Random Effects"),
    withSpinner(tableOutput(ns("modelStatsRandom")), proxy.height = "100px"),
    withSpinner(htmlOutput(ns("interRandom")), proxy.height = "100px")
    )
}

