#' Show fixed and random effects
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
modelResultsUI <- function(id, label = "Model") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  tagList(
    htmlOutput(ns("formulaText")),
    p("The time variable (eg. age) has been mean centred, to aid model convergence."),
    br(),
    htmlOutput(ns("warning")),
    br(),
    h4("Number of observations and groups"),
    textOutput(ns("Ndims")),
    h4("Fixed Effects"),
    tableOutput(ns("modelStatsFixed")),
    h4("Random Effects"),
    tableOutput(ns("modelStatsRandom"))
    )
}

