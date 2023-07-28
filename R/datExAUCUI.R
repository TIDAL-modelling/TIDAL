#' AUC for the data exploration page
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
datExAUCUI <- function(id, label = "Model") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  tagList(
    textOutput(ns("AUCoverview")),
    p(""),
    p(""),
    uiOutput(ns("AUCagesUI")),
    withSpinner(plotOutput(ns("AUCplot")), proxy.height = "100px"),
    withSpinner(tableOutput(ns("AUCtable")), proxy.height = "100px")
  )

}

