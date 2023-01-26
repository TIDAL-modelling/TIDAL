#' Plot model - data exploration page
#'
#' @noRd
#' @keywords internal
#' @export
modelPlotUI <- function(id, label = "Model Plot") {
  ns <- NS(id)

  tagList(
    plotOutput(ns("mainPlot"))
  )
}

