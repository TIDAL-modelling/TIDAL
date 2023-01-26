#' Split model by variable
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

