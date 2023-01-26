#' Run model
#'
#' @noRd
#' @keywords internal
#' @export
modelRunUI <- function(id, label = "Model") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  tagList(
    htmlOutput(ns("formulaText")),
    br(),
    tableOutput(ns("desc")),
    tableOutput(ns("modelStatsFixed")),
    tableOutput(ns("modelStatsRandom"))
  )
}

