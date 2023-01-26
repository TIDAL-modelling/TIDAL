#' Choosing variables from a drop down menu of the column names loaded in the selectData module
#'
#' @noRd
#' @keywords internal
#' @export
varsSelectUI <- function(id, label = "Variables Selected") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  tagList(
    selectInput(ns("ID"), "Participant ID variable:", choices = c()),
    selectInput(ns("traj"), "Variable to model trajectory on, eg. depression scores:", choices =  c()),
    selectInput(ns("age"), "Variable for age:", choices =  c()),
    # selectInput(ns("covars"), "Select any covariates to use in the model", choices =  c(), multiple = TRUE),
    selectInput(ns("modelType"), "Model Type:", choices = c("Linear", "Quadratic", "Cubic", "Quartic"))
  )
}
