#' Run model
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
modelRunUI <- function(id, label = "Model") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  tagList(
    htmlOutput(ns("covChoiceWarning")),
    p("Descriptive statistics of your variable of interest, eg. depression scores/height/income, for each time point."),
    tableOutput(ns("desc")),
    p('Plot the mean scores of your variable of interest by the time/age variable.
       Have a look to see what type of model you think best fits your data. Use the "Plot" tab to overlay the model on top.'),
    withSpinner(plotOutput(ns("plot")), proxy.height = "100px")
    )
}

