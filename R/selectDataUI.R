#' Selecting and reading in the dataset
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
selectDataUI <- function(id, label = "data") {
  ns <- NS(id)
  tagList(
    selectInput(ns("select"), "Select a dataset:", c("Upload a long format dataset", "Data formatted on previous page") ),
    uiOutput(ns("uploadFile")),
    selectInput(ns("ID"), "Participant ID variable:", choices = c()),
    selectInput(ns("traj"), "Variable to model trajectory on, eg. depression scores (continuous):", choices =  c()),
    selectInput(ns("age"), "Variable for age at time point (continous):", choices =  c()),
    selectInput(ns("timePoint"), "Variable for time point (categorical):", choices =  c()),
    selectInput(ns("modelType"), "Model Type:", choices = c("Linear", "Quadratic", "Cubic", "Quartic"))#,
    # actionButton(ns("button"), "Run Model")
  )
}
