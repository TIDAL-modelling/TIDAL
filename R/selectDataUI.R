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
    selectInput(ns("select"),
                "Select a dataset:",
                c("Upload a long format dataset", "Data formatted on previous page")),
    uiOutput(ns("uploadFile")),
    tableOutput(ns("test")),
    uiOutput(ns("additional"))
  )
}
