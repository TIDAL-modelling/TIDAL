#' Selecting and reading in the dataset
#'
#' @import broom.mixed
#' @import lme4
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#'
#' @keywords internal
#' @export
selectDataUI <- function(id, label = "data") {
  ns <- NS(id)
  tagList(
    selectInput(ns("select"),
                "Select a dataset:",
                c("Upload a long format dataset", "Data formatted on previous page")),
    uiOutput(ns("uploadFile")),
    uiOutput(ns("additional")),
    uiOutput(ns("randomFX_UI")),
    uiOutput(ns("weights_UI")),
    uiOutput(ns("weightCol_UI")),
    uiOutput(ns("button_UI"))
  )
}
