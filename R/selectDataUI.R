#' Selecting and reading in the dataset
#'
#' @noRd
#' @keywords internal
#' @export
selectDataUI <- function(id, label = "data") {
  ns <- NS(id)
  tagList(
    selectInput(ns("select"), "Select a dataset:", c("Upload a long format dataset", "Data formatted on previous page") ),
    uiOutput(ns("uploadFile"))
  )
}
