#' UI to download results from explore data page
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
downloadExploreUI <- function(id, label = "data") {
  ns <- NS(id)
  tagList(
    downloadButton(ns("downloadReport"), "Download Results")
  )
}
