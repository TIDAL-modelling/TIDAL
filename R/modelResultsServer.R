#' Run model - data exploration page
#'
#' @import broom.mixed
#' @import lme4
#' @import dplyr
#' @import ggplot2
#' @import data.table
#' @import shinyjs
#' @import tidyr
#' @import magrittr
#' @import stringr
#'
#' @noRd
#' @keywords internal
#' @export
modelResultsServer <- function(id,
                           modelFit
) {

  moduleServer(
    id,
    function(input, output, session) {

      # ------------------------------------------
      # paste the formula text
      output$formulaText <- renderText({
        paste0("<b>Model Formula:</b> ",  gsub(".*formula = (.+) , data =.*", "\\1", summary(modelFit())$call)[2])
      })

      # ------------------------------------------
      # model results
      output$modelStatsFixed <- renderTable(
        tidy(modelFit(), "fixed")
      )

      output$modelStatsRandom <- renderTable(
        VarCorr(modelFit())
      )
    }
  )
}

