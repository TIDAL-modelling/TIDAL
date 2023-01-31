#' split model by variable
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
modelCondServer <- function(id,
                            modelData) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      observeEvent(modelData(),{

        # allow the user to select a variable from the column names of the dataset
        # critically this should be categorical variables (for the plot to split by a factor)
        # hacky way to choose a categorical variable is below
        # user can only select columns with unique values of length < 40. 40 being an arbitary number
        # but any more than this may not be very useful or visible on a plot, 40 is already a lot.
        updateSelectInput(
          session,
          "condition",
          choices = colnames(modelData())[apply(modelData(), 2, function(x) length(unique(x))) < 40]
        )
      })

      # Select the covariates based on col names, user can choose anything - not like above.
      output$covarsOpt <- renderUI({
        if(isTRUE(input$covarsLogical)){
            selectInput(ns("covariates"), "Select covariates:", choices = colnames(modelData()), multiple = TRUE )
        } else {
          p()
          }
        })
      return(list(
        condition = reactive({input$condition}),
        covariates = reactive({input$covariates}),
        covarsLogical = reactive({input$covarsLogical})
      )
      )
    }
  )
}

