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
        updateSelectInput(
          session,
          "condition",
          choices = names(modelData())
        )
      })


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
        covarLogical = reactive({input$covarsLogical})
      )
      )
    }
  )
}

