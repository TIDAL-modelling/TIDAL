#' Choosing variables from a drop down menu of the column names loaded in the selectData module
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
varsSelectServer <- function(id, varsSelectData) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)

      colVarUpdate <- function(colVar, i){
        observeEvent(varsSelectData(), {
          updateSelectInput(
            session,
            colVar,
            choices = names(varsSelectData()),
            selected = names(varsSelectData()[i])
          )
        })
      }

      varNames <- list("ID", "traj", "age")
      varIndex <- list(NULL, NULL, NULL)
      purrr::map2(varNames, varIndex, \(varNames, varIndex) colVarUpdate(varNames, varIndex))

      # special case for covariates that can have nothing selected (figure out a better way for this)
      # observeEvent(varsSelectData(), {
      #   updateSelectInput(
      #     session,
      #     "covars",
      #     choices = c(" ", names(varsSelectData())),
      #     selected = " "
      #   )
      # })


      modelForm <- reactive({
        if(input$modelType == "Linear"){
          paste0(input$traj," ~ ", input$age, " + ", "(", input$age, "|" , input$ID, ")")
        } else if(input$modelType == "Quadratic"){
          paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2) + (", input$age, "|" , input$ID, ") + (I(",input$age, "^2)|" , input$ID, ")" )
        } else if(input$modelType == "Cubic"){
          paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2)", " + I(", input$age   ,"^3)" ," + (", input$age, "|" , input$ID, ") + (I(",input$age, "^2)|" , input$ID, ")")
        } else if(input$modelType == "Quartic"){
          paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2)", " + I(", input$age   ,"^3)" , " + I(", input$age   ,"^4)" ," + (", input$age, "|" , input$ID, ") + (I(",input$age, "^2)|" , input$ID, ")")
        }
      })


      return(
        list(
        modelForm = modelForm,
        ID = reactive({ input$ID }),
        traj = reactive({ input$traj }),
        age = reactive({ input$age }),
        modelType = reactive({ input$modelType })
        )
      )
    }
  )
}
