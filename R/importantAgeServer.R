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
importantAgeServer <- function(id,
                               modelDataEdit,
                               modelType) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      output$peakVelText <- renderText({
        hideElement(selector = "#sidebar")
        if(modelType() == "Linear"){
        "Age at peak velocity cannot be calculated for linear models."
        } else if(modelType() == "Quadratic"){

        } else if(modelType() == "Cubic"){

        } else if(modelType() == "Quartic"){

        }
      })

      output$peakVelPlot <- renderPlot({
        hideElement(selector = "#sidebar")
        if(modelType() == "Linear"){

        } else if(modelType() == "Quadratic"){

        } else if(modelType() == "Cubic"){

        } else if(modelType() == "Quartic"){

        }
      })

      output$maxSymText <- renderText({
        hideElement(selector = "#sidebar")
        if(modelType() == "Linear"){
        "Age at Maximum Symptoms cannot be calculated for linear models."
        } else if(modelType() == "Quadratic"){

        } else if(modelType() == "Cubic"){

        } else if(modelType() == "Quartic"){

        }
      })

      output$maxSymPlot <- renderPlot({
        hideElement(selector = "#sidebar")
        if(modelType() == "Linear"){

        } else if(modelType() == "Quadratic"){

        } else if(modelType() == "Cubic"){

        } else if(modelType() == "Quartic"){

        }
      })

    }
  )
}

