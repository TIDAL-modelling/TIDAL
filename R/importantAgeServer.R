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
                               modelType,
                               modelFit) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)



      output$peakVelText <- renderText({
        if(modelType() == "Linear"){
        "Age at peak velocity cannot be calculated for linear models."
        } else if(modelType() == "Quadratic"){
        "Age at peak velocity cannot be calculated for quadratic models."
        } else if(modelType() == "Cubic"){

        } else if(modelType() == "Quartic"){

        }
      })

      output$peakVelPlot <- renderPlot({
        if(modelType() == "Cubic"){

        } else if(modelType() == "Quartic"){

        }
      })

      maxSymptomsList <- reactive({
        b_intercept <- tidy(modelFit(), "fixed") %>%
          filter(term %in% "(Intercept)") %>%
          pull(estimate)

        b_age <- tidy(modelFit(), "fixed") %>%
          filter(term %in% "age") %>%
          pull(estimate)

        b_age_2 <- tidy(modelFit(), "fixed") %>%
          filter(term %in% "I(age^2)") %>%
          pull(estimate)

        # Calculate discriminant
        discriminant <- b_age^2 - 4 * b_age_2

        # Check if discriminant is negative
        if (discriminant < 0) {
          return(NA)
        } else {
          # Calculate age at maximum symptoms
          maxSymptoms <- -b_age / (2 * b_age_2) + mean(dataLong$age_original)
          # Depression score at that age:
          maxSymY <- b_intercept +
            b_age*(maxSym-mean(dataLong$age_original)) +
            b_age_2*((maxSym-mean(dataLong$age_original))^2)
          return(list(maxSymptoms = maxSymptoms,
                      maxSymY = maxSymY))
        }
      })

      output$maxSymText <- renderText({
        if(modelType() == "Linear"){
        "Age at Maximum Symptoms cannot be calculated for linear models."
        } else if(modelType() == "Quadratic"){
          # req(maxSymptomsList())
          if(is.na(maxSymptomsList())){
            "No real roots. Age at maximum symptoms cannot be determined."
          }else{
            # paste0("Age at maximum symptoms: ", round(maxSymptomsList()$maxSymptoms, 2))
            paste0(maxSymptomsList())
          }

        } else if(modelType() == "Cubic"){

        } else if(modelType() == "Quartic"){

        }
      })



      output$maxSymPlot <- renderPlot({
        if(modelType() == "Quadratic"){

        } else if(modelType() == "Cubic"){

        } else if(modelType() == "Quartic"){

        }
      })

    }
  )
}

