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
                               modelFit,
                               age) {

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
          filter(term %in% age()) %>%
          pull(estimate)

        b_age_2 <- tidy(modelFit(), "fixed") %>%
          filter(term %in% paste0("I(",age(),"^2)")) %>%
          pull(estimate)

        # Calculate discriminant
        discriminant <- b_age^2 - 4 * b_age_2

        # Check if discriminant is negative
        if (discriminant < 0) {
          return(list(maxSymptoms = NA,
                      maxSymY = NA))
        } else {
          # Calculate age at maximum symptoms
          maxSymptoms <- -b_age / (2 * b_age_2) + mean(pull(modelDataEdit(), age_original))
          # Score at that age:
          maxSymY <- b_intercept +
            b_age*(maxSymptoms-mean(pull(modelDataEdit(), age_original))) +
            b_age_2*((maxSymptoms-mean(pull(modelDataEdit(), age_original)))^2)
          return(list(maxSymptoms = maxSymptoms,
                      maxSymY = maxSymY))
        }
      })

      output$maxSymText <- renderText({
        if(modelType() == "Linear"){
        "Age at Maximum Symptoms cannot be calculated for linear models."
        } else if(modelType() == "Quadratic"){
          # req(maxSymptomsList())
          if(is.na(maxSymptomsList()$maxSymptoms)){
            "No real roots. Age at maximum symptoms cannot be determined."
          }else{
            paste0("Age at maximum symptoms: ", round(maxSymptomsList()$maxSymptoms, 2))
          }

        } else if(modelType() == "Cubic"){

        } else if(modelType() == "Quartic"){

        }
      })

      output$maxSymPlot <- renderPlot({
        if(modelType() == "Quadratic"){
          ggplot(modelDataEdit()) +
            geom_line(aes(x= age_original ,  y = pred), na.rm=T, linewidth = 0.75) +
            geom_hline(yintercept = maxSymptomsList()$maxSymY, colour="red", linetype = "longdash") +
            geom_vline(xintercept = maxSymptomsList()$maxSymptoms, colour="red", linetype = "longdash") +
            theme(text = element_text(size = 20), panel.background = element_rect(fill = "white"),
                  axis.line = element_line(colour = "grey50"))

        } else if(modelType() == "Cubic"){

        } else if(modelType() == "Quartic"){

        }
      })

    }
  )
}

