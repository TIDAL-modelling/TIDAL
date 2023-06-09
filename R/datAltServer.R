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
datExAltServer <- function(id,
                           modelDataEdit,
                           modelFit,
                           modelType

) {

  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)

      # ------------------------------------------
      # Allow the user to select the ages they want to calculate scores for
      output$selectAge <- renderUI({
        ageOrig <- modelDataEdit() %>%
                          pull(age_original)
        ageOrig <- ageOrig[!is.na(ageOrig)]
        checkboxGroupInput(ns("ageInput"),
                           "What ages do you want to calculate scores for?",
                           seq(round(min(ageOrig)),round(max(ageOrig))),
                           inline = TRUE)
      })

      # ------------------------------------------
      # Calculate the score at a given age (intercept + slope etc)
      score <- reactive({
        ageOrig <- modelDataEdit() %>% pull(age_original)
        ageOrig <- ageOrig[!is.na(ageOrig)]

        score <- sapply(as.numeric(input$ageInput), function(x){
          if(modelType() == "Linear"){
            (summary(modelFit())$coefficients[1,1] +
            (x - mean(ageOrig)) * summary(modelFit())$coefficients[2,1]) %>%
              round(2)
          } else if(modelType() == "Quadratic"){
           ( summary(modelFit())$coefficients[1,1] +
              (x - mean(ageOrig)) * summary(modelFit())$coefficients[2,1] +
              (x - mean(ageOrig))^2 * summary(modelFit())$coefficients[3,1]) %>%
              round(2)
          } else if(modelType() == "Cubic"){
            (summary(modelFit())$coefficients[1,1] +
              (x - mean(ageOrig)) * summary(modelFit())$coefficients[2,1] +
              (x - mean(ageOrig))^2 * summary(modelFit())$coefficients[3,1] +
            (x - mean(ageOrig))^3 * summary(modelFit())$coefficients[4,1] )%>%
              round(2)
          } else if(modelType() == "Quartic"){
           ( summary(modelFit())$coefficients[1,1] +
              (x - mean(ageOrig)) * summary(modelFit())$coefficients[2,1] +
              (x - mean(ageOrig))^2 * summary(modelFit())$coefficients[3,1] +
            (x - mean(ageOrig))^3 * summary(modelFit())$coefficients[4,1] +
            (x - mean(ageOrig))^4 * summary(modelFit())$coefficients[5,1]) %>%
              round(2)
          }
        })
        return(score)
      })

      # ------------------------------------------
      # Plot the score at the given age
      output$plot <- renderPlot({
        ggplot() +
          geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred ) , na.rm=T) +
          geom_vline(xintercept = as.numeric(input$ageInput), color = "red", linetype = "dashed") +
          geom_hline(yintercept = score(), color = "red", linetype = "dashed") +
          ylab("Score") +
          xlab("Age")
      })

      # ------------------------------------------
      # Return a table of the score for all the ages
      # --- Age | Score -----
      # Change "Score" to the actual column name from the dataframe - which the user previously specified
      output$table <- renderTable({
        data.frame(Age = input$ageInput,
                   Score = score())
      })


      # ------------------------------------------
    }
  )
}

