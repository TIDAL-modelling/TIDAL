#' split model by variable
#'
#' @import broom.mixed
#' @import lme4
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#'
#' @keywords internal
#' @name importantAge
utils::globalVariables(c("term", "estimate", "pred"))
importantAgeServer <- function(id,
                               modelDataEdit,
                               modelType,
                               modelFit,
                               age) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      valuesList <- reactive({
        req(modelFit())
        if(modelType() == "Quadratic"){
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
        }else if(modelType() == "Cubic"){
          b_intercept <- tidy(modelFit(), "fixed") %>%
            filter(term %in% "(Intercept)") %>%
            pull(estimate)

          b_age <- tidy(modelFit(), "fixed") %>%
            filter(term %in% age()) %>%
            pull(estimate)

          b_age_2 <- tidy(modelFit(), "fixed") %>%
            filter(term %in% paste0("I(",age(),"^2)")) %>%
            pull(estimate)

          b_age_3 <- tidy(modelFit(), "fixed") %>%
            filter(term %in% paste0("I(",age(),"^3)")) %>%
            pull(estimate)

          # Age at maximum symptoms
          maxSymptoms <- (-2*b_age_2 - (sqrt(4*(b_age_2^2) - (12*(b_age_3*b_age))))) / (6*b_age_3) + mean(pull(modelDataEdit(), age_original))

          # Depression score at that age:
          maxSymY <- b_intercept +
            b_age*(maxSymptoms-mean(pull(modelDataEdit(), age_original))) +
            b_age_2*((maxSymptoms-mean(pull(modelDataEdit(), age_original)))^2) +
            b_age_3*((maxSymptoms-mean(pull(modelDataEdit(), age_original)))^3)

          return(list(
            maxSymptoms = maxSymptoms,
            maxSymY = maxSymY
          ))

        }
      })

      output$maxSymText <- renderText({
        req(modelType())
        if(modelType() == "Linear"){
        "Age at maximum symptoms cannot be calculated for linear models."
        } else if(modelType() == "Quadratic" | modelType() == "Cubic"){
          if(is.na(valuesList()$maxSymptoms)){
            "No real roots. Age at maximum symptoms cannot be determined."
          }else{
            paste0("Age at maximum symptoms: ", round(valuesList()$maxSymptoms, 2))
          }
        } else if(modelType() == "Quartic"){
          "Calculating age at maximum symptoms for quartic models is still in development."
        }
      })

      output$maxSymPlot <- renderPlot({
        req(modelType())
        if(modelType() == "Quadratic" | modelType() == "Cubic"){
          if(is.na(valuesList()$maxSymptoms)){
            ggplot() + theme_void()
          }else{
            ggplot(modelDataEdit()) +
              geom_line(aes(x= age_original ,  y = pred), na.rm=T, linewidth = 0.75) +
              geom_hline(yintercept = valuesList()$maxSymY, colour="red", linetype = "longdash") +
              geom_vline(xintercept = valuesList()$maxSymptoms, colour="red", linetype = "longdash") +
              theme(text = element_text(size = 20), panel.background = element_rect(fill = "white"),
                    axis.line = element_line(colour = "grey50")) +
              ylab("Score") +
              xlab("Age")
          }
        }
      })
    }
  )
}

