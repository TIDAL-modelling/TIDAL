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

          # Age at increased velocity (peak velocity)
          velPeak <- -2 * b_age_2 / (3 * b_age_3) + mean(pull(modelDataEdit(), age_original))
          velMin <- 2 * b_age_2 / (3 * b_age_3) + mean(pull(modelDataEdit(), age_original))

          # Velocity as a new col
          vel <- b_age +
            ( 2 * b_age_2 * pull(modelDataEdit(), !!sym(age())) ) +
            ( 3 * b_age_3 * (pull(modelDataEdit(), !!sym(age())))^2 )

          modelDataEdit2 <- modelDataEdit() %>%
            mutate(vel = vel)

          # Age at maximum symptoms
          maxSymptoms <- (-2*b_age_2 - (sqrt(4*(b_age_2^2) - (12*(b_age_3*b_age))))) / (6*b_age_3) + mean(pull(modelDataEdit(), age_original))

          # Depression score at that age:
          maxSymY <- b_intercept +
            b_age*(maxSymptoms-mean(pull(modelDataEdit(), age_original))) +
            b_age_2*((maxSymptoms-mean(pull(modelDataEdit(), age_original)))^2) +
            b_age_3*((maxSymptoms-mean(pull(modelDataEdit(), age_original)))^3)

          return(list(
            velPeak = velPeak,
            velMin = velMin,
            modelDataEdit2 = modelDataEdit2,
            maxSymptoms = maxSymptoms,
            maxSymY = maxSymY
          ))

        } else if(modelType() == "Quartic"){
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

          b_age_4 <- tidy(modelFit(), "fixed") %>%
            filter(term %in% paste0("I(",age(),"^4)")) %>%
            pull(estimate)

          # Age at increased velocity (peak velocity)
          velPeak <- -6 * b_age_3 - sqrt((6 * b_age_3)^2 - 4 * (12 * b_age_4) * (2 * b_age_2)) / (24 * b_age_4) + mean(pull(modelDataEdit(), age_original))

          # Age at decreased velocity
          velMin <- -6 * b_age_3 + sqrt((6 * b_age_3)^2 - 4 * (12 * b_age_4) * (2 * b_age_2)) / (24 * b_age_4) + mean(pull(modelDataEdit(), age_original))

          # Velocity as a new col
            vel <- b_age +
            ( 2 * b_age_2 * pull(modelDataEdit(), !!sym(age())) ) +
            ( 3 * b_age_3 * (pull(modelDataEdit(), !!sym(age())))^2 ) +
            ( 4 * b_age_4 * (pull(modelDataEdit(), !!sym(age())))^3 )

          modelDataEdit2 <- modelDataEdit() %>%
              mutate(vel = vel)

          return(list(
            velPeak = velPeak,
            velMin = velMin,
            modelDataEdit2 = modelDataEdit2
          ))
        }

      })

      output$peakVelText <- renderText({
        req(modelType())
        if(modelType() == "Linear"){
        "Age at peak velocity cannot be calculated for linear models."
        } else if(modelType() == "Quadratic"){
        "Age at peak velocity cannot be calculated for quadratic models."
        } else if(modelType() == "Cubic"){
          range <- modelDataEdit() %>%
            pull(!!sym(age()))
          if(valuesList()$velPeak > max(range) | valuesList()$velMin < min(range)){
            "The peak velocities are outside the data range and therefore cannot be estimated for this model type."
          }else{
            paste0("Age at peak velocity: ", round(valuesList()$velMin, 2), " and ", round(valuesList()$velPeak, 2))
          }
        } else if(modelType() == "Quartic"){
          paste0("Age at peak velocity: ", round(valuesList()$velMin, 2), " and ", round(valuesList()$velPeak, 2))
        }
      })

      output$peakVelPlot <- renderPlot({
        req(modelType())
        if(modelType() == "Cubic" | modelType() == "Quartic"){
          ggplot(modelDataEdit()) +
            geom_line(aes(x= age_original ,  y = pred), na.rm=T, linewidth = 0.75) +
            geom_vline(xintercept = valuesList()$velMin, colour="red", linetype = "longdash") +
            geom_vline(xintercept = valuesList()$velPeak, colour="red", linetype = "longdash") +
            theme(text = element_text(size = 20), panel.background = element_rect(fill = "white"),
                  axis.line = element_line(colour = "grey50")) +
            ylab("Score") +
            xlab("Age")
        }
      })

      output$velPlot <- renderPlot({
        req(modelType())
        if(modelType() == "Cubic" | modelType() == "Quartic"){
        ggplot(valuesList()$modelDataEdit2) +
          geom_line(aes(x= age_original, y = vel), color = "blue") +
          geom_vline(xintercept =  valuesList()$velMin, colour="red", linetype = "longdash") +
          geom_vline(xintercept = valuesList()$velPeak, colour="red", linetype = "longdash") +
          theme(text = element_text(size = 20), panel.background = element_rect(fill = "white"),
                axis.line = element_line(colour = "grey50")) +
          ylab("Velocity") +
          xlab("Age")
        }
      })


      output$maxSymText <- renderText({
        req(modelType())
        if(modelType() == "Linear"){
        "Age at maximum symptoms cannot be calculated for linear models."
        } else if(modelType() == "Quadratic"){
          if(is.na(valuesList()$maxSymptoms)){
            "No real roots. Age at maximum symptoms cannot be determined."
          }else{
            paste0("Age at maximum symptoms: ", round(valuesList()$maxSymptoms, 2))
          }
        } else if(modelType() == "Cubic"){
          paste0("Age at maximum symptoms: ", round(valuesList()$maxSymptoms, 2))
        } else if(modelType() == "Quartic"){
          "Calculating age at maximum symptoms for quartic models is still in development."
        }
      })

      output$maxSymPlot <- renderPlot({
        req(modelType())
        if(modelType() == "Quadratic" | modelType() == "Cubic"){
          ggplot(modelDataEdit()) +
            geom_line(aes(x= age_original ,  y = pred), na.rm=T, linewidth = 0.75) +
            geom_hline(yintercept = valuesList()$maxSymY, colour="red", linetype = "longdash") +
            geom_vline(xintercept = valuesList()$maxSymptoms, colour="red", linetype = "longdash") +
            theme(text = element_text(size = 20), panel.background = element_rect(fill = "white"),
                  axis.line = element_line(colour = "grey50")) +
            ylab("Score") +
            xlab("Age")
        }
      })

    }
  )
}

