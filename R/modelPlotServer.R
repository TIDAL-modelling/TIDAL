#' Model plot - data exploration page
#'
#' @import broom.mixed
#' @import lme4
#' @import dplyr
#' @import ggplot2
#' @import data.table
#' @import shinyjs
#' @import tidyr
#' @import purrr
#'
#' @noRd
#' @keywords internal
#' @export
modelPlotServer <- function(id,
                            modelData,
                            modelFit,
                            age,
                            traj,
                            timePoint,
                            modelType,
                            button
                            ) {
  moduleServer(
    id,
    function(input, output, session) {

      # ------------------------------------------
      # add the "prediction"/model col to dataframe (adjustedScore)
      # also add the 95% CIs for these estimates, by subbing in beta ± 1.96*SE
      # in coef: column index 1 is estimate, column index 2 is SE

      modelDataEdit <- eventReactive(button(), {

        age <- modelData() %>% pull(!!age())

        coef <- summary(modelFit())$coefficients

        if(modelType() == "Linear"){

          adjustedScore <- age * coef[2,1] + coef[1,1]

          plus95 <- age * (coef[2,1] + 1.96*coef[2,2]) + (coef[1,1] + 1.96*coef[1,2])

          minus95 <- age * (coef[2,1] - 1.96*coef[2,2]) + (coef[1,1] - 1.96*coef[1,2])

        } else if(modelType() == "Quadratic"){

          adjustedScore <- age * coef[2,1] + coef[1,1] +
            age^2 * coef[3,1]

          plus95 <- age * (coef[2,1] + 1.96*coef[2,2]) + (coef[1,1] + 1.96*coef[1,2]) +
            age^2 * (coef[3,1] + 1.96*coef[3,2])

          minus95 <- age * (coef[2,1] - 1.96*coef[2,2]) + (coef[1,1] - 1.96*coef[1,2]) +
            age^2 * (coef[3,1] - 1.96*coef[3,2])

        } else if(modelType() == "Cubic"){
          adjustedScore <- age * coef[2,1] + coef[1,1]  +
            age^2 * coef[3,1] +
            age^3 * coef[4,1]

          plus95 <- age * (coef[2,1] + 1.96*coef[2,2]) + (coef[1,1] + 1.96*coef[1,2]) +
            age^2 * (coef[3,1] + 1.96*coef[3,2]) +
            age^3 * (coef[4,1] + 1.96*coef[4,2])

          minus95 <- age * (coef[2,1] - 1.96*coef[2,2]) + (coef[1,1] - 1.96*coef[1,2]) +
            age^2 * (coef[3,1] - 1.96*coef[3,2]) +
            age^3 * (coef[4,1] - 1.96*coef[4,2])

        } else if(modelType() == "Quartic"){
          adjustedScore <- age * coef[2,1] + coef[1,1]  +
            age^2 * coef[3,1] +
            age^3 * coef[4,1] +
            age^4 * coef[5,1]

          plus95 <- age * (coef[2,1] + 1.96*coef[2,2]) + (coef[1,1] + 1.96*coef[1,2]) +
            age^2 * (coef[3,1] + 1.96*coef[3,2]) +
            age^3 * (coef[4,1] + 1.96*coef[4,2]) +
            age^4 * (coef[5,1] + 1.96*coef[5,2])

          minus95 <-  age * (coef[2,1] - 1.96*coef[2,2]) + (coef[1,1] - 1.96*coef[1,2]) +
            age^2 * (coef[3,1] - 1.96*coef[3,2]) +
            age^3 * (coef[4,1] - 1.96*coef[4,2]) +
            age^4 * (coef[5,1] - 1.96*coef[5,2])

        }

        modelData() %>%
          mutate(pred = adjustedScore) %>%
          mutate(plus95 = plus95) %>%
          mutate(minus95 = minus95)
      })

      # ------------------------------------------
      # Get the mean and sd for depression scores at each time point
      df.plot <- reactive({
        modelData() %>%
          group_by(across( !!timePoint() )) %>%
          summarise(Age = mean(age_original, na.rm = T),
                    Phenotype = mean(!!sym(traj()), na.rm = T),
                    SD = sd(!!sym(traj()), na.rm = T),
                    n = sum(!is.na( !!sym(traj()) ))
          ) %>%
          mutate(upper = Phenotype + ( qnorm(0.975)*SD/sqrt(n) ),
                 lower = Phenotype - ( qnorm(0.975)*SD/sqrt(n) ))

      })

      # ------------------------------------------

      mainPlot <- eventReactive(c(input$plotCheckbox, modelFit()), {
        if(input$plotCheckbox == TRUE){
        ggplot() +
          geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred), color = "#1D86C7", linewidth = 1.5, na.rm=T) +
          geom_ribbon(data = modelDataEdit(), aes(x= age_original , ymin = minus95, ymax = plus95), fill = "#1D86C7", alpha = 0.2) +
          geom_point(data = df.plot(),aes(x=Age, y=Phenotype))+
          geom_line(data = df.plot(),aes(x=Age, y=Phenotype)) +
          geom_errorbar(data = df.plot(), aes(x=Age, y=Phenotype, ymin = lower, ymax = upper)) +
          ylab(paste0("Score (", traj(), ")")) +
          xlab("Age")

        }else if(input$plotCheckbox == FALSE){
          ggplot() +
            geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred), color = "#1D86C7", linewidth = 1.5, na.rm=T) +
            geom_ribbon(data = modelDataEdit(), aes(x= age_original , ymin = minus95, ymax = plus95), fill = "#1D86C7", alpha = 0.2) +
            ylab(paste0("Score (", traj(), ")")) +
            xlab("Age")
        }

      })

      # plot the mean trajectory against the model
      output$mainPlot <- renderPlot(
        mainPlot()
      )


      return(
        list(
        df.plot = df.plot,
        mainPlot = mainPlot,
        modelDataEdit = modelDataEdit)
        )
    }
  )
}

