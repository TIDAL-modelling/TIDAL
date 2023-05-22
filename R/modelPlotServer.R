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
                            modelType
                            ) {
  moduleServer(
    id,
    function(input, output, session) {

      # ------------------------------------------
      # add the "prediction"/model col to dataframe
      modelDataEdit <- reactive({

        age <- modelData() %>% pull(!!age())


        if(modelType() == "Linear"){
          adjustedScore <- age * summary(modelFit())$coefficients[2,1] + summary(modelFit())$coefficients[1,1]
        } else if(modelType() == "Quadratic"){
          adjustedScore <- age * summary(modelFit())$coefficients[2,1] + summary(modelFit())$coefficients[1,1] +
            age^2 * summary(modelFit())$coefficients[3,1]
        } else if(modelType() == "Cubic"){
          adjustedScore <- age * summary(modelFit())$coefficients[2,1] + summary(modelFit())$coefficients[1,1]  +
            age^2 * summary(modelFit())$coefficients[3,1] +
            age^3 * summary(modelFit())$coefficients[4,1]
        } else if(modelType() == "Quartic"){
          adjustedScore <- age * summary(modelFit())$coefficients[2,1] + summary(modelFit())$coefficients[1,1]  +
            age^2 * summary(modelFit())$coefficients[3,1] +
            age^3 * summary(modelFit())$coefficients[4,1] +
            age^4 * summary(modelFit())$coefficients[5,1]
        }

        modelData() %>%
          mutate(pred = adjustedScore)
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
      mainPlot <- reactive({
        ggplot(df.plot(),aes(x=Age, y=Phenotype)) +
          theme_light()+
          geom_point()+
          geom_line() +
          geom_errorbar(aes(ymin = lower, ymax = upper)) +
          geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred), na.rm=T)
      })

      # plot the mean trajectory against the model
      output$mainPlot <- renderPlot(
        mainPlot()
      )


      return(
        list(
        df.plot = df.plot,
        mainPlot = mainPlot)
        )
    }
  )
}

