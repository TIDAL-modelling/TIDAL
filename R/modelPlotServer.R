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
                            modelFitBasic,
                            traj,
                            timePoint
                            ) {
  moduleServer(
    id,
    function(input, output, session) {

      # ------------------------------------------
      # add the "prediction"/model col to dataframe
      modelDataEdit <- reactive({
        modelData() %>%
          mutate(pred = predict(modelFitBasic(), ., re.form = NA))
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

