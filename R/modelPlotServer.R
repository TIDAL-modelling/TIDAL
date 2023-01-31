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
                            SubjectID,
                            traj,
                            timePoint,
                            age
                            ) {
  moduleServer(
    id,
    function(input, output, session) {

      # ------------------------------------------
      # Get the mean and sd for depression scores at each time point
      df.plot <- reactive({
        modelData() %>%
          group_by(across( !!timePoint() )) %>%
          summarise(Age = mean(!!sym(age()), na.rm = T),
                    Phenotype = mean(!!sym(traj()), na.rm = T),
                    SD = sd(!!sym(traj()), na.rm = T),
                    n = sum(!is.na( !!sym(traj()) ))
          ) %>%
          mutate(upper = Phenotype + ( qnorm(0.975)*SD/sqrt(n) ),
                 lower = Phenotype - ( qnorm(0.975)*SD/sqrt(n) ))

      })

      # ------------------------------------------
      # add the "prediction"/model col to dataframe
      modelDataEdit <- reactive({
        modelData() %>%
          mutate(pred = predict(modelFit(), ., re.form = NA))
      })

      # ------------------------------------------
      # plot the mean trajectory against the model
      output$mainPlot <- renderPlot(
        ggplot(df.plot(),aes(x=Age, y=Phenotype)) +
          theme_light()+
          geom_point()+
          geom_line() +
          geom_errorbar(aes(ymin = lower, ymax = upper)) +
          geom_line(data = modelDataEdit(), aes(x= !!sym(age()) ,  y = pred), na.rm=T)
      )
      return(df.plot)
    }
  )
}

