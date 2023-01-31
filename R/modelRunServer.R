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
#'
#' @noRd
#' @keywords internal
#' @export
modelRunServer <- function(id,
                           modelData,
                           formCode,
                           age,
                           traj,
                           timePoint
                           ) {

  moduleServer(
    id,
    function(input, output, session) {

      # ------------------------------------------
      # run the model
      fit <- reactive({
        lmer(formula = formCode(), REML=F , data = modelData())
      })

      # ------------------------------------------
      # show descriptive statistics
      output$desc <- renderTable(
        modelData() %>%
          group_by(across( !!timePoint() )) %>%
          summarise(N = sum(!is.na( !!sym(traj()) )),
                    mean = mean(!!sym(traj()), na.rm = T),
                    SD = sd(!!sym(traj()), na.rm = T),
                    median = median(!!sym(traj()), na.rm = T),
                    IQR = IQR(!!sym(traj()), na.rm = T)
          )
      )

      # ------------------------------------------
      # Plot the distributions
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

      output$plot <- renderPlot(
        ggplot(df.plot(),aes(x=Age, y=Phenotype)) +
          theme_light()+
          theme_light()+
          geom_point()+
          geom_line() +
          geom_errorbar(aes(ymin = lower, ymax = upper))
      )

      return(fit)
    }
  )
}

