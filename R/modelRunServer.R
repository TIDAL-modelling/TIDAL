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
      #### Run the model

      # Mean center age to 0
      newModelData <- reactive({
        req(age())
        modelData() %>%
          mutate(age_original = !!sym(age()) ) %>%
          mutate(!!sym(age()) := !!sym(age()) - mean( !!sym(age()), na.rm = T ))
      })

      # Run the model
      fit <- reactive({
        fit <- lmer(formula = formCode(), REML=F , data = newModelData())

        # Inspect warnings from the model
        message <- summary(fit)$optinfo$conv$lme4$messages %>% paste0(collapse = ", ")

        # If the model returns a warning about not converging, need to rescale variables or is.singular then
        # rerun the model with a different optimiser

        if( str_detect(message, "converge|Rescale|singular") ) {
          fit <- lmer(formula = formCode(), REML=F , data = newModelData(),
                      control=lmerControl(optimizer="bobyqa",
                                          optCtrl=list(maxfun=2e5)))
        }else{
          fit <- fit
        }

        return(fit)
      })

      # Output message
      warning <- reactive({
        if( any(str_detect(as.character(summary(fit())$call), "optimizer")) ){
          "Model did not converge. A different optimizer was used. Please interpret results with caution."
          # paste0(summary(fit())$optinfo$conv$lme4$messages %>% paste0(collapse = ", "))
        }else{
          ""
        }
      })


      # ------------------------------------------
      # show descriptive statistics
      output$desc <- renderTable(
        newModelData() %>%
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
        newModelData() %>%
          group_by(across( !!timePoint() )) %>%
          summarise(Age = mean(age_original, na.rm = T),
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

      return(
        list(
          fit = fit,
          data = newModelData,
          warning = warning
          ))
    }
  )
}

