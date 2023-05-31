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
                           covariateChoice,
                           button,
                           modelData,
                           formCode,
                           formCodeCovars,
                           age,
                           traj,
                           timePoint
                           ) {

  moduleServer(
    id,
    function(input, output, session) {


      output$covChoiceWarning <- renderText({
        if(covariateChoice()){
          "<b style='color:red;'>Error: Do not choose a covariate that is already selected as a variable.</b>"
        }
      })
      # ------------------------------------------
      #### Run the model

      # Mean center age to 0,
      # only do this when the action button in the side pannel is clicked
      newModelData <- eventReactive(button(), {
        req(age())
        req(modelData())
        modelData() %>%
          mutate(age_original = !!sym(age()) ) %>%
          mutate(!!sym(age()) := !!sym(age()) - mean( !!sym(age()), na.rm = T ))
      })

      # Run the model
      # only do this when the action button in the side panel is clicked
      fit <- eventReactive(button(), {
        req(formCodeCovars())

        # Sometimes lmer doesn't run, eg. if there are too few time points and/or too much missing data
        # Run the mixed model
        fit <- try(lmer(formula = formCodeCovars(), REML=F , data = newModelData()), silent = TRUE)

        if(class(fit) != "try-error"){
          # Inspect warnings from the model
          message <- summary(fit)$optinfo$conv$lme4$messages %>% paste0(collapse = ", ")

          # If the model returns a warning about not converging, need to rescale variables or is.singular then
          # rerun the model with a different optimiser

          if( str_detect(message, "converge|Rescale|singular") ) {
            fit <- lmer(formula = formCodeCovars(), REML=F , data = newModelData(),
                        control=lmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=2e5)))
          }else{
            fit <- fit
          }

        }

        return(fit)
      })

      fitBasic <- eventReactive(button(), {
        req(formCode())
        fitBasic <- lmer(formula = formCode(), REML=F , data = newModelData())
      })

      # Output message
      warning <- reactive({
        if(class(fit()) != "try-error"){
          if( any(str_detect(as.character(summary(fit())$call), "optimizer")) ){
            'The lme4 &quot;bobyqa&quot; optimiser was used. Please see more info <a href="https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html" style="color:blue" target="_blank"> here</a>.'
          }else{
            ""
          }
        }else{
          "The model doesn't run. This could be because there is too much missing data or too few time points."
        }
      })


      # ------------------------------------------
      # show descriptive statistics
      mainTable <- reactive({
        req(newModelData())
        newModelData() %>%
          group_by(across( !!timePoint() )) %>%
          summarise(N = sum(!is.na( !!sym(traj()) )),
                    mean = mean(!!sym(traj()), na.rm = T),
                    SD = sd(!!sym(traj()), na.rm = T),
                    median = median(!!sym(traj()), na.rm = T),
                    IQR = IQR(!!sym(traj()), na.rm = T)
          )
      })

      output$desc <- renderTable({
        mainTable()
      })

      # ------------------------------------------
      # Plot the distributions
      # Get the mean and sd for depression scores at each time point
      df.plot <- reactive({
        req(newModelData())
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

      output$plot <- renderPlot({
        req(df.plot())
        ggplot(df.plot(),aes(x=Age, y=Phenotype)) +
          theme_light()+
          theme_light()+
          geom_point()+
          geom_line() +
          geom_errorbar(aes(ymin = lower, ymax = upper))
      })

      return(
        list(
          fit = fit,
          fitBasic = fitBasic,
          data = newModelData,
          warning = warning,
          mainTable = mainTable
          ))
    }
  )
}

