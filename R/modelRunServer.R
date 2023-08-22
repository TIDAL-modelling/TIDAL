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
                           formCodeCovars,
                           age,
                           traj,
                           timePoint,
                           weights,
                           weightCol
                           ) {

  moduleServer(
    id,
    function(input, output, session) {


      output$covChoiceWarning <- renderText({
        if(covariateChoice()){
          "<b style='color:red;'>Error: Do not choose a covariate that is already selected as a variable or choose the same covariate as both continuous and categorical.</b>"
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
          mutate(age_original = as.numeric(!!sym(age())) ) %>%
          mutate(!!sym(age()) := as.numeric(!!sym(age())) - mean( as.numeric(!!sym(age())), na.rm = T ))
      })

      # Run the model
      # only do this when the action button in the side panel is clicked
      fit <- eventReactive(button(), {
        req(formCodeCovars())

        # Sometimes lmer doesn't run, eg. if there are too few time points and/or too much missing data
        # Run the mixed model
        fit <-
          if(weights() == FALSE){
            try(lmer(formula = formCodeCovars(),
                          REML=F ,
                          data = newModelData(),
                          control=lmerControl(optimizer="bobyqa",
                                              optCtrl=list(maxfun=2e5)),
                          weights = NULL),
                     silent = TRUE)
          }else{
            try(lmer(formula = formCodeCovars(),
                     REML=F ,
                     data = newModelData(),
                     control=lmerControl(optimizer="bobyqa",
                                         optCtrl=list(maxfun=2e5)),
                     weights = weightCol()),
                silent = TRUE)
          }

        return(fit)
      })

      # Output message
      warning <- eventReactive(button(), {
        if(class(fit()) != "try-error"){
            paste0('
            The following <a href="https://cran.r-project.org/web/packages/lme4/lme4.pdf" style="color:blue" target="_blank">lme4</a> function is used to run the model:
            </br>
            <pre>
            <code>',
            if(weights() == FALSE){
            paste0('lmer(formula = ',formCodeCovars(),',
                 REML = FALSE ,
                 data = newModelData,
                 control = lmerControl(optimizer="bobyqa",
                                      optCtrl=list(maxfun=2e5)))')
            }else{
              paste0('lmer(formula = ',formCodeCovars(),',
                 REML = FALSE ,
                 data = newModelData,
                 control = lmerControl(optimizer="bobyqa",
                                      optCtrl=list(maxfun=2e5)),
                     weights = ',weightCol(),')')
            },
            '</code>
            </pre>
            Please see more infomation about the &quot;bobyqa&quot; optimiser <a href="https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html" style="color:blue" target="_blank"> here</a>.
            </br>
            The argument <code>REML = FALSE</code> indicates the model was fitted by maximum likelihood.')
        }else{
          "The model doesn't run. This could be because there is too much missing data or too few time points. Try changing the random slope term."
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
          geom_point()+
          geom_line() +
          geom_errorbar(aes(ymin = lower, ymax = upper))+
          ylab(paste0("Score (", traj(), ")")) +
          xlab("Age")
      })

      return(
        list(
          fit = fit,
          data = newModelData,
          warning = warning,
          mainTable = mainTable
          ))
    }
  )
}

