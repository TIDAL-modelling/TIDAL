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
modelResultsServer <- function(id,
                           modelFit,
                           warningMsg,
                           modelData ,
                           age,
                           traj,
                           covars
) {

  moduleServer(
    id,
    function(input, output, session) {

      # ------------------------------------------
      # paste the formula text
      modelFormRender <- reactive({
        paste0("<b>Model Formula:</b> ",  gsub(".*formula = (.+) , data =.*", "\\1", summary(modelFit())$call)[2])
      })

      output$formulaText <- renderText({
        modelFormRender()
      })
      # ------------------------------------------
      # Message that "The time variable `age` has been mean centred to `meanAge`, which is the mean age across all time assessments. This aids model convergence."
      statement <- reactive({
        req(modelData())
        ageVar <- age()
        ageMeanVal <- modelData() %>%
          pull(sym(age())) %>%
          mean(na.rm = T) %>%
          round(2)

        statement <- paste0('The time variable "',ageVar,'" has been mean centred to ',ageMeanVal,', which is the mean value across all time assessments. This aids model convergence.')
        return(statement)
      })

      output$ageMean <- renderText({
        statement()
      })

      # ------------------------------------------
      # Warning text
      output$warning <- renderText({
        warningMsg()
      })

      # ------------------------------------------
      # model results
      fixedTab <- reactive({
        cbind(
          tidy(modelFit(), "fixed"),
          confint(modelFit(), "beta_", method = "Wald")) %>%
          mutate(p.z = 2 * (1 - pnorm(abs(statistic)))) %>%
          mutate(p.z = ifelse(p.z <= 0, "p < 0.001", round(p.z, 3)))
      })
      output$modelStatsFixed <- renderTable(
        fixedTab()
      )

      # ------------------------------------------
      # Interpretation of fixed effects
      interpretation <- eventReactive(modelFit(), {
        req(modelData())

        # Intercept:
        intercept <- round(fixedTab()$estimate[1], 2)

        # Age variable name:
        ageVar <- age()

        # y-axis name:
        trajVar <- traj()

        # Mean age:
        ageMeanVal <- modelData() %>%
          pull(sym(age())) %>%
          mean(na.rm = T) %>%
          round(2)

        # Slope:
        slope <- round(fixedTab()$estimate[2], 2)

        # Direction
        direction <- ifelse(slope >= 0 , "an increase", "a decrease")

        # Confounders:
        confounders <- paste0(covars(), collapse = ", ")

        # Confounder level names and estimates
        if(length(covars()) > 0){
        i <- which(str_detect(fixedTab()$term, paste0(covars(), collapse = "|")))
        confounderLevels <- paste0(fixedTab()$term[i] , collapse = ", ")
        confounderEst <- paste0(round(fixedTab()$estimate[i],2) , collapse = ", ")
        }

        # Text:
        paste0(
        'The score at the intercept is ', intercept, '. The intercept here has been shifted to the mean age of all the assessments which is ', ageMeanVal, '. You could interpret this as the score at the intercept of ', ageVar,' ', ageMeanVal,' is ', intercept, '.
        <br/>
        <br/>

        Every unit increase in ',ageVar,' is associated with ', direction ,' of ', trajVar ,' by ', slope,'.
        <br/>
        <br/>',
        ifelse(length(covars()) > 0,
        paste0('These estimates adjusted for the following confounders/covariates: ', confounders ,'.
        <br/>
        <br/>

        In addition, the following confounders/covariates: ', confounderLevels ,' are associated with an increase or decrease of ', trajVar ,' by ',confounderEst,' respectively.
        <br/>
        <br/>

        Please note, this section does not estimate group specific trajectories. See the Interaction Variable tab for group specific interactions and trajectories.
        <br/>
        <br/>'), paste0('')))
      })

      output$interFixed <- renderText({
        interpretation()
      })

      # ------------------------------------------
      # Extract variance & correlation components for the random effects. In the
      # resulting data frame, vcov indicates the variances and covariances, and sdcor
      # indicates the SDs and correlations. "lower.tri" returns the estimates in the
      # order of the lower triangle of the variance-covariance matrix (can
      # alternatively request "cov.last" to return correlations / covariances last).

      randomTab <- reactive({
        as.data.frame(VarCorr(modelFit()),
                      order = "lower.tri") %>%
          mutate(across(where(is.numeric), round, 3))
      })

      output$modelStatsRandom <- renderTable(
        randomTab()
      )

      # number of observations (measurements) and the number of groups (people)
      N <- reactive({
        paste0("The number of observations (measurements) is ",
               format(summary(modelFit())$devcomp$dims[[1]], big.mark=",", scientific=FALSE),
               " and the number of groups (people) is ",
               format(summary(modelFit())$ngrps[[1]], big.mark=",", scientific=FALSE) ,
               ".")
      })
      output$Ndims <- renderText(
        N()
      )
      return(
        list(
        statement = statement,
        modelFormRender = modelFormRender,
        fixedTab = fixedTab,
        randomTab = randomTab,
        N = N
        )
      )

    }
  )
}

