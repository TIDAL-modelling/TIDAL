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
                           age
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
          mutate(p.z = ifelse(p.z <= 0, "p < 0.001", p.z))
      })
      output$modelStatsFixed <- renderTable(
        fixedTab()
      )


      # Extract variance & correlation components for the random effects. In the
      # resulting data frame, vcov indicates the variances and covariances, and sdcor
      # indicates the SDs and correlations. "lower.tri" returns the estimates in the
      # order of the lower triangle of the variance-covariance matrix (can
      # alternatively request "cov.last" to return correlations / covariances last).

      randomTab <- reactive({
        as.data.frame(VarCorr(modelFit()),
                      order = "lower.tri")
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

