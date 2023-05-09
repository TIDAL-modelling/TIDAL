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
                           warningMsg
) {

  moduleServer(
    id,
    function(input, output, session) {

      # ------------------------------------------
      # paste the formula text
      output$formulaText <- renderText({
        paste0("<b>Model Formula:</b> ",  gsub(".*formula = (.+) , data =.*", "\\1", summary(modelFit())$call)[2])
      })

      # ------------------------------------------
      # Warning text
      output$warning <- renderText({
        warningMsg()
      })

      # ------------------------------------------
      # model results
      output$modelStatsFixed <- renderTable(
        cbind(
          tidy(modelFit(), "fixed"),
          confint(modelFit(), "beta_", method = "Wald")) %>%
          mutate(p.z = 2 * (1 - pnorm(abs(statistic)))) %>%
          mutate(p.z = ifelse(p.z <= 0, "p < 0.001", p.z))
      )


      # Extract variance & correlation components for the random effects. In the
      # resulting data frame, vcov indicates the variances and covariances, and sdcor
      # indicates the SDs and correlations. "lower.tri" returns the estimates in the
      # order of the lower triangle of the variance-covariance matrix (can
      # alternatively request "cov.last" to return correlations / covariances last).

      output$modelStatsRandom <- renderTable(
        as.data.frame(VarCorr(modelFit()),
                      order = "lower.tri")
      )

      # number of observations (measurements) and the number of groups (people)
      output$Ndims <- renderText(
        paste0("The number of observations (measurements) is ",
               format(summary(modelFit())$devcomp$dims[[1]], big.mark=",", scientific=FALSE),
               " and the number of groups (people) is ",
                format(summary(modelFit())$ngrps[[1]], big.mark=",", scientific=FALSE) ,
               ".")
      )

    }
  )
}

