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
                           SubjectID,
                           traj,
                           age,
                           timePoint
                           ) {

  moduleServer(
    id,
    function(input, output, session) {

      fit <- reactive({
        lmer(formula = formCode(), REML=F , data = modelData())
      })


      output$formulaText <- renderText({
        if(nchar(formCode()) > 20 ){            # not the best/hacky way to make sure the formula doesn't show straight away
          paste0("<b>Model Formula:</b> ", formCode())
        }else{
          "<b>Model Formula:</b>"
        }
      })

      output$desc <- renderTable(
        modelData() %>%
          group_by(timePoint()) %>%
          summarise(N = sum(!is.na(traj())),
                    mean = mean(traj(), na.rm = T),
                    SD = sd(traj(), na.rm = T),
                    median = median(traj(), na.rm = T),
                    IQR = IQR(traj(), na.rm = T)
          )
      )

      output$modelStatsFixed <- renderTable(
        tidy(fit(), "fixed")
      )

      output$modelStatsRandom <- renderTable(
        print(VarCorr(fit()), digits = 2, comp=c("Variance")) %>%
          as.data.frame() %>%
          magrittr::set_rownames(NULL)
      )
      return(fit)
    }
  )
}

