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
      # paste the formula text
      output$formulaText <- renderText({
        if(nchar(formCode()) > 20 ){            # not the best/hacky way to make sure the formula doesn't show straight away
          paste0("<b>Model Formula:</b> ", formCode())
        }else{
          "<b>Model Formula:</b>"
        }
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

      return(fit)
    }
  )
}

