#' Run model - data exploration page
#'
#' @noRd
#' @keywords internal
#' @export
modelRunServer <- function(id, modelData, formCode) {
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

      # output$desc <- renderTable(
      #
      # )

      output$modelStatsFixed <- renderTable(
        tidy(fit(), "fixed")
      )

      output$modelStatsRandom <- renderTable(
        print(VarCorr(fit()), digits = 2, comp=c("Variance")) %>%
          as.data.frame() %>%
          set_rownames(NULL)
      )
      return(fit)
    }
  )
}

