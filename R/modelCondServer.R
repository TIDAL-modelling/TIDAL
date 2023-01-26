#' split model by variable
#'
#' @noRd
#' @keywords internal
#' @export
modelCondServer <- function(id, modelData, formCode, dfPlot) {
  moduleServer(
    id,
    function(input, output, session) {
      fit <- reactive({
        lmer(formula = paste0(formCode(), "+ ", input$condition), REML=F , data = modelData())
      })

      modelDataEdit <- reactive({
        modelDataEdit <- modelData() %>%
          mutate(pred = predict(fit(), ., re.form = NA))
        modelDataEdit$Group_Level <- as.factor(modelDataEdit[,input$condition])
        return(modelDataEdit)
      })

      cond <- reactive({
        input$condition
      })

      output$modelCondPlot <- renderPlot({
        ggplot(data = dfPlot(),aes(x=Age, y=Dep)) +
          theme_light()+
          geom_point()+
          geom_line() +
          geom_errorbar(aes(ymin = lower, ymax = upper)) +
          geom_line(data = modelDataEdit(), aes(x=age,  y = pred, color = Group_Level ) , na.rm=T)+
          scale_colour_discrete(na.translate = F)
      })

    }
  )
}

