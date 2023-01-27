#' split model by variable
#'
#' @import broom.mixed
#' @import lme4
#' @import dplyr
#' @import ggplot2
#' @import data.table
#' @import shinyjs
#' @import tidyr
#'
#' @noRd
#' @keywords internal
#' @export
modelCondPlotServer <- function(id,
                            modelData,
                            formCode,
                            dfPlot,
                            traj,
                            age,
                            timePoint,
                            condition,
                            covariates,
                            covarLogical){

  moduleServer(
    id,
    function(input, output, session) {

    modelDataEdit <- reactive({
      fit <- lmer(formula = paste0(formCode(), "+ ", condition()), REML=F , data = modelData())

      modelDataEdit <- modelData() %>%
        mutate(pred = predict(fit, ., re.form = NA)) %>%
        mutate(Group_Level = as.factor( sex ) )

      return(modelDataEdit)
    })

    output$test <- renderText({
      paste(formCode(), "+ ", condition() )
    })

    output$table <- renderTable({
      head(modelDataEdit())
    })

    output$modelCondPlot <- renderPlot({
      ggplot(data = dfPlot(),aes(x=Age, y=Phenotype)) +
        theme_light()+
        geom_point()+
        geom_line() +
        geom_errorbar(aes(ymin = lower, ymax = upper)) +
        geom_line(data = modelDataEdit(), aes(x= !!sym(age()) ,  y = pred, color = Group_Level ) , na.rm=T)+
        scale_colour_discrete(na.translate = F)
    })
  }
  )
}

