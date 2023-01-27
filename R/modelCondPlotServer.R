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
                            conditionVar,
                            covariates,
                            covarsLogical){

  moduleServer(
    id,
    function(input, output, session) {

    modelDataEdit <- reactive({
      if(!covarsLogical()){
      fit <- lmer(formula = paste0(formCode(), "+ ", conditionVar()), REML=F , data = modelData())
      }else{
        fit <- lmer(formula = paste0(formCode(), "+ ", conditionVar(), " + ", paste0(covariates(), collapse = " + ") ), REML=F , data = modelData())
      }

      i <- which(colnames(modelData()) %in% conditionVar())

      modelDataEdit <- modelData() %>%
        mutate(pred = predict(fit, ., re.form = NA)) %>%
        mutate(Group_Level = .[[i]] )

      return(modelDataEdit)
    })

    output$form<- renderText({
      if(nchar(formCode()) > 20 & !as.logical(covarsLogical()) ){            # not the best/hacky way to make sure the formula doesn't show straight away
        paste0("<b>Model Formula:</b> ", formCode(), "+ ", conditionVar() )
      }else if(nchar(formCode()) > 20 & as.logical(covarsLogical())){
        paste0("<b>Model Formula:</b> ", formCode(), "+ ", conditionVar(), " + ", paste(covariates(), collapse = " + ") )
      }else{
        "<b>Model Formula:</b>"
      }
    })

    output$modelCondPlot <- renderPlot({
      ggplot(data = dfPlot(),aes(x=Age, y=Phenotype)) +
        theme_light()+
        geom_point()+
        geom_line() +
        geom_errorbar(aes(ymin = lower, ymax = upper)) +
        geom_line(data = modelDataEdit(), aes(x= !!sym(age()) ,  y = pred, color = factor(Group_Level) ) , na.rm=T)+
        scale_colour_discrete(na.translate = F)
    })
  }
  )
}

