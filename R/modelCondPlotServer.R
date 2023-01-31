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
                            modelType,
                            conditionVar#,
                            # covariates,
                            # covarsLogical
                            ){

  moduleServer(
    id,
    function(input, output, session) {

    # ---------------------------------------
    # paste the formula depending on whether covariates are wanted or not
    modelDataEdit <- reactive({
      # if(!covarsLogical()){
      # fit <- lmer(formula = paste0(formCode(), "+ `", conditionVar(), "`"), REML=F , data = modelData())
      # }else{
      #   fit <- lmer(formula = paste0(formCode(), "+ `", conditionVar(), "`", " + ", paste0(covariates(), collapse = " + ") ), REML=F , data = modelData())
      # }

      if(modelType() == "Linear"){
        fit <- lmer(formula = paste0(formCode(), "+ `", conditionVar(), "` + ", age(), "*", conditionVar() ), REML=F , data = modelData())
      } else if(modelType() == "Quadratic"){
        fit <- lmer(formula = paste0(formCode(), "+ `", conditionVar(), "` + ", age(), "*", conditionVar(), " + I(", age(), "^2)*", conditionVar() ), REML=F , data = modelData())
      } else if(modelType() == "Cubic"){
        fit <- lmer(formula = paste0(formCode(), "+ `", conditionVar(), "` + ", age(), "*", conditionVar(), " + I(", age(), "^2)*", conditionVar()), REML=F , data = modelData())
      } else if(modelType() == "Quartic"){
        fit <- lmer(formula = paste0(formCode(), "+ `", conditionVar(), "` + ", age(), "*", conditionVar(), " + I(", age(), "^2)*", conditionVar()), REML=F , data = modelData())
      }

      # select the index for the column that the user wants to split the analysis on
      i <- which(colnames(modelData()) %in% conditionVar())

      # add the "predicted" column to this dataset (it's not really a prediction because its the same dataset, it just shows the model)
      # add a column for coloring the plot by the split by variable
      modelDataEdit <- modelData() %>%
        mutate(pred = predict(fit, ., re.form = NA)) %>%
        mutate(Group_Level = .[[i]] )

      # ---------------
      # model results
      output$modelStatsFixed <- renderTable(
        tidy(fit, "fixed")
      )

      output$modelStatsRandom <- renderTable(
        VarCorr(fit)
      )

      # ---------------------------------------
      # Paste the model formula for the user to see (don't want it to appear straight away - could improve this)
      output$form<- renderText({
        paste0("<b>Model Formula:</b> ",  gsub(".*formula = (.+) , data =.*", "\\1", summary(fit)$call)[2])
      })

      return(modelDataEdit)
    })

    # ---------------------------------------
    # Plot the split by variable plot
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

