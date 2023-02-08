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
    fit <- reactive({

      if(modelType() == "Linear"){
        fit <- lmer(formula = paste0(formCode(),
                                     "+ factor(", conditionVar(), ")",
                                     " + ", age(), "*factor(", conditionVar(), ")",
                                     ),
                    REML=F , data = modelData())
      } else if(modelType() == "Quadratic"){
        fit <- lmer(formula = paste0(formCode(),
                                     "+ factor(", conditionVar(), ")",
                                     " + ", age(), "*factor(", conditionVar(), ")",
                                     " + I(", age(), "^2)*factor(", conditionVar(), ")"
                                     ),
                    REML=F , data = modelData())
      } else if(modelType() == "Cubic"){
        fit <- lmer(formula = paste0(formCode(),
                                     "+ factor(", conditionVar(), ")",
                                     " + ", age(), "*factor(", conditionVar(), ")",
                                     " + I(", age(), "^2)*factor(", conditionVar(), ")",
                                     " + I(", age(), "^3)*factor(", conditionVar(), ")"
                                     ),
                    REML=F , data = modelData())
      } else if(modelType() == "Quartic"){
        fit <- lmer(formula = paste0(formCode(),
                                     "+ factor(", conditionVar(), ")",
                                     " + ", age(), "*factor(", conditionVar(), ")",
                                     " + I(", age(), "^2)*factor(", conditionVar(), ")",
                                     " + I(", age(), "^3)*factor(", conditionVar(), ")",
                                     " + I(", age(), "^4)*factor(", conditionVar(), ")"
                                     ),
                    REML=F , data = modelData())
      }
      return(fit)
    })

    modelDataEdit <- reactive({

      # select the index for the column that the user wants to split the analysis on
      i <- which(colnames(modelData()) %in% conditionVar())

      # add the "predicted" column to this dataset (it's not really a prediction because its the same dataset, it just shows the model)
      # add a column for coloring the plot by the split by variable
      modelDataEdit <- modelData() %>%
        mutate(pred = predict(fit(), ., re.form = NA)) %>%
        mutate(Group_Level = .[[i]] )

      return(modelDataEdit)
    })
      # ---------------
      # model results
      output$modelStatsFixed <- renderTable(
        cbind(
          tidy(fit(), "fixed"),
          confint(fit(), "beta_", method = "Wald")) %>%
          mutate(p.z = 2 * (1 - pnorm(abs(statistic)))) %>%
          mutate(p.z = ifelse(p.z < 0.001, "p < 0.001", p.z))
      )

      output$modelStatsRandom <- renderTable(
        as.data.frame(VarCorr(fit()),
                      order = "lower.tri")
      )

      # ---------------------------------------
      # Paste the model formula for the user to see (don't want it to appear straight away - could improve this)
      output$form<- renderText({
        paste0("<b>Model Formula:</b> ",  gsub(".*formula = (.+) , data =.*", "\\1", summary(fit())$call)[2])

      })

    # ---------------------------------------
    # Plot the split by variable plot
    output$modelCondPlot <- renderPlot({
      ggplot(data = dfPlot(),aes(x=Age, y=Phenotype)) +
        theme_light()+
        geom_point()+
        geom_line() +
        geom_errorbar(aes(ymin = lower, ymax = upper)) +
        geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred, color = factor(Group_Level) ) , na.rm=T)+
        scale_colour_discrete(na.translate = F)
    })
  }
  )
}

