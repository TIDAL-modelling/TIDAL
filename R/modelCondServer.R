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
modelCondServer <- function(id,
                            modelData,
                            formCode,
                            dfPlot,
                            traj,
                            age,
                            timePoint,
                            modelType) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      observeEvent(modelData(),{

        # allow the user to select a variable from the column names of the dataset
        # critically this should be categorical variables (for the plot to split by a factor)
        # hacky way to choose a categorical variable is below
        # user can only select columns with unique values of length < 40. 40 being an arbitary number
        # but any more than this may not be very useful or visible on a plot, 40 is already a lot.
        updateSelectInput(
          session,
          "condition",
          choices = colnames(modelData())[apply(modelData(), 2, function(x) length(unique(x))) < 40]
        )
      })

      # ---------------------------------------
      # paste the formula depending on whether covariates are wanted or not
      fit <- eventReactive(input$button,{

        if(modelType() == "Linear"){
          fit <- lmer(formula = paste0(formCode(),
                                       "+ factor(", input$condition, ")",
                                       " + ", age(), "*factor(", input$condition, ")"
          ),
          REML=F , data = modelData())
        } else if(modelType() == "Quadratic"){
          fit <- lmer(formula = paste0(formCode(),
                                       "+ factor(", input$condition, ")",
                                       " + ", age(), "*factor(", input$condition, ")",
                                       " + I(", age(), "^2)*factor(", input$condition, ")"
          ),
          REML=F , data = modelData())
        } else if(modelType() == "Cubic"){
          fit <- lmer(formula = paste0(formCode(),
                                       "+ factor(", input$condition, ")",
                                       " + ", age(), "*factor(", input$condition, ")",
                                       " + I(", age(), "^2)*factor(", input$condition, ")",
                                       " + I(", age(), "^3)*factor(", input$condition, ")"
          ),
          REML=F , data = modelData())
        } else if(modelType() == "Quartic"){
          fit <- lmer(formula = paste0(formCode(),
                                       "+ factor(", input$condition, ")",
                                       " + ", age(), "*factor(", input$condition, ")",
                                       " + I(", age(), "^2)*factor(", input$condition, ")",
                                       " + I(", age(), "^3)*factor(", input$condition, ")",
                                       " + I(", age(), "^4)*factor(", input$condition, ")"
          ),
          REML=F , data = modelData())
        }
        return(fit)
      })

      modelDataEdit <- eventReactive(input$button,{

        # select the index for the column that the user wants to split the analysis on
        i <- which(colnames(modelData()) %in% input$condition)

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

