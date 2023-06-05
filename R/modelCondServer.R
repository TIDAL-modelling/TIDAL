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
                            formCodeCovars,
                            dfPlot,
                            traj,
                            age,
                            timePoint,
                            modelType) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      vars <- reactive({
        req(modelData())
        if(input$varType == "cat"){
          colnames(modelData())[apply(modelData(), 2, function(x) length(unique(x))) < 40]
        }else if(input$varType == "cont"){
          colnames(modelData())[!apply(modelData(), 2, function(x) length(unique(x))) <= 2]
        }else{
          colnames(modelData())
        }
    })

      observeEvent(vars(),{
        req(modelData())
        # allow the user to select a variable from the column names of the dataset
        # critically this should be categorical variables (for the plot to split by a factor)
        # hacky way to choose a categorical variable is below
        # user can only select columns with unique values of length < 40. 40 being an arbitary number
        # but any more than this may not be very useful or visible on a plot, 40 is already a lot.

        updateSelectInput(
          session,
          "condition",
          choices = vars()
        )
      })

      # ---------------------------------------
      # If the user has selected a variable that is already a covariate print a warning message
      # and don't run anything further
      output$warningCov <- renderText({
        if(str_detect(formCodeCovars(), input$condition)){
          paste0("Error: You have chosen to condition on a variable that was chosen as a covariate on the previous page.\n
                 Please go back and remove it as a covariate or choose another variable.")
        }
      })


      # ---------------------------------------
      # allow the user to change the reference value for the categorical variable, by default i think it's the first item of the factor levels
      ###############################################################
      ###############################################################
      ######### change reference value
      ###############################################################
      ###############################################################


      # ---------------------------------------
      # paste the formula

      fit <- eventReactive(input$button,{

        ###############################################################
        ###############################################################
        #   EDIT TO USE THE SAME OPTIMISER AS PREVIOUSLY, IE. If warning message previously contains optimiser then use that below...
        ###############################################################
        ###############################################################

        # either factorise or make numeric the input$condition depending on the input$varType option
        if(input$varType == "cat"){
          cond <- paste0("as.factor(", input$condition, ")")
        }else if(input$varType == "cont"){
          cond <- paste0("as.numeric(", input$condition, ")")
        }


        if(modelType() == "Linear"){
          fit <- lmer(formula = paste0(formCodeCovars(),
                                       "+ ", cond,
                                       " + ", age(), "*", cond
          ),
          REML=F , data = modelData(),
          control=lmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
        } else if(modelType() == "Quadratic"){
          fit <- lmer(formula = paste0(formCodeCovars(),
                                       "+ ", cond,
                                       " + ", age(), "*", cond,
                                       " + I(", age(), "^2)*", cond
          ),
          REML=F , data = modelData(),
          control=lmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
        } else if(modelType() == "Cubic"){
          fit <- lmer(formula = paste0(formCodeCovars(),
                                       "+ ", cond,
                                       " + ", age(), "*", cond,
                                       " + I(", age(), "^2)*", cond,
                                       " + I(", age(), "^3)*", cond
          ),
          REML=F , data = modelData(),
          control=lmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
        } else if(modelType() == "Quartic"){
          fit <- lmer(formula = paste0(formCodeCovars(),
                                       "+ ", cond,
                                       " + ", age(), "*", cond,
                                       " + I(", age(), "^2)*", cond,
                                       " + I(", age(), "^3)*", cond,
                                       " + I(", age(), "^4)*", cond
          ),
          REML=F , data = modelData(),
          control=lmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
        }
        return(fit)
      })


      modelDataEdit <- eventReactive(input$button,{

        # select the index for the column that the user wants to split the analysis on
        colSplit <- which(colnames(modelData()) %in% input$condition)

        # add the "predicted" column to this dataset (it's not really a prediction because its the same dataset, it just shows the model)
        # add a column for coloring the plot by the split by variable

        ageVec <- modelData() %>% pull(!!age())

        ###############################################################
        ###############################################################
        #   edit for non-linear models
        ###############################################################
        ###############################################################
        if(modelType() == "Linear"){
          zero <- ageVec * summary(fit())$coefficients[2,1] + summary(fit())$coefficients[1,1]
        } else if(modelType() == "Quadratic"){
          zero  <- ageVec * summary(fit())$coefficients[2,1] + summary(fit())$coefficients[1,1] +
            ageVec^2 * summary(fit())$coefficients[3,1]
        } else if(modelType() == "Cubic"){
          zero  <- ageVec * summary(fit())$coefficients[2,1] + summary(fit())$coefficients[1,1]  +
            ageVec^2 * summary(fit())$coefficients[3,1] +
            ageVec^3 * summary(fit())$coefficients[4,1]
        } else if(modelType() == "Quartic"){
          zero  <- ageVec * summary(fit())$coefficients[2,1] + summary(fit())$coefficients[1,1]  +
            ageVec^2 * summary(fit())$coefficients[3,1] +
            ageVec^3 * summary(fit())$coefficients[4,1] +
            ageVec^4 * summary(fit())$coefficients[5,1]
        }

        if(input$varType == "cat"){
        n <- length(unique(pull(modelData(), !!sym(input$condition))))
        rowIndex <- which(str_detect(string = row.names(summary(fit())$coefficients),
                                     pattern = input$condition) &
                            str_starts(string = row.names(summary(fit())$coefficients),
                                       pattern = age(), negate = T))

        predCovs <- lapply(1:(n-1), function(i){

          if(modelType() == "Linear"){
            ageVec * summary(fit())$coefficients[2,1] + summary(fit())$coefficients[1,1] + summary(fit())$coefficients[rowIndex[i],1]
          } else if(modelType() == "Quadratic"){
            ageVec * summary(fit())$coefficients[2,1] + summary(fit())$coefficients[1,1] + summary(fit())$coefficients[rowIndex[i],1] +
              ageVec^2 * summary(fit())$coefficients[3,1]
          } else if(modelType() == "Cubic"){
            ageVec * summary(fit())$coefficients[2,1] + summary(fit())$coefficients[1,1] + summary(fit())$coefficients[rowIndex[i],1] +
              ageVec^2 * summary(fit())$coefficients[3,1] +
              ageVec^3 * summary(fit())$coefficients[4,1]
          } else if(modelType() == "Quartic"){
            ageVec * summary(fit())$coefficients[2,1] + summary(fit())$coefficients[1,1] + summary(fit())$coefficients[rowIndex[i],1]  +
              ageVec^2 * summary(fit())$coefficients[3,1] +
              ageVec^3 * summary(fit())$coefficients[4,1] +
              ageVec^4 * summary(fit())$coefficients[5,1]
          }

        })

        num <- str_subset(row.names(summary(fit())$coefficients), input$condition) %>%
                  str_split(., "\\)", simplify = T)  %>%
                  as.data.frame() %>%
                  filter(!str_detect(V1, "\\^")) %>%
                  pull(V2) %>%
                  unique()

        names(predCovs) <- paste0(input$condition, "_", num)

        modelDataEdit <- cbind(modelData(), do.call(cbind, predCovs)) %>%
          mutate(zero = zero) %>%
          mutate(!!input$condition := as.factor(.[[colSplit]]) ) %>%
          mutate(pred =  eval(parse(text =
                                      paste0(paste0("ifelse(", input$condition, " == '", num, "', ", input$condition, "_",num,",", collapse = " "), "zero",
                                             paste0(rep(")", length(num)), collapse = ""), collapse = "")
          )))
        }else if(input$varType == "cont"){
          modelDataEdit <- modelData() %>%
            mutate(pred = zero)
        }
        return(modelDataEdit)
      })
      # ---------------
      # model results
      output$modelStatsFixed <- renderTable({
        if(str_detect(formCodeCovars(), input$condition)){
          data.frame(NULL)
        }else{
        cbind(
          tidy(fit(), "fixed"),
          confint(fit(), "beta_", method = "Wald")) %>%
          mutate(p.z = 2 * (1 - pnorm(abs(statistic)))) %>%
          mutate(p.z = ifelse(p.z < 0.001, "p < 0.001", p.z))
        }
      })

      output$modelStatsRandom <- renderTable({
        if(str_detect(formCodeCovars(), input$condition)){
          data.frame(NULL)
        }else{
        as.data.frame(VarCorr(fit()),
                      order = "lower.tri")
        }
      })

      # ---------------------------------------
      # Paste the model formula for the user to see (don't want it to appear straight away - could improve this)
      output$form<- renderText({
        if(str_detect(formCodeCovars(), input$condition)){
          ""
        }else{
        paste0("<b>Model Formula:</b> ",  gsub(".*formula = (.+) , data =.*", "\\1", summary(fit())$call)[2])
        }
      })

      # ---------------------------------------
      ###############################################################
      ###############################################################
      ######### DIFFERENT FOR CONTINUOUS AND CATEGORICAL SPLIT - change continuous plot to percentiles or ± 1 SDs
      ###############################################################
      ###############################################################
      # Plot the split by variable plot
      plot <- eventReactive(input$button, {

        if(input$varType == "cat"){
          ggplot(data = dfPlot(),aes(x=Age, y=Phenotype)) +
            theme_light()+
            geom_point()+
            geom_line() +
            geom_errorbar(aes(ymin = lower, ymax = upper)) +
            geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred, color = !!sym(input$condition) ) , na.rm=T) +
            theme(legend.text = element_text(color = "black", size = 10))
        }else if(input$varType == "cont"){
          ggplot(data = dfPlot(),aes(x=Age, y=Phenotype)) +
            theme_light()+
            geom_point()+
            geom_line() +
            geom_errorbar(aes(ymin = lower, ymax = upper)) +
            geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred ) , na.rm=T)+
            scale_colour_discrete(na.translate = F) +
            theme(legend.text = element_text(color = "black", size = 10))
        }

      })

      output$modelCondPlot <- renderPlot({
        if(str_detect(formCodeCovars(), input$condition)){

        }else{
        plot()
        }
        })

      return(list(
        modelDataEdit = modelDataEdit
        ))

    }
  )
}

