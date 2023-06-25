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
      # z-scale continuous variable
      # also factorise or make numeric the interaction variable
      modelDataScaled <- eventReactive(input$button,{
        colSplit <- which(colnames(modelData()) %in% input$condition)

        if(input$varType == "cat"){
          modelData() %>%
            filter(!is.na(!!sym(input$condition))) %>%
            mutate(!!input$condition := factor(.[[colSplit]]) )

        }else if(input$varType == "cont"){
          modelData() %>%
            mutate(!!input$condition := scale(.[[colSplit]]) )
        }
      })

      output$zScore <- renderText({
        if(input$varType == "cat"){
          paste0("")
        }else if(input$varType == "cont"){
          paste0('Variable: "', input$condition, '" has been z-score standardised.')
        }
      })



      # ---------------------------------------
      fit <- eventReactive(input$button,{


        # either factorise or make numeric the input$condition depending on the input$varType option
        if(modelType() == "Linear"){
          fit <- lmer(formula = paste0(formCodeCovars(),
                                       "+ ", input$condition,
                                       " + ", age(), "*", input$condition
          ),
          REML=F , data = modelDataScaled(),
          control=lmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
        } else if(modelType() == "Quadratic"){
          fit <- lmer(formula = paste0(formCodeCovars(),
                                       "+ ", input$condition,
                                       " + ", age(), "*", input$condition,
                                       " + I(", age(), "^2)*", input$condition
          ),
          REML=F , data = modelDataScaled(),
          control=lmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
        } else if(modelType() == "Cubic"){
          fit <- lmer(formula = paste0(formCodeCovars(),
                                       "+ ", input$condition,
                                       " + ", age(), "*", input$condition,
                                       " + I(", age(), "^2)*", input$condition,
                                       " + I(", age(), "^3)*", input$condition
          ),
          REML=F , data = modelDataScaled(),
          control=lmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
        } else if(modelType() == "Quartic"){
          fit <- lmer(formula = paste0(formCodeCovars(),
                                       "+ ", input$condition,
                                       " + ", age(), "*", input$condition,
                                       " + I(", age(), "^2)*", input$condition,
                                       " + I(", age(), "^3)*", input$condition,
                                       " + I(", age(), "^4)*", input$condition
          ),
          REML=F , data = modelDataScaled(),
          control=lmerControl(optimizer="bobyqa",
                              optCtrl=list(maxfun=2e5)))
        }
        return(fit)
      })


      modelDataEdit <- eventReactive(input$button,{

        # select the index for the column that the user wants to split the analysis on
        colSplit <- which(colnames(modelDataScaled()) %in% input$condition)

        # add the "predicted" column to this dataset (it's not really a prediction because its the same dataset, it just shows the model)
        # add a column for coloring the plot by the split by variable

        ageVec <- modelDataScaled() %>% pull(!!age())

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

        rowIndex <- which(str_detect(string = row.names(summary(fit())$coefficients),
                                     pattern = input$condition) &
                            str_detect(string = row.names(summary(fit())$coefficients),
                                       pattern = ":", negate = T))

        rowIndexInteract1 <- which(str_detect(string = row.names(summary(fit())$coefficients),
                                              pattern = input$condition) &
                                     str_detect(string = row.names(summary(fit())$coefficients),
                                                pattern = ":") &
                                     str_detect(string = row.names(summary(fit())$coefficients),
                                                pattern = "\\^", negate = T))

        rowIndexInteract2 <- which(str_detect(string = row.names(summary(fit())$coefficients),
                                              pattern = input$condition) &
                                     str_detect(string = row.names(summary(fit())$coefficients),
                                                pattern = ":") &
                                     str_detect(string = row.names(summary(fit())$coefficients),
                                                pattern = "\\^2"))

        rowIndexInteract3 <- which(str_detect(string = row.names(summary(fit())$coefficients),
                                              pattern = input$condition) &
                                     str_detect(string = row.names(summary(fit())$coefficients),
                                                pattern = ":") &
                                     str_detect(string = row.names(summary(fit())$coefficients),
                                                pattern = "\\^3"))

        rowIndexInteract4 <- which(str_detect(string = row.names(summary(fit())$coefficients),
                                              pattern = input$condition) &
                                     str_detect(string = row.names(summary(fit())$coefficients),
                                                pattern = ":") &
                                     str_detect(string = row.names(summary(fit())$coefficients),
                                                pattern = "\\^4"))

        if(input$varType == "cat"){
          n <- length(unique(pull(modelDataScaled(), !!sym(input$condition))))

          predCovs <- lapply(1:(n-1), function(i){
            if(modelType() == "Linear"){
              summary(fit())$coefficients[1,1] +
              (ageVec * summary(fit())$coefficients[2,1]) +
              summary(fit())$coefficients[rowIndex[i],1] +
              (ageVec * summary(fit())$coefficients[rowIndexInteract1[i],1])
            } else if(modelType() == "Quadratic"){
              summary(fit())$coefficients[1,1] +
              ageVec * summary(fit())$coefficients[2,1] +
              summary(fit())$coefficients[rowIndex[i],1] +
              ageVec^2 * summary(fit())$coefficients[3,1] +
              (ageVec * summary(fit())$coefficients[rowIndexInteract1[i],1]) +
              (ageVec^2 * summary(fit())$coefficients[(rowIndexInteract2)[i],1])
            } else if(modelType() == "Cubic"){
              summary(fit())$coefficients[1,1] +
              ageVec * summary(fit())$coefficients[2,1] +
              summary(fit())$coefficients[rowIndex[i],1] +
              ageVec^2 * summary(fit())$coefficients[3,1] +
              ageVec^3 * summary(fit())$coefficients[4,1] +
              (ageVec * summary(fit())$coefficients[rowIndexInteract1[i],1]) +
              (ageVec^2 * summary(fit())$coefficients[(rowIndexInteract2)[i],1]) +
              (ageVec^3 * summary(fit())$coefficients[(rowIndexInteract3)[i],1])
            } else if(modelType() == "Quartic"){
              summary(fit())$coefficients[1,1] +
              ageVec * summary(fit())$coefficients[2,1] +
                summary(fit())$coefficients[rowIndex[i],1]  +
                ageVec^2 * summary(fit())$coefficients[3,1] +
                ageVec^3 * summary(fit())$coefficients[4,1] +
                ageVec^4 * summary(fit())$coefficients[5,1] +
                (ageVec * summary(fit())$coefficients[rowIndexInteract1[i],1]) +
                (ageVec^2 * summary(fit())$coefficients[(rowIndexInteract2)[i],1]) +
                (ageVec^3 * summary(fit())$coefficients[(rowIndexInteract3)[i],1]) +
                (ageVec^4 * summary(fit())$coefficients[(rowIndexInteract4)[i],1])
            }

          })

          num <- str_subset(row.names(summary(fit())$coefficients), input$condition) %>%
            str_split(., "\\)", simplify = T)  %>%
            as.data.frame() %>%
            filter(!str_detect(V1, "\\^")) %>%
            pull(V2) %>%
            unique()

          names(predCovs) <- paste0(input$condition, "_", num)

          modelDataEdit <- cbind(modelDataScaled(), do.call(cbind, predCovs)) %>%
            mutate(zero = zero) %>%
            mutate(!!input$condition := as.factor(.[[colSplit]]) ) %>%
            mutate(pred =  eval(parse(text =
                                        paste0(paste0("ifelse(", input$condition, " == '", num, "', ", input$condition, "_",num,",", collapse = " "), "zero",
                                               paste0(rep(")", length(num)), collapse = ""), collapse = "")
            )))
        }else if(input$varType == "cont"){

          if(modelType() == "Linear"){
            plus <- ageVec * summary(fit())$coefficients[2,1] +
              summary(fit())$coefficients[1,1] +
              summary(fit())$coefficients[rowIndex,1]  +
              (ageVec * summary(fit())$coefficients[rowIndexInteract1,1])

            minus <- ageVec * summary(fit())$coefficients[2,1] +
              summary(fit())$coefficients[1,1] -
              summary(fit())$coefficients[rowIndex,1] -
              (ageVec * summary(fit())$coefficients[rowIndexInteract1,1])

          } else if(modelType() == "Quadratic"){
            plus  <- ageVec * summary(fit())$coefficients[2,1] +
              summary(fit())$coefficients[1,1] +
              ageVec^2 * summary(fit())$coefficients[3,1] +
              summary(fit())$coefficients[rowIndex,1] +
              (ageVec * summary(fit())$coefficients[rowIndexInteract1,1]) +
              (ageVec^2 * summary(fit())$coefficients[(rowIndexInteract2),1])

            minus <- ageVec * summary(fit())$coefficients[2,1] +
              summary(fit())$coefficients[1,1] +
              ageVec^2 * summary(fit())$coefficients[3,1] -
              summary(fit())$coefficients[rowIndex,1] -
              (ageVec * summary(fit())$coefficients[rowIndexInteract1,1]) -
              (ageVec^2 * summary(fit())$coefficients[(rowIndexInteract2),1])

          } else if(modelType() == "Cubic"){
            plus  <- ageVec * summary(fit())$coefficients[2,1] +
              summary(fit())$coefficients[1,1]  +
              ageVec^2 * summary(fit())$coefficients[3,1] +
              ageVec^3 * summary(fit())$coefficients[4,1] +
              summary(fit())$coefficients[rowIndex,1] +
              (ageVec * summary(fit())$coefficients[rowIndexInteract1,1]) +
              (ageVec^2 * summary(fit())$coefficients[(rowIndexInteract2),1]) +
              (ageVec^3 * summary(fit())$coefficients[(rowIndexInteract3),1])

            minus <-  ageVec * summary(fit())$coefficients[2,1] +
              summary(fit())$coefficients[1,1]  +
              ageVec^2 * summary(fit())$coefficients[3,1] +
              ageVec^3 * summary(fit())$coefficients[4,1] -
              summary(fit())$coefficients[rowIndex,1] -
              (ageVec * summary(fit())$coefficients[rowIndexInteract1,1]) -
              (ageVec^2 * summary(fit())$coefficients[(rowIndexInteract2),1]) -
              (ageVec^3 * summary(fit())$coefficients[(rowIndexInteract3),1])

          } else if(modelType() == "Quartic"){
            plus  <- ageVec * summary(fit())$coefficients[2,1] +
              summary(fit())$coefficients[1,1]  +
              ageVec^2 * summary(fit())$coefficients[3,1] +
              ageVec^3 * summary(fit())$coefficients[4,1] +
              ageVec^4 * summary(fit())$coefficients[5,1] +
              summary(fit())$coefficients[rowIndex,1] +
              (ageVec * summary(fit())$coefficients[rowIndexInteract1,1]) +
              (ageVec^2 * summary(fit())$coefficients[(rowIndexInteract2),1]) +
              (ageVec^3 * summary(fit())$coefficients[(rowIndexInteract3),1]) +
              (ageVec^4 * summary(fit())$coefficients[(rowIndexInteract4),1])

            minus <-  ageVec * summary(fit())$coefficients[2,1] +
              summary(fit())$coefficients[1,1]  +
              ageVec^2 * summary(fit())$coefficients[3,1] +
              ageVec^3 * summary(fit())$coefficients[4,1] +
              ageVec^4 * summary(fit())$coefficients[5,1] -
              summary(fit())$coefficients[rowIndex,1] -
              (ageVec * summary(fit())$coefficients[rowIndexInteract1,1]) -
              (ageVec^2 * summary(fit())$coefficients[(rowIndexInteract2),1]) -
              (ageVec^3 * summary(fit())$coefficients[(rowIndexInteract3),1]) -
              (ageVec^4 * summary(fit())$coefficients[(rowIndexInteract4),1])
          }

          modelDataEdit <- modelDataScaled() %>%
            mutate(pred = zero) %>%
            mutate(plus = plus) %>%
            mutate(minus = minus)
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
            mutate(p.z = ifelse(p.z < 0.001, "p < 0.001", round(p.z, 3) ))
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
      # Plot the split by variable plot
      plot <- eventReactive(c(input$button, input$plotCheckbox), {

        if(input$plotCheckbox == TRUE){
          if(input$varType == "cat"){
            ggplot(data = dfPlot(),aes(x=Age, y=Phenotype)) +

              geom_point()+
              geom_line() +
              geom_errorbar(aes(ymin = lower, ymax = upper)) +
              geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred, color = !!sym(input$condition) ) , na.rm=T) +
              theme(legend.text = element_text(color = "black"))+
              ylab(paste0("Score (", traj(), ")")) +
              xlab("Age")
          }else if(input$varType == "cont"){
            ggplot(data = dfPlot(),aes(x=Age, y=Phenotype)) +

              geom_point()+
              geom_line() +
              geom_errorbar(aes(ymin = lower, ymax = upper)) +
              geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred, color = "Population Average" ) , linewidth = 1.5, na.rm=T)+
              geom_line(data = modelDataEdit(), aes(x= age_original ,  y = plus, color = "+ 1 SD" ) , na.rm=T)+
              geom_line(data = modelDataEdit(), aes(x= age_original ,  y = minus, color = "- 1 SD" ) , na.rm=T)+
              theme(legend.text = element_text(color = "black"))+
              labs(color = "") +
              scale_color_manual(
                breaks = c("+ 1 SD", "Population Average", "- 1 SD"),
                values = c("#d55e00", "black", "#0072b2")) +
              ylab(paste0("Score (", traj(), ")")) +
              xlab("Age")
          }
        }else if(input$plotCheckbox == FALSE){
          if(input$varType == "cat"){
            ggplot() +
              geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred, color = !!sym(input$condition) ) , na.rm=T) +
              theme(legend.text = element_text(color = "black")) +
              ylab(paste0("Score (", traj(), ")")) +
              xlab("Age")
          }else if(input$varType == "cont"){
            ggplot() +
              geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred, color = "Population Average" ) , linewidth = 1.5, na.rm=T)+
              geom_line(data = modelDataEdit(), aes(x= age_original ,  y = plus, color = "+ 1 SD" ) , na.rm=T)+
              geom_line(data = modelDataEdit(), aes(x= age_original ,  y = minus, color = "- 1 SD" ) , na.rm=T)+
              theme(legend.text = element_text(color = "black"))+
              labs(color = "") +
              scale_color_manual(
                breaks = c("+ 1 SD", "Population Average", "- 1 SD"),
                values = c("#d55e00", "black", "#0072b2")) +
              ylab(paste0("Score (", traj(), ")")) +
              xlab("Age")
          }
        }

      })

      output$modelCondPlot <- renderPlot({
        if(str_detect(formCodeCovars(), input$condition)){

        }else{
          plot()
        }
      })

      ###############################################################
      # --- Score for a given set of ages - Alternative Model Results Tab -----
      # ------------------------------------------
      # Allow the user to select the ages they want to calculate scores for
      output$selectAgeScore <- renderUI({
        ageOrig <- modelDataEdit() %>%
          pull(age_original)
        ageOrig <- ageOrig[!is.na(ageOrig)]

        checkboxGroupInput(ns("ageInputScore"),
                           "What ages do you want to calculate scores for?",
                           seq(round(min(ageOrig, na.rm =T)),round(max(ageOrig, na.rm =T))),
                           inline = TRUE)
      })

      # ------------------------------------------
      # Calculate the score at a given age (intercept + slope etc)
      score <- reactive({
        ageOrig <- modelDataEdit() %>% pull(age_original)
        ageOrig <- ageOrig[!is.na(ageOrig)]

        score <- sapply(as.numeric(input$ageInputScore), function(x){
          if(modelType() == "Linear"){
            (summary(fit())$coefficients[1,1] +
               (x - mean(ageOrig)) * summary(fit())$coefficients[2,1]) %>%
              round(2)
          } else if(modelType() == "Quadratic"){
            ( summary(fit())$coefficients[1,1] +
                (x - mean(ageOrig)) * summary(fit())$coefficients[2,1] +
                (x - mean(ageOrig))^2 * summary(fit())$coefficients[3,1]) %>%
              round(2)
          } else if(modelType() == "Cubic"){
            (summary(fit())$coefficients[1,1] +
               (x - mean(ageOrig)) * summary(fit())$coefficients[2,1] +
               (x - mean(ageOrig))^2 * summary(fit())$coefficients[3,1] +
               (x - mean(ageOrig))^3 * summary(fit())$coefficients[4,1] )%>%
              round(2)
          } else if(modelType() == "Quartic"){
            ( summary(fit())$coefficients[1,1] +
                (x - mean(ageOrig)) * summary(fit())$coefficients[2,1] +
                (x - mean(ageOrig))^2 * summary(fit())$coefficients[3,1] +
                (x - mean(ageOrig))^3 * summary(fit())$coefficients[4,1] +
                (x - mean(ageOrig))^4 * summary(fit())$coefficients[5,1]) %>%
              round(2)
          }
        })

        if(input$varType == "cat"){
          n <- length(unique(pull(modelDataScaled(), !!sym(input$condition))))
          rowIndex <- which(str_detect(string = row.names(summary(fit())$coefficients),
                                       pattern = input$condition) &
                              str_detect(string = row.names(summary(fit())$coefficients),
                                         pattern = ":", negate = T))

          rowIndexInteract1 <- which(str_detect(string = row.names(summary(fit())$coefficients),
                                                pattern = input$condition) &
                                       str_detect(string = row.names(summary(fit())$coefficients),
                                                  pattern = ":") &
                                       str_detect(string = row.names(summary(fit())$coefficients),
                                                  pattern = "\\^", negate = T))

          rowIndexInteract2 <- which(str_detect(string = row.names(summary(fit())$coefficients),
                                                pattern = input$condition) &
                                       str_detect(string = row.names(summary(fit())$coefficients),
                                                  pattern = ":") &
                                       str_detect(string = row.names(summary(fit())$coefficients),
                                                  pattern = "\\^2"))

          rowIndexInteract3 <- which(str_detect(string = row.names(summary(fit())$coefficients),
                                                pattern = input$condition) &
                                       str_detect(string = row.names(summary(fit())$coefficients),
                                                  pattern = ":") &
                                       str_detect(string = row.names(summary(fit())$coefficients),
                                                  pattern = "\\^3"))

          rowIndexInteract4 <- which(str_detect(string = row.names(summary(fit())$coefficients),
                                                pattern = input$condition) &
                                       str_detect(string = row.names(summary(fit())$coefficients),
                                                  pattern = ":") &
                                       str_detect(string = row.names(summary(fit())$coefficients),
                                                  pattern = "\\^4"))

          scoreCovs <- lapply(1:(n-1), function(i){
            sapply(as.numeric(input$ageInputScore), function(x){
              if(modelType() == "Linear"){
                scoreCov <-   summary(fit())$coefficients[1,1] +
                              summary(fit())$coefficients[rowIndex[i],1] +
                              (x - mean(ageOrig)) * summary(fit())$coefficients[2,1] +
                              ((x - mean(ageOrig)) * summary(fit())$coefficients[rowIndexInteract1[i],1])
                round(scoreCov, 2)
              } else if(modelType() == "Quadratic"){
                scoreCov <-  summary(fit())$coefficients[1,1] +
                  summary(fit())$coefficients[rowIndex[i],1] +
                  (x - mean(ageOrig)) * summary(fit())$coefficients[2,1] +
                  (x - mean(ageOrig))^2 * summary(fit())$coefficients[3,1] +
                  ((x - mean(ageOrig)) * summary(fit())$coefficients[rowIndexInteract1[i],1]) +
                  ((x - mean(ageOrig))^2 * summary(fit())$coefficients[rowIndexInteract2[i],1])
                round(scoreCov, 2)
              } else if(modelType() == "Cubic"){
                scoreCov <-  summary(fit())$coefficients[1,1] +
                  summary(fit())$coefficients[rowIndex[i],1] +
                  (x - mean(ageOrig)) * summary(fit())$coefficients[2,1] +
                  (x - mean(ageOrig))^2 * summary(fit())$coefficients[3,1] +
                  (x - mean(ageOrig))^3 * summary(fit())$coefficients[4,1] +
                  ((x - mean(ageOrig)) * summary(fit())$coefficients[rowIndexInteract1[i],1]) +
                  ((x - mean(ageOrig))^2 * summary(fit())$coefficients[rowIndexInteract2[i],1]) +
                  ((x - mean(ageOrig))^3 * summary(fit())$coefficients[rowIndexInteract3[i],1])
                round(scoreCov, 2)
              } else if(modelType() == "Quartic"){
                scoreCov <-  summary(fit())$coefficients[1,1] +
                             summary(fit())$coefficients[rowIndex[i],1]  +
                             (x - mean(ageOrig)) * summary(fit())$coefficients[2,1] +
                             (x - mean(ageOrig))^2 * summary(fit())$coefficients[3,1] +
                             (x - mean(ageOrig))^3 * summary(fit())$coefficients[4,1] +
                             (x - mean(ageOrig))^4 * summary(fit())$coefficients[5,1] +
                             ((x - mean(ageOrig)) * summary(fit())$coefficients[rowIndexInteract1[i],1]) +
                             ((x - mean(ageOrig))^2 * summary(fit())$coefficients[rowIndexInteract2[i],1]) +
                             ((x - mean(ageOrig))^3 * summary(fit())$coefficients[rowIndexInteract3[i],1]) +
                             ((x - mean(ageOrig))^4 * summary(fit())$coefficients[rowIndexInteract4[i],1])
                round(scoreCov, 2)
              }
            })
          })
          return( list(score = score, scoreCovs = scoreCovs) )
        }else{
          return(list(scoreCont = score))
        }
      })

      # ------------------------------------------
      # Plot the score at the given age

      plotScoreAll <- eventReactive(input$ageInputScore, {

        if(input$varType == "cat"){
          req(score()$scoreCovs)

          # points of intersection of age and score
          points <- data.frame(x = as.numeric(input$ageInputScore),
                               y = c(score()$score, unlist(score()$scoreCovs))
          )

          ggplot() +
            geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred, color = !!sym(input$condition) ) , na.rm=T) +
            theme(legend.text = element_text(color = "black")) +
            geom_point(data = points, aes(x = x, y = y), col = "#1D86C7", size = 5) +
            ylab(paste0("Score (", traj(), ")")) +
            xlab("Age")

        }else if(input$varType == "cont"){
          # points of intersection of age and score
          points <- data.frame(x = as.numeric(input$ageInputScore),
                               y = score()$scoreCont
          )

          req(score()$scoreCont)
          ggplot() +
            geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred ) , na.rm=T)+
            scale_colour_discrete(na.translate = F) +
            theme(legend.text = element_text(color = "black")) +
            geom_point(data = points, aes(x = x, y = y), col = "#1D86C7", size = 5) +
            ylab(paste0("Score (", traj(), ")")) +
            xlab("Age")
        }
      })
      output$plotScore <- renderPlot({
        plotScoreAll()
      })

      # ------------------------------------------
      # Return a table of the score for all the ages
      # --- Age | Score
      # Change "Score" to the actual column name from the dataframe - which the user previously specified
      tableScoreAll <- eventReactive(input$ageInputScore, {
        if(input$varType == "cat"){
          req(score()$scoreCovs)

          df <- t(
            cbind(
              data.frame( input$ageInputScore,
                          score()$score  ),
              do.call(cbind, score()$scoreCovs)
            )
          )
          rowname <- paste0("Score (", traj(), ")")
          levelNames <- as.character(levels(as.factor(pull(modelDataEdit(), input$condition))))
          rownames(df) <- c("Age", paste0(rowname, " [", input$condition, ", level = ", levelNames, " ]") )
          df

        }else if(input$varType == "cont"){
          req(score()$scoreCont)
          df <- t(data.frame( input$ageInputScore, score()$scoreCont ))
          rowname <- paste0("Score (", traj(), ") [", input$condition, "]")
          rownames(df) <- c("Age", rowname)
          df
        }
      })

      output$tableScore <- renderTable({
        tableScoreAll()
      }, colnames = FALSE, rownames = TRUE)

      # ------------------------------------------
      ###############################################################
      # --- AUC tab ------
      ###############################################################

      # ------------------------------------------
      # Overview of what AUC does text
      output$AUCoverview <- renderText({
        paste0("The area under curve (AUC) represents the proportion of time with phenotype (", traj(), ").")
      })

      # ------------------------------------------
      # Make age slider for user
      output$AUCagesUI <- renderUI({
        ageOrig <- modelDataEdit() %>%
          pull(age_original)
        ageOrig <- ageOrig[!is.na(ageOrig)]

        sliderInput(ns("AUCages"),
                    "Select the age range to calculate AUC for:",
                    min = round(min(ageOrig, na.rm =T)),
                    max = round(max(ageOrig, na.rm =T)),
                    value = c(round(min(ageOrig, na.rm =T)),round(max(ageOrig, na.rm =T)))
        )
      })

      # ------------------------------------------
      # Calculate the AUC for the ages the user has chosen for the chosen input$condition levels
      AUC <- reactive({
        coef <- summary(fit())$coefficients

        ageOrig <- modelDataEdit() %>%
          pull(age_original)
        ageOrig <- ageOrig[!is.na(ageOrig)]
        age1 <- input$AUCages[1] - mean(ageOrig)
        age2 <- input$AUCages[2] - mean(ageOrig)

        AUC <-
          if(modelType() == "Linear"){
            ((age2*coef[1,1]) + (coef[2,1]*age2^2/2)) - ((age1*coef[1,1]) + (coef[2,1]*age1^2/2)) %>%
              round(2)
          } else if(modelType() == "Quadratic"){
            ((age2*coef[1,1]) + (coef[2,1]*age2^2/2) + (coef[3,1]*age2^3/3)) - ((age1*coef[1,1]) + (coef[2,1]*age1^2/2) + (coef[3,1]*age1^3/3)) %>%
              round(2)
          } else if(modelType() == "Cubic"){
            ((age2*coef[1,1]) + (coef[2,1]*age2^2/2) + (coef[3,1]*age2^3/3) + (coef[4,1]*age2^4/4)) - ((age1*coef[1,1]) + (coef[2,1]*age1^2/2) + (coef[3,1]*age1^3/3) + (coef[4,1]*age1^4/4))  %>%
              round(2)
          } else if(modelType() == "Quartic"){
            ((age2*coef[1,1]) + (coef[2,1]*age2^2/2) + (coef[3,1]*age2^3/3) + (coef[4,1]*age2^4/4) + (coef[5,1]*age2^5/5)) - ((age1*coef[1,1]) + (coef[2,1]*age1^2/2) + (coef[3,1]*age1^3/3) + (coef[4,1]*age1^4/4) +  (coef[5,1]*age1^5/5)) %>%
              round(2)
          }

        if(input$varType == "cat"){
          n <- length(unique(pull(modelDataScaled(), !!sym(input$condition))))
          rowIndex <- which(str_detect(string = row.names(summary(fit())$coefficients),
                                       pattern = input$condition) &
                              str_detect(string = row.names(summary(fit())$coefficients),
                                         pattern = ":", negate = T))

          rowIndexInteract1 <- which(str_detect(string = row.names(summary(fit())$coefficients),
                                                pattern = input$condition) &
                                       str_detect(string = row.names(summary(fit())$coefficients),
                                                  pattern = ":") &
                                       str_detect(string = row.names(summary(fit())$coefficients),
                                                  pattern = "\\^", negate = T))

          rowIndexInteract2 <- which(str_detect(string = row.names(summary(fit())$coefficients),
                                                pattern = input$condition) &
                                       str_detect(string = row.names(summary(fit())$coefficients),
                                                  pattern = ":") &
                                       str_detect(string = row.names(summary(fit())$coefficients),
                                                  pattern = "\\^2"))

          rowIndexInteract3 <- which(str_detect(string = row.names(summary(fit())$coefficients),
                                                pattern = input$condition) &
                                       str_detect(string = row.names(summary(fit())$coefficients),
                                                  pattern = ":") &
                                       str_detect(string = row.names(summary(fit())$coefficients),
                                                  pattern = "\\^3"))

          rowIndexInteract4 <- which(str_detect(string = row.names(summary(fit())$coefficients),
                                                pattern = input$condition) &
                                       str_detect(string = row.names(summary(fit())$coefficients),
                                                  pattern = ":") &
                                       str_detect(string = row.names(summary(fit())$coefficients),
                                                  pattern = "\\^4"))

          AUCCovs <- lapply(1:(n-1), function(i){
            if(modelType() == "Linear"){
              AUC <- ((age2*coef[1,1]) + (coef[2,1]*age2^2/2) + (coef[rowIndex[i],1]*age2) + (summary(fit())$coefficients[rowIndexInteract1[i],1]*age2^2/2) ) -
                ((age1*coef[1,1]) + (coef[2,1]*age1^2/2) + (coef[rowIndex[i],1]*age1) + (summary(fit())$coefficients[rowIndexInteract1[i],1]*age1^2/2) ) %>%
                round(2)
            } else if(modelType() == "Quadratic"){
              AUC <- ((age2*coef[1,1]) + (coef[2,1]*age2^2/2) + (coef[3,1]*age2^3/3) + (coef[rowIndex[i],1]*age2) + (summary(fit())$coefficients[rowIndexInteract1[i],1]*age2^2/2) + (summary(fit())$coefficients[rowIndexInteract2[i],1]*age2^3/3) ) -
                ((age1*coef[1,1]) + (coef[2,1]*age1^2/2) + (coef[3,1]*age1^3/3) + (coef[rowIndex[i],1]*age1) + (summary(fit())$coefficients[rowIndexInteract1[i],1]*age1^2/2) + (summary(fit())$coefficients[rowIndexInteract2[i],1]*age1^3/3)) %>%
                round(2)
            } else if(modelType() == "Cubic"){
              AUC <- ((age2*coef[1,1]) + (coef[2,1]*age2^2/2) + (coef[3,1]*age2^3/3) + (coef[4,1]*age2^4/4) + (coef[rowIndex[i],1]*age2) + (summary(fit())$coefficients[rowIndexInteract1[i],1]*age2^2/2) + (summary(fit())$coefficients[rowIndexInteract2[i],1]*age2^3/3) + (summary(fit())$coefficients[rowIndexInteract3[i],1]*age2^4/4) ) -
                ((age1*coef[1,1]) + (coef[2,1]*age1^2/2) + (coef[3,1]*age1^3/3) + (coef[4,1]*age1^4/4) + (coef[rowIndex[i],1]*age1) + (summary(fit())$coefficients[rowIndexInteract1[i],1]*age1^2/2) + (summary(fit())$coefficients[rowIndexInteract2[i],1]*age1^3/3) + (summary(fit())$coefficients[rowIndexInteract3[i],1]*age1^4/4))  %>%
                round(2)
            } else if(modelType() == "Quartic"){
              AUC <- ((age2*coef[1,1]) + (coef[2,1]*age2^2/2) + (coef[3,1]*age2^3/3) + (coef[4,1]*age2^4/4) + (coef[5,1]*age2^5/5) + (coef[rowIndex[i],1]*age2)  + (summary(fit())$coefficients[rowIndexInteract1[i],1]*age2^2/2) + (summary(fit())$coefficients[rowIndexInteract2[i],1]*age2^3/3) + (summary(fit())$coefficients[rowIndexInteract3[i],1]*age2^4/4) + (summary(fit())$coefficients[rowIndexInteract4[i],1]*age2^5/5)) -
                ((age1*coef[1,1]) + (coef[2,1]*age1^2/2) + (coef[3,1]*age1^3/3) + (coef[4,1]*age1^4/4) +  (coef[5,1]*age1^5/5) + (coef[rowIndex[i],1]*age1) + (summary(fit())$coefficients[rowIndexInteract1[i],1]*age1^2/2) + (summary(fit())$coefficients[rowIndexInteract2[i],1]*age1^3/3) + (summary(fit())$coefficients[rowIndexInteract3[i],1]*age1^4/4) + (summary(fit())$coefficients[rowIndexInteract4[i],1]*age1^5/5)) %>%
                round(2)
            }
            AUC <- round(AUC, 2)
          })
          return( list(AUC = AUC, AUCCovs = AUCCovs) )
        }else{
          return(list(AUCCont = AUC))
        }
      })

      # ------------------------------------------
      # Plot AUC
      plotAUC <- eventReactive(c(input$button, input$AUCages), {

        if(input$varType == "cat"){
          req(AUC()$AUCCovs)

          ggplot(data = modelDataEdit()) +
            geom_ribbon(data = modelDataEdit(),
                        aes(x = age_original, ymax = pred, ymin = 0, fill = !!sym(input$condition)),
                        alpha = 0.1, show.legend = FALSE) +
            coord_cartesian(xlim = c(input$AUCages[1], input$AUCages[2])) +
            geom_line(aes(x = age_original, y = pred, color = !!sym(input$condition)), na.rm = TRUE) +
            theme(legend.text = element_text(color = "black")) +
            ylab(paste0("Score (", traj(), ")")) +
            xlab("Age") +
            scale_x_continuous(breaks = seq(round(min(modelDataEdit()$age_original, na.rm =T)), round(max(modelDataEdit()$age_original, na.rm =T)), by = 1),
                               expand = c(0, 0))+
            scale_y_continuous(expand = c(0, 0))


        }else if(input$varType == "cont"){

          req(AUC()$AUCCont)
          ggplot() +
            geom_ribbon(data = modelDataEdit(),
                        aes(x = age_original, ymax = pred, ymin = 0),
                        alpha = 0.1, show.legend = FALSE, fill = "#1D86C7") +
            geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred ) , na.rm=T)+
            coord_cartesian(xlim = c(input$AUCages[1], input$AUCages[2])) +
            scale_colour_discrete(na.translate = F) +
            theme(legend.text = element_text(color = "black")) +
            ylab(paste0("Score (", traj(), ")")) +
            xlab("Age") +
            scale_x_continuous(breaks = seq(round(min(modelDataEdit()$age_original, na.rm =T)), round(max(modelDataEdit()$age_original, na.rm =T)), by = 1),
                               expand = c(0, 0)) +
            scale_y_continuous(expand = c(0, 0))
        }
      })

      output$AUCplot <- renderPlot({
        plotAUC()
      })


      tableAUC <- eventReactive(c(input$button, input$AUCages), {
        if(input$varType == "cat"){
          req(AUC()$AUCCovs)
          df <- t(
            cbind(
              data.frame(paste0(input$AUCages[1], " - ", input$AUCages[2]),
                         round(AUC()$AUC, 2)),
              do.call(cbind, AUC()$AUCCovs)
            )
          )

          rowname <- paste0("AUC (", traj(), ")")
          levelNames <- as.character(levels(as.factor(pull(modelDataEdit(), input$condition))))
          rownames(df) <- c("Age Range", paste0(rowname, " [", input$condition, ", level = ", levelNames, " ]") )
          df

        }else if(input$varType == "cont"){
          req(AUC()$AUCCont)
          df <- t(
            data.frame(paste0(input$AUCages[1], " - ", input$AUCages[2]),
                       round(AUC()$AUC, 2))
          )
          rowname <- paste0("AUC (", traj(), ") [", input$condition, " ]")
          rownames(df) <- c("Age Range", rowname)
          df
        }
      })


      output$AUCtable <- renderTable({
        tableAUC()
      }, colnames = FALSE, rownames = TRUE)

      # ------
      # User select which 2 levels of their factor they want to see a difference between
      # if input var is categorical
      output$levelsAUCUI <- renderUI({
        if(input$varType == "cat"){
          if(length(unique(pull(modelDataEdit(), !!sym(input$condition)))) > 2){
            selectizeInput(ns("levelsAUC"), "Select two levels from your factor to calculate the difference in AUC between:",
                           sort(unique(pull(modelDataEdit(), !!sym(input$condition)))),
                           multiple = TRUE,
                           options = list(maxItems = 2))
          }else if(length(unique(pull(modelDataEdit(), !!sym(input$condition)))) == 2){
            selectizeInput(ns("levelsAUC"), "Select two levels from your factor to calculate the difference in AUC between:",
                           sort(unique(pull(modelDataEdit(), !!sym(input$condition)))),
                           multiple = TRUE,
                           options = list(maxItems = 2),
                           selected = sort(unique(pull(modelDataEdit(), !!sym(input$condition)))))
          }
        }else{

        }
      })

      difference <- reactive({
        levelChoice <- as.character(input$levelsAUC)
        abs(as.numeric( tableAUC()[str_detect(row.names(tableAUC()), paste0("level = ", levelChoice[1])),1] ) - as.numeric(tableAUC()[str_detect(row.names(tableAUC()), paste0("level = ", levelChoice[2])),1]))
      })

      output$test <- renderText({
        if(input$varType == "cat" & length(difference() == 2)){
          paste0("The difference between the two factor levels and the age range you selected is: ", round( difference() ,2) ,". Please note this section is in development, it will change to a table output with a statistical test summary.")
        }else{

        }
      })


      return(list(
        modelDataEdit = modelDataEdit
      ))

    }
  )
}

