#' split model by variable
#'
#' @import broom.mixed
#' @import lme4
#' @import dplyr
#' @import ggplot2
#' @import data.table
#' @import shinyjs
#' @import tidyr
#' @import multcomp
#' @import tibble
#' @import purrr
#' @import tinytex
#' @import rmarkdown
#' @import kableExtra
#' @import car
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

      # ---------------------------------------
      # Allow the user to select a variable from the column names of the dataset
      # Note if the user selects categorical then only columns with less than 40 unique values are shown (as more than this is messy and likely not to be a categorical variable as it's unusual to have that many categories)
      # If the user selects continuous then only columns with more than 2 unique values are shown, ie. this removes things like males/females and other binary categorical variables.
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
      # z-scale continuous variable
      # either factorise or make numeric the input$condition depending on the input$varType option
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
      # Run lme4
      fit <- eventReactive(input$button,{

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

      # ---------------------------------------
      # Add columns to the data frame

      modelDataEdit <- eventReactive(input$button,{

        # select the index for the column that the user wants to split the analysis on
        colSplit <- which(colnames(modelDataScaled()) %in% input$condition)

        # add the "predicted" column to this dataset (it's not really a prediction because its the same dataset, it just shows the model)

        ageVec <- modelDataScaled() %>% pull(!!age())

        coef <- summary(fit())$coefficients

        if(modelType() == "Linear"){
          zero <- ageVec * coef[2,1] + coef[1,1]

        } else if(modelType() == "Quadratic"){
          zero  <- ageVec * coef[2,1] + coef[1,1] +
            ageVec^2 * coef[3,1]

        } else if(modelType() == "Cubic"){
          zero  <- ageVec * coef[2,1] + coef[1,1]  +
            ageVec^2 * coef[3,1] +
            ageVec^3 * coef[4,1]

        } else if(modelType() == "Quartic"){
          zero  <- ageVec * coef[2,1] + coef[1,1]  +
            ageVec^2 * coef[3,1] +
            ageVec^3 * coef[4,1] +
            ageVec^4 * coef[5,1]

        }

        rowIndex <- which(str_detect(string = row.names(coef),
                                     pattern = input$condition) &
                            str_detect(string = row.names(coef),
                                       pattern = ":", negate = T))

        rowIndexInteract1 <- which(str_detect(string = row.names(coef),
                                              pattern = input$condition) &
                                     str_detect(string = row.names(coef),
                                                pattern = ":") &
                                     str_detect(string = row.names(coef),
                                                pattern = "\\^", negate = T))

        rowIndexInteract2 <- which(str_detect(string = row.names(coef),
                                              pattern = input$condition) &
                                     str_detect(string = row.names(coef),
                                                pattern = ":") &
                                     str_detect(string = row.names(coef),
                                                pattern = "\\^2"))

        rowIndexInteract3 <- which(str_detect(string = row.names(coef),
                                              pattern = input$condition) &
                                     str_detect(string = row.names(coef),
                                                pattern = ":") &
                                     str_detect(string = row.names(coef),
                                                pattern = "\\^3"))

        rowIndexInteract4 <- which(str_detect(string = row.names(coef),
                                              pattern = input$condition) &
                                     str_detect(string = row.names(coef),
                                                pattern = ":") &
                                     str_detect(string = row.names(coef),
                                                pattern = "\\^4"))

        if(input$varType == "cat"){
          n <- length(unique(pull(modelDataScaled(), !!sym(input$condition))))

          predCovs <- lapply(1:(n-1), function(i){
            if(modelType() == "Linear"){
              coef[1,1] +
              (ageVec * coef[2,1]) +
              coef[rowIndex[i],1] +
              (ageVec * coef[rowIndexInteract1[i],1])
            } else if(modelType() == "Quadratic"){
              coef[1,1] +
              ageVec * coef[2,1] +
              coef[rowIndex[i],1] +
              ageVec^2 * coef[3,1] +
              (ageVec * coef[rowIndexInteract1[i],1]) +
              (ageVec^2 * coef[(rowIndexInteract2)[i],1])
            } else if(modelType() == "Cubic"){
              coef[1,1] +
              ageVec * coef[2,1] +
              coef[rowIndex[i],1] +
              ageVec^2 * coef[3,1] +
              ageVec^3 * coef[4,1] +
              (ageVec * coef[rowIndexInteract1[i],1]) +
              (ageVec^2 * coef[(rowIndexInteract2)[i],1]) +
              (ageVec^3 * coef[(rowIndexInteract3)[i],1])
            } else if(modelType() == "Quartic"){
              coef[1,1] +
              ageVec * coef[2,1] +
                coef[rowIndex[i],1]  +
                ageVec^2 * coef[3,1] +
                ageVec^3 * coef[4,1] +
                ageVec^4 * coef[5,1] +
                (ageVec * coef[rowIndexInteract1[i],1]) +
                (ageVec^2 * coef[(rowIndexInteract2)[i],1]) +
                (ageVec^3 * coef[(rowIndexInteract3)[i],1]) +
                (ageVec^4 * coef[(rowIndexInteract4)[i],1])
            }
          })


          num <- str_subset(row.names(coef), input$condition) %>%
            str_sub(-1)%>%
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
        # Get the ±1 SD for this predict/population average line
          if(modelType() == "Linear"){
            plus <- ageVec * coef[2,1] +
              coef[1,1] +
              coef[rowIndex,1]  +
              (ageVec * coef[rowIndexInteract1,1])

            minus <- ageVec * coef[2,1] +
              coef[1,1] -
              coef[rowIndex,1] -
              (ageVec * coef[rowIndexInteract1,1])

          } else if(modelType() == "Quadratic"){
            plus  <- ageVec * coef[2,1] +
              coef[1,1] +
              ageVec^2 * coef[3,1] +
              coef[rowIndex,1] +
              (ageVec * coef[rowIndexInteract1,1]) +
              (ageVec^2 * coef[(rowIndexInteract2),1])

            minus <- ageVec * coef[2,1] +
              coef[1,1] +
              ageVec^2 * coef[3,1] -
              coef[rowIndex,1] -
              (ageVec * coef[rowIndexInteract1,1]) -
              (ageVec^2 * coef[(rowIndexInteract2),1])

          } else if(modelType() == "Cubic"){
            plus  <- ageVec * coef[2,1] +
              coef[1,1]  +
              ageVec^2 * coef[3,1] +
              ageVec^3 * coef[4,1] +
              coef[rowIndex,1] +
              (ageVec * coef[rowIndexInteract1,1]) +
              (ageVec^2 * coef[(rowIndexInteract2),1]) +
              (ageVec^3 * coef[(rowIndexInteract3),1])

            minus <-  ageVec * coef[2,1] +
              coef[1,1]  +
              ageVec^2 * coef[3,1] +
              ageVec^3 * coef[4,1] -
              coef[rowIndex,1] -
              (ageVec * coef[rowIndexInteract1,1]) -
              (ageVec^2 * coef[(rowIndexInteract2),1]) -
              (ageVec^3 * coef[(rowIndexInteract3),1])

          } else if(modelType() == "Quartic"){
            plus  <- ageVec * coef[2,1] +
              coef[1,1]  +
              ageVec^2 * coef[3,1] +
              ageVec^3 * coef[4,1] +
              ageVec^4 * coef[5,1] +
              coef[rowIndex,1] +
              (ageVec * coef[rowIndexInteract1,1]) +
              (ageVec^2 * coef[(rowIndexInteract2),1]) +
              (ageVec^3 * coef[(rowIndexInteract3),1]) +
              (ageVec^4 * coef[(rowIndexInteract4),1])

            minus <-  ageVec * coef[2,1] +
              coef[1,1]  +
              ageVec^2 * coef[3,1] +
              ageVec^3 * coef[4,1] +
              ageVec^4 * coef[5,1] -
              coef[rowIndex,1] -
              (ageVec * coef[rowIndexInteract1,1]) -
              (ageVec^2 * coef[(rowIndexInteract2),1]) -
              (ageVec^3 * coef[(rowIndexInteract3),1]) -
              (ageVec^4 * coef[(rowIndexInteract4),1])
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
      modelStatsFixed <- reactive({
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

      output$modelStatsFixed <- renderTable({
        modelStatsFixed()
        }, digits = 3)

      modelStatsRandom <- reactive({
        if(str_detect(formCodeCovars(), input$condition)){
          data.frame(NULL)
        }else{
          as.data.frame(VarCorr(fit()),
                        order = "lower.tri")
        }
      })

      output$modelStatsRandom <- renderTable({
        modelStatsRandom()
        }, digits = 3)

      # ---------------------------------------
      # Paste the model formula for the user to see (don't want it to appear straight away - could improve this)
      modelform <- reactive({
        if(str_detect(formCodeCovars(), input$condition)){
          ""
        }else{
          paste0("<b>Model Formula:</b> ",  gsub(".*formula = (.+) , data =.*", "\\1", summary(fit())$call)[2])
        }
      })

      output$form<- renderText({
        modelform()
      })

      # ---------------------------------------
      # Checkbox for which 95% CIs to display (categorical only)

      observeEvent(c(input$button), {
        if(input$varType == "cat"){
          levelNames <- as.character(levels(as.factor(pull(modelDataEdit(), !!sym(input$condition)))))
          output$plotCheckboxLevelsUI <- renderUI({
          checkboxGroupInput(ns("plotCheckboxLevels"),
                             "Display 95% confidence intervals for:",
                             choices = levelNames)
          })
        }else{
          output$plotCheckboxLevelsUI <- renderUI({})
        }
      })

      # ---------------------------------------
      # use glht to calculate 95% CIs
      CI_glht <- reactive({
        ageOrig <- modelDataEdit() %>% pull(age_original)
        ageOrig <- ageOrig[!is.na(ageOrig)]
        ageCalcs <-  c(min(ageOrig), seq(round(min(ageOrig), 1), round(max(ageOrig), 1), 0.5), max(ageOrig) )

        score <- lapply(ageCalcs, function(x){

          if(input$varType == "cat"){
            n <- length(unique(pull(modelDataEdit(), !!sym(input$condition))))
          }

          rowIndex <- which(str_detect(string = row.names(summary(fit())$coefficients),
                                       pattern = input$condition) &
                              str_detect(string = row.names(summary(fit())$coefficients),
                                         pattern = ":", negate = T))

          rowNames <- rownames(summary(fit())$coefficients)

          ageInput <- round(x - mean(ageOrig), 3)
          ageInput2 <- ageInput^2
          ageInput3 <- ageInput^3
          ageInput4 <- ageInput^4
          # --------------------
          # Change for model type
          if(modelType() == "Linear"){
            if(input$varType == "cat"){
              equations <- sapply(1:(n-1), function(i){
                paste0(rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[rowIndex[i]] , " + ", rowNames[2], ":", rowNames[rowIndex[i]],"*",ageInput , " == 0")
              })
            }

            res <- multcomp::glht(fit(), linfct = c( paste0(rowNames[1], " + ", rowNames[2], "*", ageInput, " == 0"), equations) )

          }else if(modelType() == "Quadratic"){
            if(input$varType == "cat"){
              equations <- sapply(1:(n-1), function(i){
                paste0(rowNames[1], " + ",
                       rowNames[rowIndex[i]] , " + ",
                       rowNames[2], "*", ageInput, " + \`",
                       rowNames[3], "\`*", ageInput2, " + ",
                       rowNames[2], ":", rowNames[rowIndex[i]],"*",ageInput , " + \`",
                       rowNames[3], ":", rowNames[rowIndex[i]],"\`*",ageInput2 , "",
                       " == 0")
              })

            }

            res <- glht(fit(), linfct = c( paste0(rowNames[1], " + ",
                                                  rowNames[2], "*", ageInput," + \`",
                                                  rowNames[3], "\`*", ageInput2,
                                                  " == 0"),
                                           equations) )
          } else if(modelType() == "Cubic"){
            if(input$varType == "cat"){
              equations <- sapply(1:(n-1), function(i){
                paste0(rowNames[1], " + ",
                       rowNames[rowIndex[i]] , " + ",
                       rowNames[2], "*", ageInput, " + \`",
                       rowNames[3], "\`*", ageInput2, " + \`",
                       rowNames[4], "\`*", ageInput3,  " + ",
                       rowNames[2], ":", rowNames[rowIndex[i]],"*",ageInput , " + \`",
                       rowNames[3], ":", rowNames[rowIndex[i]],"\`*",ageInput2 , " + \`",
                       rowNames[4], ":", rowNames[rowIndex[i]],"\`*",ageInput3 ,
                       " == 0")
              })

            }

            res <- glht(fit(), linfct = c( paste0(rowNames[1], " + ",
                                                  rowNames[2], "*", ageInput," + \`",
                                                  rowNames[3], "\`*", ageInput2,  " + \`",
                                                  rowNames[4], "\`*", ageInput3,
                                                  " == 0"),
                                           equations) )

          } else if(modelType() == "Quartic"){
            if(input$varType == "cat"){
              equations <- sapply(1:(n-1), function(i){
                paste0(rowNames[1], " + ",
                       rowNames[rowIndex[i]] , " + ",
                       rowNames[2], "*", ageInput, " + \`",
                       rowNames[3], "\`*", ageInput2, " + \`",
                       rowNames[4], "\`*", ageInput3,  " + \`",
                       rowNames[5], "\`*", ageInput4,  " + ",
                       rowNames[2], ":", rowNames[rowIndex[i]],"*",ageInput , " + \`",
                       rowNames[3], ":", rowNames[rowIndex[i]],"\`*",ageInput2 , " + \`",
                       rowNames[4], ":", rowNames[rowIndex[i]],"\`*",ageInput3 , " + \`",
                       rowNames[5], ":", rowNames[rowIndex[i]],"\`*",ageInput4 ,
                       " == 0")
              })
            }

            res <- glht(fit(), linfct = c( paste0(rowNames[1], " + ",
                                                  rowNames[2], "*", ageInput," + \`",
                                                  rowNames[3], "\`*", ageInput2,  " + \`",
                                                  rowNames[4], "\`*", ageInput3,  " + \`",
                                                  rowNames[5], "\`*", ageInput4,
                                                  " == 0"),
                                           equations) )
          }

          # --------------------
          # Tidy results dataframe and rename rows/columns
          res <- tidy(confint(res))

          rowname <- paste0("Score (", traj(), ")")

          if(input$varType == "cat"){
            levelNames <- as.character(levels(as.factor(pull(modelDataEdit(), !!sym(input$condition)))))

            res <-  res %>%
              mutate(condition = levelNames)
          }
          return( res )
        })
        return(score)
      })


      # Plot the split by variable plot
      plot <- eventReactive(c(input$button, input$plotCheckbox, input$plotCheckboxLevels), {

         if(input$varType == "cat"){
        req(CI_glht())

        ageOrig <- modelDataEdit() %>% pull(age_original)
        ageOrig <- ageOrig[!is.na(ageOrig)]
        ageCalcs <-  c(min(ageOrig), seq(round(min(ageOrig), 1), round(max(ageOrig), 1), 0.5), max(ageOrig) )
        levelNames <- as.character(levels(as.factor(pull(modelDataEdit(), !!sym(input$condition)))))

        estimate <- do.call(rbind, CI_glht()) %>%
          mutate(age = rep(ageCalcs, each = length(levelNames)) )

         }

        # Checkbox = TRUE means the descriptive stats plot is displayed above too
        # ±1 SD is plotted for continuous, categorical is split by the factor levels
        if(input$plotCheckbox == TRUE){
          if(input$varType == "cat"){
            # Create a subset of the color palette based on the selected levels
            levelNames <- as.character(levels(as.factor(pull(modelDataEdit(), !!sym(input$condition)))))
            selected_colors <- getOption("ggplot2.discrete.colour")[which(levelNames %in% input$plotCheckboxLevels)]

            ggplot() +
              geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred, color = !!sym(input$condition) ) , na.rm=T) +
              geom_ribbon(data = filter(estimate, condition %in% as.character(input$plotCheckboxLevels) ) ,
                          aes(x= age , ymin = conf.low, ymax = conf.high, fill = condition), alpha = 0.2, na.rm = T, show.legend = FALSE) +
              scale_color_manual(values = getOption("ggplot2.discrete.colour")) +
              scale_fill_manual(values = selected_colors) +
              geom_point(data = dfPlot(),aes(x=Age, y=Phenotype))+
              geom_line(data = dfPlot(),aes(x=Age, y=Phenotype)) +
              geom_errorbar(data = dfPlot(), aes(x=Age, y=Phenotype, ymin = lower, ymax = upper)) +
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
            # Create a subset of the color palette based on the selected levels
            levelNames <- as.character(levels(as.factor(pull(modelDataEdit(), !!sym(input$condition)))))
            selected_colors <- getOption("ggplot2.discrete.colour")[which(levelNames %in% input$plotCheckboxLevels)]

            ggplot() +
              geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred, color = !!sym(input$condition) ) , na.rm=T) +
              geom_ribbon(data = filter(estimate, condition %in% as.character(input$plotCheckboxLevels) ) ,
                          aes(x= age , ymin = conf.low, ymax = conf.high, fill = condition), alpha = 0.2, na.rm = T, show.legend = FALSE) +
              scale_color_manual(values = getOption("ggplot2.discrete.colour")) +
              scale_fill_manual(values = selected_colors) +
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
      # --- Score for a given set of ages -----
      # ------------------------------------------
      # Allow the user to select the ages they want to calculate scores for
      output$selectAgeScore <- renderUI({
        ageOrig <- modelDataEdit() %>%
          pull(age_original)
        ageOrig <- ageOrig[!is.na(ageOrig)]

        checkboxGroupInput(ns("ageInputScore"),
                           "What ages do you want to calculate scores for?",
                           seq(ceiling(min(ageOrig, na.rm =T)),floor(max(ageOrig, na.rm =T))),
                           inline = TRUE)
      })


      # ------------------------------------------
      # use glht to calculate scores at ages
      score_glht <- reactive({
        ageOrig <- modelDataEdit() %>% pull(age_original)
        ageOrig <- ageOrig[!is.na(ageOrig)]

        score <- lapply(as.numeric(input$ageInputScore), function(x){

          if(input$varType == "cat"){
          n <- length(unique(pull(modelDataEdit(), !!sym(input$condition))))
          }

          rowIndex <- which(str_detect(string = row.names(summary(fit())$coefficients),
                                       pattern = input$condition) &
                              str_detect(string = row.names(summary(fit())$coefficients),
                                         pattern = ":", negate = T))

          rowNames <- rownames(summary(fit())$coefficients)

          ageInput <- round(x - mean(ageOrig), 3)
          ageInput2 <- ageInput^2
          ageInput3 <- ageInput^3
          ageInput4 <- ageInput^4
          # --------------------
          # Change for model type
          if(modelType() == "Linear"){
            if(input$varType == "cat"){
              equations <- sapply(1:(n-1), function(i){
                paste0(rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[rowIndex[i]] , " + ", rowNames[2], ":", rowNames[rowIndex[i]],"*",ageInput , " == 0")
              })
            }else if(input$varType == "cont"){
              equations <- c(paste0(rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[rowIndex] , " + ", rowNames[2], ":", rowNames[rowIndex],"*",ageInput , " == 0") ,
                             paste0(rowNames[1], " + ", rowNames[2], "*", ageInput, " - ", rowNames[rowIndex] , " - ", rowNames[2], ":", rowNames[rowIndex],"*",ageInput , " == 0") )
            }

            res <- multcomp::glht(fit(), linfct = c( paste0(rowNames[1], " + ", rowNames[2], "*", ageInput, " == 0"), equations) )

          }else if(modelType() == "Quadratic"){
            if(input$varType == "cat"){
            equations <- sapply(1:(n-1), function(i){
              paste0(rowNames[1], " + ",
                     rowNames[rowIndex[i]] , " + ",
                     rowNames[2], "*", ageInput, " + \`",
                     rowNames[3], "\`*", ageInput2, " + ",
                     rowNames[2], ":", rowNames[rowIndex[i]],"*",ageInput , " + \`",
                     rowNames[3], ":", rowNames[rowIndex[i]],"\`*",ageInput2 , "",
                     " == 0")
            })

            }else if(input$varType == "cont"){
              equations <- c(paste0(rowNames[1], " + ",
                       rowNames[rowIndex] , " + ",
                       rowNames[2], "*", ageInput, " + \`",
                       rowNames[3], "\`*", ageInput2, " + ",
                       rowNames[2], ":", rowNames[rowIndex],"*",ageInput , " + \`",
                       rowNames[3], ":", rowNames[rowIndex],"\`*",ageInput2 , "",
                       " == 0"),
                paste0(rowNames[1], " - ",
                       rowNames[rowIndex] , " + ",
                       rowNames[2], "*", ageInput, " + \`",
                       rowNames[3], "\`*", ageInput2, " - ",
                       rowNames[2], ":", rowNames[rowIndex],"*",ageInput , " - \`",
                       rowNames[3], ":", rowNames[rowIndex],"\`*",ageInput2 , "",
                       " == 0"))
            }

            res <- glht(fit(), linfct = c( paste0(rowNames[1], " + ",
                                                rowNames[2], "*", ageInput," + \`",
                                                rowNames[3], "\`*", ageInput2,
                                                " == 0"),
                                         equations) )
           } else if(modelType() == "Cubic"){
             if(input$varType == "cat"){
               equations <- sapply(1:(n-1), function(i){
                 paste0(rowNames[1], " + ",
                        rowNames[rowIndex[i]] , " + ",
                        rowNames[2], "*", ageInput, " + \`",
                        rowNames[3], "\`*", ageInput2, " + \`",
                        rowNames[4], "\`*", ageInput3,  " + ",
                        rowNames[2], ":", rowNames[rowIndex[i]],"*",ageInput , " + \`",
                        rowNames[3], ":", rowNames[rowIndex[i]],"\`*",ageInput2 , " + \`",
                        rowNames[4], ":", rowNames[rowIndex[i]],"\`*",ageInput3 ,
                        " == 0")
               })

             }else if(input$varType == "cont"){
               equations <-
                 c(paste0(rowNames[1], " + ",
                        rowNames[rowIndex] , " + ",
                        rowNames[2], "*", ageInput, " + \`",
                        rowNames[3], "\`*", ageInput2, " + \`",
                        rowNames[4], "\`*", ageInput3,  " + ",
                        rowNames[2], ":", rowNames[rowIndex],"*",ageInput , " + \`",
                        rowNames[3], ":", rowNames[rowIndex],"\`*",ageInput2 , " + \`",
                        rowNames[4], ":", rowNames[rowIndex],"\`*",ageInput3 ,
                        " == 0"),
                   paste0(rowNames[1], " - ",
                          rowNames[rowIndex] , " + ",
                          rowNames[2], "*", ageInput, " + \`",
                          rowNames[3], "\`*", ageInput2, " + \`",
                          rowNames[4], "\`*", ageInput3,  " - ",
                          rowNames[2], ":", rowNames[rowIndex],"*",ageInput , " - \`",
                          rowNames[3], ":", rowNames[rowIndex],"\`*",ageInput2 , " - \`",
                          rowNames[4], ":", rowNames[rowIndex],"\`*",ageInput3 ,
                          " == 0"))

             }

             res <- glht(fit(), linfct = c( paste0(rowNames[1], " + ",
                                                 rowNames[2], "*", ageInput," + \`",
                                                 rowNames[3], "\`*", ageInput2,  " + \`",
                                                 rowNames[4], "\`*", ageInput3,
                                                 " == 0"),
                                          equations) )

           } else if(modelType() == "Quartic"){
             if(input$varType == "cat"){
               equations <- sapply(1:(n-1), function(i){
                 paste0(rowNames[1], " + ",
                        rowNames[rowIndex[i]] , " + ",
                        rowNames[2], "*", ageInput, " + \`",
                        rowNames[3], "\`*", ageInput2, " + \`",
                        rowNames[4], "\`*", ageInput3,  " + \`",
                        rowNames[5], "\`*", ageInput4,  " + ",
                        rowNames[2], ":", rowNames[rowIndex[i]],"*",ageInput , " + \`",
                        rowNames[3], ":", rowNames[rowIndex[i]],"\`*",ageInput2 , " + \`",
                        rowNames[4], ":", rowNames[rowIndex[i]],"\`*",ageInput3 , " + \`",
                        rowNames[5], ":", rowNames[rowIndex[i]],"\`*",ageInput4 ,
                        " == 0")
               })

             }else if(input$varType == "cont"){
               equations <- c(
                 paste0(rowNames[1], " + ",
                        rowNames[rowIndex] , " + ",
                        rowNames[2], "*", ageInput, " + \`",
                        rowNames[3], "\`*", ageInput2, " + \`",
                        rowNames[4], "\`*", ageInput3,  " + \`",
                        rowNames[5], "\`*", ageInput4,  " + ",
                        rowNames[2], ":", rowNames[rowIndex],"*",ageInput , " + \`",
                        rowNames[3], ":", rowNames[rowIndex],"\`*",ageInput2 , " + \`",
                        rowNames[4], ":", rowNames[rowIndex],"\`*",ageInput3 , " + \`",
                        rowNames[5], ":", rowNames[rowIndex],"\`*",ageInput4 ,
                        " == 0"),
                 paste0(rowNames[1], " - ",
                        rowNames[rowIndex] , " + ",
                        rowNames[2], "*", ageInput, " + \`",
                        rowNames[3], "\`*", ageInput2, " + \`",
                        rowNames[4], "\`*", ageInput3,  " + \`",
                        rowNames[5], "\`*", ageInput4,  " - ",
                        rowNames[2], ":", rowNames[rowIndex],"*",ageInput , " + \`",
                        rowNames[3], ":", rowNames[rowIndex],"\`*",ageInput2 , " - \`",
                        rowNames[4], ":", rowNames[rowIndex],"\`*",ageInput3 , " - \`",
                        rowNames[5], ":", rowNames[rowIndex],"\`*",ageInput4 ,
                        " == 0"))
             }

             res <- glht(fit(), linfct = c( paste0(rowNames[1], " + ",
                                                   rowNames[2], "*", ageInput," + \`",
                                                   rowNames[3], "\`*", ageInput2,  " + \`",
                                                   rowNames[4], "\`*", ageInput3,  " + \`",
                                                   rowNames[5], "\`*", ageInput4,
                                                   " == 0"),
                                            equations) )
          }

          # --------------------
          # Tidy results dataframe and rename rows/columns
          res <- tidy(confint(res))

          rowname <- paste0("Score (", traj(), ")")

          if(input$varType == "cat"){
          levelNames <- as.character(levels(as.factor(pull(modelDataEdit(), !!sym(input$condition)))))

          res <-  res %>%
            mutate(contrast = paste0(rowname, " [", input$condition, ", level = ", levelNames, " ] (95% CIs)")) %>%
            column_to_rownames(var = "contrast") %>%
            mutate(across(where(is.numeric), round, 2))
          }else if(input$varType == "cont"){
            res <-  res %>%
              mutate(contrast = paste0(paste0(rowname, " [", input$condition, " ] "), c("Population Average", "+ 1 SD", "- 1 SD") ) ) %>%
              column_to_rownames(var = "contrast") %>%
              mutate(across(where(is.numeric), round, 2))
          }

          return( res )
        })
        return(score)
      })


      # ------------------------------------------
      # Plot the score at the given age

      plotScoreAll <- eventReactive(c(input$ageInputScore,input$button), {


          req(score_glht())

          estimate <- lapply(score_glht(), function(df) {
            df %>%
              dplyr::select(estimate)
          })  %>% do.call(cbind, .)
          colnames(estimate) <- input$ageInputScore
          if(!is.null(estimate)){
            estimate <- estimate %>%
              gather(age, score, 1:ncol(estimate)) %>%
              mutate(age = as.numeric(age))

            conf.low <- lapply(score_glht(), function(df) {df %>% dplyr::select(conf.low)}) %>% do.call(cbind, .)
            colnames(conf.low) <- input$ageInputScore
            conf.low  <- conf.low %>%
              gather(age, conf.low, 1:ncol(conf.low)) %>%
              mutate(age = as.numeric(age))

            conf.high <- lapply(score_glht(), function(df) {df %>% dplyr::select(conf.high)}) %>% do.call(cbind, .)
            colnames(conf.high) <- input$ageInputScore
            conf.high  <- conf.high %>%
              gather(age, conf.high, 1:ncol(conf.high)) %>%
              mutate(age = as.numeric(age))%>%
              dplyr::select(-age)

            conf <- cbind(conf.low, conf.high, "age")

            if(input$varType == "cat"){
              ggplot() +
                geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred, color = !!sym(input$condition) ) , na.rm=T) +
                theme(legend.text = element_text(color = "black")) +
                geom_errorbar(data = conf, aes(x = age, ymin = conf.low, ymax = conf.high), width = 0.5) +
                geom_point(data = estimate, aes(x = age, y = score), col = "#1D86C7", size = 5) +
                ylab(paste0("Score (", traj(), ")")) +
                xlab("Age")
            }else if(input$varType == "cont"){
              ggplot() +
                geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred, color = "Population Average" ) , linewidth = 1.5, na.rm=T)+
                geom_line(data = modelDataEdit(), aes(x= age_original ,  y = plus, color = "+ 1 SD" ) , na.rm=T)+
                geom_line(data = modelDataEdit(), aes(x= age_original ,  y = minus, color = "- 1 SD" ) , na.rm=T) +
                theme(legend.text = element_text(color = "black"))+
                geom_errorbar(data = conf, aes(x = age, ymin = conf.low, ymax = conf.high), width = 0.5) +
                geom_point(data = estimate, aes(x = age, y = score), col = "#1D86C7", size = 5) +
                labs(color = "") +
                scale_color_manual(
                  breaks = c("+ 1 SD", "Population Average", "- 1 SD"),
                  values = c("#d55e00", "black", "#0072b2")) +
                ylab(paste0("Score (", traj(), ")")) +
                xlab("Age")
            }
          }

      })
      output$plotScore <- renderPlot({
        plotScoreAll()
      })

      # ------------------------------------------
      # Return a table of the score for all the ages
      # --- Age | Score
      # Change "Score" to the actual column name from the dataframe - which the user previously specified
      tableScoreAll <- eventReactive(c(input$ageInputScore,input$button), {
          req(score_glht())

          estimateCI <- lapply(score_glht(), function(df) {
            df %>%
              mutate(estimateCI = paste0(estimate, " (", conf.low, " - ", conf.high, ")")) %>%
              dplyr::select(estimateCI)
          })  %>% do.call(cbind, .)
          colnames(estimateCI) <- input$ageInputScore
          estimateCI
      })

      output$tableScore <- renderTable({
        tableScoreAll()
      }, colnames =TRUE, rownames = TRUE)

      # ------------------------------------------
      # Difference in scores at ages (with 95% CIs)

      # User select which 2 levels of their factor they want to see a difference between
      # if input var is categorical
      output$levelsScoresUI <- renderUI({
        if(input$varType == "cat"){
          if(length(unique(pull(modelDataEdit(), !!sym(input$condition)))) > 2){
            selectizeInput(ns("levelsScores"), "Select two levels from your factor to calculate the difference in scores:",
                           sort(unique(pull(modelDataEdit(), !!sym(input$condition)))),
                           multiple = TRUE,
                           options = list(maxItems = 2))
          }else if(length(unique(pull(modelDataEdit(), !!sym(input$condition)))) == 2){
            selectizeInput(ns("levelsScores"), "Select two levels from your factor to calculate the difference in scores:",
                           sort(unique(pull(modelDataEdit(), !!sym(input$condition)))),
                           multiple = TRUE,
                           options = list(maxItems = 2),
                           selected = sort(unique(pull(modelDataEdit(), !!sym(input$condition)))))
          }
        }else{

        }
      })

      differenceScores <- reactive({
        if(input$varType == "cat" & length(input$levelsScores == 2)){

          ageOrig <- modelDataEdit() %>% pull(age_original)
          ageOrig <- ageOrig[!is.na(ageOrig)]

          statements <- lapply(as.numeric(input$ageInputScore), function(x){
            ageInput <- round(x - mean(ageOrig), 3)
            ageInput2 <- ageInput^2
            ageInput3 <- ageInput^3
            ageInput4 <- ageInput^4

            coef <- summary(fit())$coefficients

            rowIndex <- which(str_detect(string = row.names(coef),
                                         pattern = input$condition) &
                                str_detect(string = row.names(coef),
                                           pattern = ":", negate = T))

            rowNames <- rownames(coef) %>%
              str_remove_all("I|\\(|\\^|\\)|\\:")

            levelNames <- paste0(input$condition,input$levelsScores) %>%
              str_remove_all("I|\\(|\\^|\\)|\\:")

            if( sum(str_detect(rowNames, levelNames[1])) == sum(str_detect(rowNames, levelNames[2])) ){

              levelNames1 <- rowNames[str_detect(rowNames, levelNames[1])]
              levelNames2 <- rowNames[str_detect(rowNames, levelNames[2])]

              if(modelType() == "Linear"){

                res <- deltaMethod(fit(), c(paste0(
                  "(", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", levelNames1[1], " + ", levelNames1[2], "*", ageInput, ") - (",rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", levelNames2[1], " + ", levelNames2[2], "*", ageInput, ")"
                )), parameterNames = rowNames )

              }else if(modelType() == "Quadratic"){
                res <- deltaMethod(fit(), c(paste0(
                  "(", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2, " + ", levelNames1[1], " + ", levelNames1[2], "*", ageInput, " + ", levelNames1[3], "*", ageInput2,  ") - (", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2, " + ", levelNames2[1], " + ", levelNames2[2], "*", ageInput, " + ", levelNames2[3], "*", ageInput2,  ")"
                )), parameterNames = rowNames )

              }else if(modelType() == "Cubic"){
                res <- deltaMethod(fit(), c(paste0(
                  "(", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2, " + ", rowNames[4], "*", ageInput3, " + ", levelNames1[1], " + ", levelNames1[2], "*", ageInput, " + ", levelNames1[3], "*", ageInput2, " + ", levelNames1[4], "*", ageInput3,  ") - (", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2, " + ", rowNames[4], "*", ageInput3, " + ", levelNames2[1], " + ", levelNames2[2], "*", ageInput, " + ", levelNames2[3], "*", ageInput2, " + ", levelNames2[4], "*", ageInput3,  ")"
                )), parameterNames = rowNames )

              }else if(modelType() == "Quartic"){
                res <- deltaMethod(fit(), c(paste0(
                  "(", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2, " + ", rowNames[4], "*", ageInput3, " + ", rowNames[5], "*", ageInput4, " + ", levelNames1[1], " + ", levelNames1[2], "*", ageInput, " + ", levelNames1[3], "*", ageInput2, " + ", levelNames1[4], "*", ageInput3, " + ", levelNames1[5], "*", ageInput4,  ") - (", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2, " + ", rowNames[4], "*", ageInput3, " + ", rowNames[5], "*", ageInput4, " + ", levelNames2[1], " + ", levelNames2[2], "*", ageInput, " + ", levelNames2[3], "*", ageInput2, " + ", levelNames2[4], "*", ageInput3, " + ", levelNames2[5], "*", ageInput4,  ")"
                )), parameterNames = rowNames )
              }

            }else{
              levelNames1 <- rowNames[str_detect(rowNames, paste0(levelNames, collapse = "|"))]

              if(modelType() == "Linear"){

                res <- deltaMethod(fit(), c(paste0(
                  "(", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", levelNames1[1], " + ", levelNames1[2], "*", ageInput, ") - (",rowNames[1], " + ", rowNames[2], "*", ageInput, ")"
                )), parameterNames = rowNames )

              }else if(modelType() == "Quadratic"){
                res <- deltaMethod(fit(), c(paste0(
                  "(", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2, " + ", levelNames1[1], " + ", levelNames1[2], "*", ageInput, " + ", levelNames1[3], "*", ageInput2,  ") - (", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2,  ")"
                )), parameterNames = rowNames )

              }else if(modelType() == "Cubic"){
                res <- deltaMethod(fit(), c(paste0(
                  "(", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2, " + ", rowNames[4], "*", ageInput3, " + ", levelNames1[1], " + ", levelNames1[2], "*", ageInput, " + ", levelNames1[3], "*", ageInput2, " + ", levelNames1[4], "*", ageInput3,  ") - (", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2, " + ", rowNames[4], "*", ageInput3,  ")"
                )), parameterNames = rowNames )

              }else if(modelType() == "Quartic"){
                res <- deltaMethod(fit(), c(paste0(
                  "(", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2, " + ", rowNames[4], "*", ageInput3, " + ", rowNames[5], "*", ageInput4, " + ", levelNames1[1], " + ", levelNames1[2], "*", ageInput, " + ", levelNames1[3], "*", ageInput2, " + ", levelNames1[4], "*", ageInput3, " + ", levelNames1[5], "*", ageInput4,  ") - (", rowNames[1], " + ", rowNames[2], "*", ageInput, " + ", rowNames[3], "*", ageInput2, " + ", rowNames[4], "*", ageInput3, " + ", rowNames[5], "*", ageInput4,   ")"
                )), parameterNames = rowNames )
              }

            }

            dif <- paste0( round(res$Estimate, 2), " (", round(res$`2.5 %`,2), " - ", round(res$`97.5 %`,2), ")")
            res <- data.frame(age = dif)
            return(res)
          })
          return(statements)
        }else{
          return(NULL)
        }
      })

      difTab <- reactive({
        if(input$varType == "cat" & length(input$levelsScores == 2)){
        difTab <- do.call(cbind, differenceScores())
        colnames(difTab) <- input$ageInputScore
        rownames(difTab) <- paste0("Difference between ",input$levelsScores[1]," and ", input$levelsScores[2]," (95% CI)")
        difTab
        }else{
          data.frame(NA)
        }
      })

      output$scoresDif <- renderTable({
        if(input$varType == "cat" & length(input$levelsScores == 2)){
        difTab()
        }
      }, rownames = TRUE)


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
      # ------------------------------------------
      # New method:
      AUC_delta <- reactive({

        coef <- summary(fit())$coefficients

        ageOrig <- modelDataEdit() %>%
          pull(age_original)
        ageOrig <- ageOrig[!is.na(ageOrig)]
        age1 <- input$AUCages[1] - mean(ageOrig)
        age2 <- input$AUCages[2] - mean(ageOrig)

        rowNames <- rownames(coef) %>%
          str_remove_all("I|\\(|\\^|\\)|\\:")

        AUC <-
          if(modelType() == "Linear"){
            deltaMethod(fit(), c( paste0("(((", age2, ")*(", rowNames[1], ")) + ((", rowNames[2], ")*(", age2, ")^2/2)) - (((", age1,")*(", rowNames[1], ")) + ((", rowNames[2], ")*(", age1, ")^2/2))") ) , parameterNames = rowNames)
          } else if(modelType() == "Quadratic"){
            deltaMethod(fit(), c( paste0("(((", age2, ")*(", rowNames[1], ")) + ((", rowNames[2], ")*(", age2, ")^2/2) + ((", rowNames[3], ")*(", age2, ")^3/3)) - (((", age1,")*(", rowNames[1], ")) + ((", rowNames[2], ")*(", age1, ")^2/2) + ((", rowNames[3], ")*(", age1, ")^3/3))") ), parameterNames = rowNames )
          } else if(modelType() == "Cubic"){
            deltaMethod(fit(), c( paste0("(((", age2, ")*(", rowNames[1], ")) + ((", rowNames[2], ")*(", age2, ")^2/2) + ((", rowNames[3], ")*(", age2, ")^3/3) + ((", rowNames[4], ")*(", age2, ")^4/4)) - (((", age1,")*(", rowNames[1], ")) + ((", rowNames[2], ")*(", age1, ")^2/2) + ((", rowNames[3], ")*(", age1, ")^3/3) + ((", rowNames[4], ")*(", age1, ")^4/4))") ), parameterNames = rowNames )
          } else if(modelType() == "Quartic"){
            deltaMethod(fit(), c( paste0("(((", age2, ")*(", rowNames[1], ")) + ((", rowNames[2], ")*(", age2, ")^2/2) + ((", rowNames[3], ")*(", age2, ")^3/3) + ((", rowNames[4], ")*(", age2, ")^4/4) + ((", rowNames[5], ")*(", age2, ")^5/5)) - (((", age1,")*(", rowNames[1], ")) + ((", rowNames[2], ")*(", age1, ")^2/2) + ((", rowNames[3], ")*(", age1, ")^3/3) + ((", rowNames[4], ")*(", age1, ")^4/4) + ((", rowNames[5], ")*(", age1, ")^5/5))") ), parameterNames = rowNames )
          }
        AUC <- paste0( round(AUC$Estimate, 2), " (", round(AUC$`2.5 %`,2), " - ", round(AUC$`97.5 %`,2), ")")


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

        AUCCovs <- lapply(1:(n-1), function(i){
          if(modelType() == "Linear"){

            AUC <- deltaMethod(fit(),
                               c( paste0(
                                 "(((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2) + ((",rowNames[rowIndex[i]],")*(",age2,")) + ((",rowNames[rowIndexInteract1[i]],")*(",age2,")^2/2) ) -
                                   (((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[rowIndex[i]],")*(",age1,")) + ((",rowNames[rowIndexInteract1[i]],")*(",age1,")^2/2) )"
                               ) )
                               , parameterNames = rowNames )

          } else if(modelType() == "Quadratic"){

            AUC <- deltaMethod(fit(),
                               c( paste0(
                                 "(((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2) + ((",rowNames[3],")*(",age2,")^3/3) + ((",rowNames[rowIndex[i]],")*(",age2,")) + ((",rowNames[rowIndexInteract1[i]],")*(",age2,")^2/2) + ((",rowNames[rowIndexInteract2[i]],")*(",age2,")^3/3) ) -
              (((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3) + ((",rowNames[rowIndex[i]],")*(",age1,")) + ((",rowNames[rowIndexInteract1[i]],")*(",age1,")^2/2) + ((",rowNames[rowIndexInteract2[i]],")*(",age1,")^3/3) )"
                               ) )
                               , parameterNames = rowNames )

          } else if(modelType() == "Cubic"){

            AUC <- deltaMethod(fit(),
                               c( paste0(
                                 "(((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2) + ((",rowNames[3],")*(",age2,")^3/3) + ((",rowNames[4],")*(",age2,")^4/4) + ((",rowNames[rowIndex[i]],")*(",age2,")) + ((",rowNames[rowIndexInteract1[i]],")*(",age2,")^2/2) + ((",rowNames[rowIndexInteract2[i]], ")*(",age2,")^3/3) + ((",rowNames[rowIndexInteract3[i]], ")*(", age2, ")^4/4) ) -
                                 (((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3) + ((",rowNames[4],")*(",age1,")^4/4) + ((",rowNames[rowIndex[i]],")*(",age1,")) + ((",rowNames[rowIndexInteract1[i]],")*(",age1,")^2/2) + ((",rowNames[rowIndexInteract2[i]], ")*(",age1,")^3/3) + ((",rowNames[rowIndexInteract3[i]], ")*(", age1, ")^4/4) )"
                               ) )
                               , parameterNames = rowNames )

          } else if(modelType() == "Quartic"){

            AUC <- deltaMethod(fit(),
                               c( paste0(
                                 "(((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2) + ((",rowNames[3],")*(",age2,")^3/3) + ((",rowNames[4],")*(",age2,")^4/4) + ((",rowNames[5],")*(",age2,")^5/5) + ((",rowNames[rowIndex[i]],")*(",age2,")) + ((",rowNames[rowIndexInteract1[i]],")*(",age2,")^2/2) + ((",rowNames[rowIndexInteract2[i]], ")*(",age2,")^3/3) + ((",rowNames[rowIndexInteract3[i]], ")*(", age2, ")^4/4)  + ((",rowNames[rowIndexInteract4[i]], ")*(", age2, ")^5/5)) -
                                 (((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3) + ((",rowNames[4],")*(",age1,")^4/4) + ((",rowNames[5],")*(",age1,")^5/5) + ((",rowNames[rowIndex[i]],")*(",age1,")) + ((",rowNames[rowIndexInteract1[i]],")*(",age1,")^2/2) + ((",rowNames[rowIndexInteract2[i]], ")*(",age1,")^3/3) + ((",rowNames[rowIndexInteract3[i]], ")*(", age1, ")^4/4) + ((",rowNames[rowIndexInteract4[i]], ")*(", age1, ")^5/5) )"
                               ) )
                               , parameterNames = rowNames )

          }
          AUC <- paste0( round(AUC$Estimate, 2), " (", round(AUC$`2.5 %`,2), " - ", round(AUC$`97.5 %`,2), ")")
        })
        return( list(AUC = AUC, AUCCovs = AUCCovs) )

      }else if(input$varType == "cont"){

        # continuous variable, calculate AUC ±1 sd
        AUC_SD <-
          if(modelType() == "Linear"){

            plus <- deltaMethod(fit(),
                               c( paste0(
                                 "(((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2) + ((",rowNames[rowIndex],")*(",age2,")) + ((",rowNames[rowIndexInteract1],")*(",age2,")^2/2) ) -
                                   (((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[rowIndex],")*(",age1,")) + ((",rowNames[rowIndexInteract1],")*(",age1,")^2/2) )"
                               ) )
                               , parameterNames = rowNames )

            minus <- deltaMethod(fit(),
                                c( paste0(
                                  "(((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2) - ((",rowNames[rowIndex],")*(",age2,")) - ((",rowNames[rowIndexInteract1],")*(",age2,")^2/2) ) -
                                   (((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) - ((",rowNames[rowIndex],")*(",age1,")) - ((",rowNames[rowIndexInteract1],")*(",age1,")^2/2) )"
                                ) )
                                , parameterNames = rowNames )

            c(paste0( round(plus$Estimate, 2), " (", round(plus$`2.5 %`,2), " - ", round(plus$`97.5 %`,2), ")"),
              paste0( round(minus$Estimate, 2), " (", round(minus$`2.5 %`,2), " - ", round(minus$`97.5 %`,2), ")"))

          } else if(modelType() == "Quadratic"){

            plus <- deltaMethod(fit(),
                                c( paste0(
                                  "(((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2) + ((",rowNames[3],")*(",age2,")^3/3) + ((",rowNames[rowIndex],")*(",age2,")) + ((",rowNames[rowIndexInteract1],")*(",age2,")^2/2) + ((",rowNames[rowIndexInteract2],")*(",age2,")^3/3)) -
                                   (((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3) + ((",rowNames[rowIndex],")*(",age1,")) + ((",rowNames[rowIndexInteract1],")*(",age1,")^2/2) + ((",rowNames[rowIndexInteract2],")*(",age1,")^3/3) )"
                                ) )
                                , parameterNames = rowNames )

            minus <- deltaMethod(fit(),
                                 c( paste0(
                                   "(((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2) + ((",rowNames[3],")*(",age2,")^3/3) - ((",rowNames[rowIndex],")*(",age2,")) - ((",rowNames[rowIndexInteract1],")*(",age2,")^2/2) - ((",rowNames[rowIndexInteract2],")*(",age2,")^3/3)) -
                                   (((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3) - ((",rowNames[rowIndex],")*(",age1,")) - ((",rowNames[rowIndexInteract1],")*(",age1,")^2/2) - ((",rowNames[rowIndexInteract2],")*(",age1,")^3/3) )"
                                 ) )
                                 , parameterNames = rowNames )

            c(paste0( round(plus$Estimate, 2), " (", round(plus$`2.5 %`,2), " - ", round(plus$`97.5 %`,2), ")"),
              paste0( round(minus$Estimate, 2), " (", round(minus$`2.5 %`,2), " - ", round(minus$`97.5 %`,2), ")"))

          } else if(modelType() == "Cubic"){

            plus <- deltaMethod(fit(),
                                c( paste0(
                                  "(((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2) + ((",rowNames[3],")*(",age2,")^3/3) + ((",rowNames[4],")*(",age2,")^4/4) + ((",rowNames[rowIndex],")*(",age2,")) + ((",rowNames[rowIndexInteract1],")*(",age2,")^2/2) + ((",rowNames[rowIndexInteract2],")*(",age2,")^3/3) + ((",rowNames[rowIndexInteract3],")*(",age2,")^4/4) ) -
                                   (((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3) + ((",rowNames[4],")*(",age1,")^4/4) + ((",rowNames[rowIndex],")*(",age1,")) + ((",rowNames[rowIndexInteract1],")*(",age1,")^2/2) + ((",rowNames[rowIndexInteract2],")*(",age1,")^3/3) + ((",rowNames[rowIndexInteract3],")*(",age1,")^4/4) )"
                                ) )
                                , parameterNames = rowNames )

            minus <- deltaMethod(fit(),
                                 c( paste0(
                                   "(((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2) + ((",rowNames[3],")*(",age2,")^3/3) + ((",rowNames[4],")*(",age2,")^4/4) - ((",rowNames[rowIndex],")*(",age2,")) - ((",rowNames[rowIndexInteract1],")*(",age2,")^2/2) - ((",rowNames[rowIndexInteract2],")*(",age2,")^3/3) - ((",rowNames[rowIndexInteract3],")*(",age2,")^4/4) ) -
                                   (((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3) + ((",rowNames[4],")*(",age1,")^4/4) - ((",rowNames[rowIndex],")*(",age1,")) - ((",rowNames[rowIndexInteract1],")*(",age1,")^2/2) - ((",rowNames[rowIndexInteract2],")*(",age1,")^3/3) - ((",rowNames[rowIndexInteract3],")*(",age1,")^4/4) )"
                                 ) )
                                 , parameterNames = rowNames )

            c(paste0( round(plus$Estimate, 2), " (", round(plus$`2.5 %`,2), " - ", round(plus$`97.5 %`,2), ")"),
              paste0( round(minus$Estimate, 2), " (", round(minus$`2.5 %`,2), " - ", round(minus$`97.5 %`,2), ")"))

          } else if(modelType() == "Quartic"){

            plus <- deltaMethod(fit(),
                                c( paste0(
                                  "(((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2) + ((",rowNames[3],")*(",age2,")^3/3) + ((",rowNames[4],")*(",age2,")^4/4) + ((",rowNames[5],")*(",age2,")^5/5) + ((",rowNames[rowIndex],")*(",age2,")) + ((",rowNames[rowIndexInteract1],")*(",age2,")^2/2) + ((",rowNames[rowIndexInteract2],")*(",age2,")^3/3) + ((",rowNames[rowIndexInteract3],")*(",age2,")^4/4) + ((",rowNames[rowIndexInteract4],")*(",age2,")^5/5) ) -
                                   (((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3) + ((",rowNames[4],")*(",age1,")^4/4) + ((",rowNames[5],")*(",age1,")^5/5) + ((",rowNames[rowIndex],")*(",age1,")) + ((",rowNames[rowIndexInteract1],")*(",age1,")^2/2) + ((",rowNames[rowIndexInteract2],")*(",age1,")^3/3) + ((",rowNames[rowIndexInteract3],")*(",age1,")^4/4) + ((",rowNames[rowIndexInteract4],")*(",age1,")^5/5) )"
                                ) )
                                , parameterNames = rowNames )

            minus <- deltaMethod(fit(),
                                 c( paste0(
                                   "(((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2) + ((",rowNames[3],")*(",age2,")^3/3) + ((",rowNames[4],")*(",age2,")^4/4) + ((",rowNames[5],")*(",age2,")^5/5) - ((",rowNames[rowIndex],")*(",age2,")) - ((",rowNames[rowIndexInteract1],")*(",age2,")^2/2) - ((",rowNames[rowIndexInteract2],")*(",age2,")^3/3) - ((",rowNames[rowIndexInteract3],")*(",age2,")^4/4) - ((",rowNames[rowIndexInteract4],")*(",age2,")^5/5) ) -
                                   (((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3) + ((",rowNames[4],")*(",age1,")^4/4) + ((",rowNames[5],")*(",age1,")^5/5) - ((",rowNames[rowIndex],")*(",age1,")) - ((",rowNames[rowIndexInteract1],")*(",age1,")^2/2) - ((",rowNames[rowIndexInteract2],")*(",age1,")^3/3) - ((",rowNames[rowIndexInteract3],")*(",age1,")^4/4) - ((",rowNames[rowIndexInteract4],")*(",age1,")^5/5) )"
                                 ) )
                                 , parameterNames = rowNames )

            c(paste0( round(plus$Estimate, 2), " (", round(plus$`2.5 %`,2), " - ", round(plus$`97.5 %`,2), ")"),
              paste0( round(minus$Estimate, 2), " (", round(minus$`2.5 %`,2), " - ", round(minus$`97.5 %`,2), ")"))

          }
        return(list(AUCCont = AUC, AUC_SD = AUC_SD))
      }

      })

      # ------------------------------------------
      # Plot AUC
      plotAUC <- eventReactive(c(input$button, input$AUCages), {

        if(input$varType == "cat"){
          req(AUC_delta()$AUCCovs)

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

          req(AUC_delta()$AUCCont)
          ggplot() +
            geom_ribbon(data = modelDataEdit(),
                        aes(x = age_original, ymax = pred, ymin = 0),
                        alpha = 0.1, show.legend = FALSE, fill = "#1D86C7") +
            geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred ) , na.rm=T)+
            geom_line(data = modelDataEdit(), aes(x= age_original ,  y = plus, color = "+ 1 SD" ) , na.rm=T)+
            geom_line(data = modelDataEdit(), aes(x= age_original ,  y = minus, color = "- 1 SD" ) , na.rm=T)+
            coord_cartesian(xlim = c(input$AUCages[1], input$AUCages[2])) +
            theme(legend.text = element_text(color = "black")) +
            ylab(paste0("Score (", traj(), ")")) +
            xlab("Age") +
            scale_x_continuous(breaks = seq(round(min(modelDataEdit()$age_original, na.rm =T)), round(max(modelDataEdit()$age_original, na.rm =T)), by = 1),
                               expand = c(0, 0)) +
            scale_color_manual(
              breaks = c("+ 1 SD", "Population Average", "- 1 SD"),
              values = c("#d55e00", "black", "#0072b2")) +
            scale_y_continuous(expand = c(0, 0))
        }
      })

      output$AUCplot <- renderPlot({
        plotAUC()
      })


      tableAUC <- eventReactive(c(input$button, input$AUCages), {
        if(input$varType == "cat"){
          req(AUC_delta()$AUCCovs)
          df <- t(
            cbind(
              data.frame(paste0(input$AUCages[1], " - ", input$AUCages[2]),
                         AUC_delta()$AUC),
              do.call(cbind, AUC_delta()$AUCCovs)
            )
          )

          rowname <- paste0("AUC (", traj(), ")")
          levelNames <- as.character(levels(as.factor(pull(modelDataEdit(), input$condition))))
          rownames(df) <- c("Age Range", paste0(rowname, " [", input$condition, ", level = ", levelNames, " ] (95% CIs)") )
          df

        }else if(input$varType == "cont"){
          req(AUC_delta()$AUCCont)
          df <- t(
            data.frame(paste0(input$AUCages[1], " - ", input$AUCages[2]),
                       AUC_delta()$AUCCont,
                       AUC_delta()$AUC_SD[1],
                       AUC_delta()$AUC_SD[2])
          )
          rowname <- paste0(paste0("AUC (", traj(), ") [", input$condition, " ] (95% CIs) "), c("Population Average", "+ 1 SD", "- 1 SD")  )
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
        if(input$varType == "cat" & length(input$levelsAUC == 2)){
        levelNames <- paste0(input$condition,input$levelsAUC) %>%
          str_remove_all("I|\\(|\\^|\\)|\\:")

        coef <- summary(fit())$coefficients

        ageOrig <- modelDataEdit() %>%
          pull(age_original)
        ageOrig <- ageOrig[!is.na(ageOrig)]
        age1 <- input$AUCages[1] - mean(ageOrig)
        age2 <- input$AUCages[2] - mean(ageOrig)

        rowNames <- rownames(coef) %>%
          str_remove_all("I|\\(|\\^|\\)|\\:")

        if( sum(str_detect(rowNames, levelNames[1])) == sum(str_detect(rowNames, levelNames[2])) ){

          levelNames1 <- rowNames[str_detect(rowNames, levelNames[1])]
          levelNames2 <- rowNames[str_detect(rowNames, levelNames[2])]

          if(modelType() == "Linear"){

          res <- deltaMethod(fit(), c( paste0(" (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)   + ( (",levelNames1[1],")*(",age2,") ) + ((",levelNames1[2],")*(",age2,")^2/2) ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2)   + ( (",levelNames1[1],")*(",age1,") ) + ((",levelNames1[2],")*(",age1,")^2/2) )) - (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)   + ( (",levelNames2[1],")*(",age2,") ) + ((",levelNames2[2],")*(",age2,")^2/2) ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2)   + ( (",levelNames2[1],")*(",age1,") ) + ((",levelNames2[2],")*(",age1,")^2/2) )) ") ) ,parameterNames = rowNames)

          }else if(modelType() == "Quadratic"){

            res <- deltaMethod(fit(), c( paste0(" (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)  + ((",rowNames[3],")*(",age2,")^3/3)   + ( (",levelNames1[1],")*(",age2,") ) + ((",levelNames1[2],")*(",age2,")^2/2) + ((",levelNames1[3],")*(",age2,")^3/3) ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3)  + ( (",levelNames1[1],")*(",age1,") ) + ((",levelNames1[2],")*(",age1,")^2/2) + ((",levelNames1[3],")*(",age1,")^3/3) )) - (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)   + ((",rowNames[3],")*(",age2,")^3/3)  + ( (",levelNames2[1],")*(",age2,") ) + ((",levelNames2[2],")*(",age2,")^2/2) + ((",levelNames2[3],")*(",age2,")^3/3) ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3)   + ( (",levelNames2[1],")*(",age1,") ) + ((",levelNames2[2],")*(",age1,")^2/2) + ((",levelNames2[3],")*(",age1,")^3/3) )) ") ) ,parameterNames = rowNames)

          }else if(modelType() == "Cubic"){

            res <- deltaMethod(fit(), c( paste0(" (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)  + ((",rowNames[3],")*(",age2,")^3/3) + ((",rowNames[4],")*(",age2,")^4/4)   + ( (",levelNames1[1],")*(",age2,") ) + ((",levelNames1[2],")*(",age2,")^2/2) + ((",levelNames1[3],")*(",age2,")^3/3) + ((",levelNames1[4],")*(",age2,")^4/4) ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3)  + ((",rowNames[4],")*(",age1,")^4/4)  + ( (",levelNames1[1],")*(",age1,") ) + ((",levelNames1[2],")*(",age1,")^2/2) + ((",levelNames1[3],")*(",age1,")^3/3) + ((",levelNames1[4],")*(",age1,")^4/4) )) - (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)   + ((",rowNames[3],")*(",age2,")^3/3)  + ((",rowNames[4],")*(",age2,")^4/4)  + ( (",levelNames2[1],")*(",age2,") ) + ((",levelNames2[2],")*(",age2,")^2/2) + ((",levelNames2[3],")*(",age2,")^3/3)  + ((",levelNames2[4],")*(",age2,")^4/4) ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3)  + ((",rowNames[4],")*(",age1,")^4/4)   + ( (",levelNames2[1],")*(",age1,") ) + ((",levelNames2[2],")*(",age1,")^2/2) + ((",levelNames2[3],")*(",age1,")^3/3) + ((",levelNames2[4],")*(",age1,")^4/4) )) ") ) ,parameterNames = rowNames)

          }else if(modelType() == "Quartic"){

            res <- deltaMethod(fit(), c( paste0(" (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)  + ((",rowNames[3],")*(",age2,")^3/3) + ((",rowNames[4],")*(",age2,")^4/4) + ((",rowNames[5],")*(",age2,")^5/5)   + ( (",levelNames1[1],")*(",age2,") ) + ((",levelNames1[2],")*(",age2,")^2/2) + ((",levelNames1[3],")*(",age2,")^3/3) + ((",levelNames1[4],")*(",age2,")^4/4)  + ((",levelNames1[5],")*(",age2,")^5/5) ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3)  + ((",rowNames[4],")*(",age1,")^4/4) + ((",rowNames[5],")*(",age1,")^5/5)  + ( (",levelNames1[1],")*(",age1,") ) + ((",levelNames1[2],")*(",age1,")^2/2) + ((",levelNames1[3],")*(",age1,")^3/3) + ((",levelNames1[4],")*(",age1,")^4/4) + ((",levelNames1[5],")*(",age1,")^5/5) )) -  (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)   + ((",rowNames[3],")*(",age2,")^3/3)  + ((",rowNames[4],")*(",age2,")^4/4) + ((",rowNames[5],")*(",age2,")^5/5)  + ( (",levelNames2[1],")*(",age2,") ) + ((",levelNames2[2],")*(",age2,")^2/2) + ((",levelNames2[3],")*(",age2,")^3/3)  + ((",levelNames2[4],")*(",age2,")^4/4) + ((",levelNames2[5],")*(",age2,")^5/5) ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3)  + ((",rowNames[4],")*(",age1,")^4/4)  + ((",rowNames[5],")*(",age1,")^5/5)   + ( (",levelNames2[1],")*(",age1,") ) + ((",levelNames2[2],")*(",age1,")^2/2) + ((",levelNames2[3],")*(",age1,")^3/3) + ((",levelNames2[4],")*(",age1,")^4/4)  + ((",levelNames2[5],")*(",age1,")^5/5) )) ") ) ,parameterNames = rowNames)

          }

        }else{
          levelNames1 <- rowNames[str_detect(rowNames, paste0(levelNames, collapse = "|"))]

          if(modelType() == "Linear"){
          res <- deltaMethod(fit(), c( paste0(" (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)   + ( (",levelNames1[1],")*(",age2,") ) + ((",levelNames1[2],")*(",age2,")^2/2) ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2)   + ( (",levelNames1[1],")*(",age1,") ) + ((",levelNames1[2],")*(",age1,")^2/2) )) - (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2) ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2)     )) ") ) ,parameterNames = rowNames)
          }else if(modelType() == "Quadratic"){

          res <- deltaMethod(fit(), c( paste0(" (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)  + ((",rowNames[3],")*(",age2,")^3/3)   + ( (",levelNames1[1],")*(",age2,") ) + ((",levelNames1[2],")*(",age2,")^2/2) + ((",levelNames1[3],")*(",age2,")^3/3) ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3)  + ( (",levelNames1[1],")*(",age1,") ) + ((",levelNames1[2],")*(",age1,")^2/2) + ((",levelNames1[3],")*(",age1,")^3/3) )) - (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)   + ((",rowNames[3],")*(",age2,")^3/3)   ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3)    )) ") ) ,parameterNames = rowNames)

        }else if(modelType() == "Cubic"){

          res <- deltaMethod(fit(), c( paste0(" (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)  + ((",rowNames[3],")*(",age2,")^3/3) + ((",rowNames[4],")*(",age2,")^4/4)   + ( (",levelNames1[1],")*(",age2,") ) + ((",levelNames1[2],")*(",age2,")^2/2) + ((",levelNames1[3],")*(",age2,")^3/3) + ((",levelNames1[4],")*(",age2,")^4/4) ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3)  + ((",rowNames[4],")*(",age1,")^4/4)  + ( (",levelNames1[1],")*(",age1,") ) + ((",levelNames1[2],")*(",age1,")^2/2) + ((",levelNames1[3],")*(",age1,")^3/3) + ((",levelNames1[4],")*(",age1,")^4/4) )) - (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)   + ((",rowNames[3],")*(",age2,")^3/3)  + ((",rowNames[4],")*(",age2,")^4/4)  ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3)  + ((",rowNames[4],")*(",age1,")^4/4)    )) ") ) ,parameterNames = rowNames)

        }else if(modelType() == "Quartic"){

          res <- deltaMethod(fit(), c( paste0(" (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)  + ((",rowNames[3],")*(",age2,")^3/3) + ((",rowNames[4],")*(",age2,")^4/4) + ((",rowNames[5],")*(",age2,")^5/5)   + ( (",levelNames1[1],")*(",age2,") ) + ((",levelNames1[2],")*(",age2,")^2/2) + ((",levelNames1[3],")*(",age2,")^3/3) + ((",levelNames1[4],")*(",age2,")^4/4)  + ((",levelNames1[5],")*(",age2,")^5/5) ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3)  + ((",rowNames[4],")*(",age1,")^4/4) + ((",rowNames[5],")*(",age1,")^5/5)  + ( (",levelNames1[1],")*(",age1,") ) + ((",levelNames1[2],")*(",age1,")^2/2) + ((",levelNames1[3],")*(",age1,")^3/3) + ((",levelNames1[4],")*(",age1,")^4/4) + ((",levelNames1[5],")*(",age1,")^5/5) )) -  (( ((",age2,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age2,")^2/2)   + ((",rowNames[3],")*(",age2,")^3/3)  + ((",rowNames[4],")*(",age2,")^4/4) + ((",rowNames[5],")*(",age2,")^5/5)   ) - ( ((",age1,")*(",rowNames[1],")) + ((",rowNames[2],")*(",age1,")^2/2) + ((",rowNames[3],")*(",age1,")^3/3)  + ((",rowNames[4],")*(",age1,")^4/4)  + ((",rowNames[5],")*(",age1,")^5/5)    )) ") ) ,parameterNames = rowNames)

        }
        }
        dif <- paste0( round(res$Estimate, 2), " 95% CI: (", round(res$`2.5 %`,2), " - ", round(res$`97.5 %`,2), ")")
        statement <- paste0("The difference between the two factor levels (for the age ranges ", input$AUCages[1], " - ", input$AUCages[2] ,") is ", dif ,".")

        return(statement)
        }else{
          return(paste0(NA))
        }

      })

      output$AUCdifText <- renderText({
        if(input$varType == "cat" & length(input$levelsAUC == 2)){
        difference()
        }else{
        " "
        }
      })

      #################################################################################
      # ----- DOWNLOADING RESULTS TAB -------
      ###############################################################
      # Add UI to download results
      output$buttonHere <- renderUI({
        req(plot())
        tagList(
          downloadButton(ns("downloadReport"), "Download report")
        )
      })

      output$downloadReport <- downloadHandler(
        filename = function(){
          paste0("Interaction_Variable_", Sys.Date(), ".pdf")
        },
        content = function(file) {
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path("www/interactionVar.Rmd")
          file.copy("interactionVar.Rmd", tempReport, overwrite = TRUE)



          # -------Set up parameters to pass to Rmd document--------
          # pass condition selection to an object for report?
          condition <- reactive({ input$condition })
          # pass vartype to an object for report?
          vartype <- reactive({ input$varType })

          if( length(input$ageInputScore) == 0 & length(input$AUCages) == 0 ){
            params <- list(
              cond = condition(),
              condtype = vartype(),
              condPlot = plot(),
              condModelForm = modelform(),
              condFixed = modelStatsFixed(),
              condRandom = modelStatsRandom(),
              modelDataEdit = modelDataEdit(),
              plotScore = NA,
              tableScore = NA,
              difTab = NA,
              AUCplot = NA,
              AUCtable = NA,
              difference = NA,
              traj = traj(),
              modelType = modelType()
            )
          }else if( length(input$ageInputScore) == 0 & length(input$AUCages) > 0 ){
            params <- list(
              cond = condition(),
              condtype = vartype(),
              condPlot = plot(),
              condModelForm = modelform(),
              condFixed = modelStatsFixed(),
              condRandom = modelStatsRandom(),
              modelDataEdit = modelDataEdit(),
              plotScore = NA,
              tableScore = NA,
              difTab = NA,
              AUCplot = plotAUC(),
              AUCtable = tableAUC(),
              difference = difference(),
              traj = traj(),
              modelType = modelType()
            )
          }else if( length(input$ageInputScore) > 0 & length(input$AUCages) == 0 ){
            params <- list(
              cond = condition(),
              condtype = vartype(),
              condPlot = plot(),
              condModelForm = modelform(),
              condFixed = modelStatsFixed(),
              condRandom = modelStatsRandom(),
              modelDataEdit = modelDataEdit(),
              plotScore = plotScoreAll(),
              tableScore = tableScoreAll(),
              difTab = difTab(),
              AUCplot = NA,
              AUCtable = NA,
              difference = difference(),
              traj = traj(),
              modelType = modelType()
            )
          }else{
            params <- list(
              cond = condition(),
              condtype = vartype(),
              condPlot = plot(),
              condModelForm = modelform(),
              condFixed = modelStatsFixed(),
              condRandom = modelStatsRandom(),
              modelDataEdit = modelDataEdit(),
              plotScore = plotScoreAll(),
              tableScore = tableScoreAll(),
              difTab = difTab(),
              AUCplot = plotAUC(),
              AUCtable = tableAUC(),
              difference = difference(),
              traj = traj(),
              modelType = modelType()
            )
          }

          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(tempReport, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }
)

      ###
      return(
        list(
          condType = reactive({ input$varType }),
          cond = reactive({ input$condition }),
          condPlot = reactive({ output$modelCondPlot  }),
          condModelForm = reactive({ output$form }),
          condFixed = reactive({ output$modelStatsFixed }),
          condRandom = reactive({ output$modelStatsRandom }),
          modelDataEdit = reactive({ modelDataEdit })
        )
      )

    }
  )
}

