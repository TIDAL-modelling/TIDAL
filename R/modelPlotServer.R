#' Model plot - data exploration page
#'
#' @import broom.mixed
#' @import lme4
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @importFrom stats IQR confint deviance fitted median pnorm qnorm sd
#' @import utils
#' @importFrom multcomp glht
#' @importFrom car deltaMethod
#'
#' @keywords internal
#' @name modelPlotServer
utils::globalVariables(c("Phenotype", "SD", "pred", "conf.low", "conf.high", "Age", "lower", "upper"))
modelPlotServer <- function(id,
                            modelData,
                            modelFit,
                            age,
                            traj,
                            timePoint,
                            modelType,
                            button
                            ) {
  moduleServer(
    id,
    function(input, output, session) {

      # ------------------------------------------
      # add the "prediction"/model col to dataframe (adjustedScore)


      modelDataEdit <- eventReactive(button(), {

        age <- modelData() %>% pull(!!age())

        coef <- summary(modelFit())$coefficients

        if(modelType() == "Linear"){

          adjustedScore <- age * coef[2,1] + coef[1,1]

        } else if(modelType() == "Quadratic"){

          adjustedScore <- age * coef[2,1] + coef[1,1] +
            age^2 * coef[3,1]

        } else if(modelType() == "Cubic"){
          adjustedScore <- age * coef[2,1] + coef[1,1]  +
            age^2 * coef[3,1] +
            age^3 * coef[4,1]

        } else if(modelType() == "Quartic"){
          adjustedScore <- age * coef[2,1] + coef[1,1]  +
            age^2 * coef[3,1] +
            age^3 * coef[4,1] +
            age^4 * coef[5,1]

        }

        modelData() %>%
          mutate(pred = adjustedScore)
      })


      # ------------------------------------------
      # also add the 95% CIs for these estimates, by using glht

      score_glht <- reactive({
        ageOrig <- modelDataEdit() %>% pull(age_original)
        ageOrig <- ageOrig[!is.na(ageOrig)]

        ageCalcs <- c(min(ageOrig), seq(round(min(ageOrig), 1), round(max(ageOrig), 1), 0.5), max(ageOrig) )

        score <- lapply(ageCalcs, function(x){

          rowNames <- rownames(summary(modelFit())$coefficients)

          ageInput <- round(x - mean(ageOrig), 3)
          ageInput2 <- ageInput^2
          ageInput3 <- ageInput^3
          ageInput4 <- ageInput^4
          # --------------------
          # Change for model type
          if(modelType() == "Linear"){
            res <- deltaMethod(modelFit(), c( paste0(rowNames[1], " + ", rowNames[2], "*", ageInput)) )
          }else if(modelType() == "Quadratic"){


            res <- deltaMethod(modelFit(), c( paste0(rowNames[1], " + ",
                                                       rowNames[2], "*", ageInput," + \`",
                                                       rowNames[3], "\`*", ageInput2)) )
          } else if(modelType() == "Cubic"){


            res <- deltaMethod(modelFit(), c( paste0(rowNames[1], " + ",
                                                       rowNames[2], "*", ageInput," + \`",
                                                       rowNames[3], "\`*", ageInput2,  " + \`",
                                                       rowNames[4], "\`*", ageInput3) ) )

          } else if(modelType() == "Quartic"){


            res <- deltaMethod(modelFit(), c( paste0(rowNames[1], " + ",
                                                       rowNames[2], "*", ageInput," + \`",
                                                       rowNames[3], "\`*", ageInput2,  " + \`",
                                                       rowNames[4], "\`*", ageInput3,  " + \`",
                                                       rowNames[5], "\`*", ageInput4) ))
          }

          # --------------------
          rowname <- paste0("Score (", traj(), ")")

          res <- data.frame(estimate = res$Estimate,
                            conf.low = res$`2.5 %`,
                            conf.high = res$`97.5 %`) %>%
            mutate(across(where(is.numeric), round, 2))
          rownames(res) <- paste0(rowname, " (95% CIs)")
          res

          return( res )
        })
        return(score)
      })


      # ------------------------------------------
      # Get the mean and sd for depression scores at each time point
      df.plot <- reactive({
        modelData() %>%
          group_by(across( !!timePoint() )) %>%
          summarise(Age = mean(age_original, na.rm = T),
                    Phenotype = mean(!!sym(traj()), na.rm = T),
                    SD = sd(!!sym(traj()), na.rm = T),
                    n = sum(!is.na( !!sym(traj()) ))
          ) %>%
          mutate(upper = Phenotype + ( qnorm(0.975)*SD/sqrt(n) ),
                 lower = Phenotype - ( qnorm(0.975)*SD/sqrt(n) ))

      })

      # ------------------------------------------

      mainPlot <- eventReactive(c(input$plotCheckbox, modelFit()), {
        req(score_glht())

        ageOrig <- modelDataEdit() %>% pull(age_original)
        ageOrig <- ageOrig[!is.na(ageOrig)]
        ageCalcs <- c(min(ageOrig), seq(round(min(ageOrig), 1), round(max(ageOrig), 1), 0.5), max(ageOrig) )

        estimate <- do.call(rbind, score_glht()) %>%
                      mutate(age = ageCalcs)

        if(input$plotCheckbox == TRUE){
        ggplot() +
          geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred), color = "#1D86C7", linewidth = 1.5, na.rm=T) +
          geom_ribbon(data = estimate, aes(x= age , ymin = conf.low, ymax = conf.high), fill = "#1D86C7", alpha = 0.2, na.rm = T) +
          geom_point(data = df.plot(),aes(x=Age, y=Phenotype))+
          geom_line(data = df.plot(),aes(x=Age, y=Phenotype)) +
          geom_errorbar(data = df.plot(), aes(x=Age, y=Phenotype, ymin = lower, ymax = upper)) +
          ylab(paste0("Score (", traj(), ")")) +
          xlab("Age")

        }else if(input$plotCheckbox == FALSE){
          ggplot() +
            geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred), color = "#1D86C7", linewidth = 1.5, na.rm=T) +
            geom_ribbon(data = estimate, aes(x= age , ymin = conf.low, ymax = conf.high), fill = "#1D86C7", alpha = 0.2, na.rm = T) +
            ylab(paste0("Score (", traj(), ")")) +
            xlab("Age")
        }

      })

      # plot the mean trajectory against the model
      output$mainPlot <- renderPlot(
        if(!inherits(modelFit(), "try-error")){
        mainPlot()
        }
      )

      return(
        list(
        df.plot = df.plot,
        mainPlot = mainPlot,
        modelDataEdit = modelDataEdit)
        )
    }
  )
}

