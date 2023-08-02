#' Display score at an age for the data exploration page
#'
#' @import broom.mixed
#' @import lme4
#' @import dplyr
#' @import ggplot2
#' @import data.table
#' @import shinyjs
#' @import tidyr
#' @import magrittr
#' @import stringr
#'
#' @noRd
#' @keywords internal
#' @export
datExAltServer <- function(id,
                           modelDataEdit,
                           modelFit,
                           modelType,
                           traj

) {

  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)

      # ------------------------------------------
      # Allow the user to select the ages they want to calculate scores for
      output$selectAge <- renderUI({
        ageOrig <- modelDataEdit() %>%
                          pull(age_original)
        ageOrig <- ageOrig[!is.na(ageOrig)]
        checkboxGroupInput(ns("ageInput"),
                           "What ages do you want to calculate scores for?",
                           seq(ceiling(min(ageOrig, na.rm =T)),floor(max(ageOrig, na.rm =T))),
                           inline = TRUE)
      })

      # ------------------------------------------
      # Calculate the score at a given age (intercept + slope etc)
      # use glht to calculate scores at ages
      score_glht <- reactive({
        ageOrig <- modelDataEdit() %>% pull(age_original)
        ageOrig <- ageOrig[!is.na(ageOrig)]

        score <- lapply(as.numeric(input$ageInput), function(x){

          rowNames <- rownames(summary(modelFit())$coefficients)

          ageInput <- round(x - mean(ageOrig), 3)
          ageInput2 <- ageInput^2
          ageInput3 <- ageInput^3
          ageInput4 <- ageInput^4
          # --------------------
          # Change for model type
          if(modelType() == "Linear"){
            res <- multcomp::glht(modelFit(), linfct = c( paste0(rowNames[1], " + ", rowNames[2], "*", ageInput, " == 0")) )
          }else if(modelType() == "Quadratic"){


            res <- glht(modelFit(), linfct = c( paste0(rowNames[1], " + ",
                                                  rowNames[2], "*", ageInput," + \`",
                                                  rowNames[3], "\`*", ageInput2,
                                                  " == 0")) )
          } else if(modelType() == "Cubic"){


            res <- glht(modelFit(), linfct = c( paste0(rowNames[1], " + ",
                                                  rowNames[2], "*", ageInput," + \`",
                                                  rowNames[3], "\`*", ageInput2,  " + \`",
                                                  rowNames[4], "\`*", ageInput3,
                                                  " == 0") ) )

          } else if(modelType() == "Quartic"){


            res <- glht(modelFit(), linfct = c( paste0(rowNames[1], " + ",
                                                  rowNames[2], "*", ageInput," + \`",
                                                  rowNames[3], "\`*", ageInput2,  " + \`",
                                                  rowNames[4], "\`*", ageInput3,  " + \`",
                                                  rowNames[5], "\`*", ageInput4,
                                                  " == 0") ))
          }

          # --------------------
          res <- tidy(confint(res))

          rowname <- paste0("Score (", traj(), ")")

          res <-  res %>%
            mutate(contrast = paste0(rowname, " (95% CIs)")) %>%
            column_to_rownames(var = "contrast")

          return( res )
        })
        return(score)
      })

      # ------------------------------------------
      # Plot the score at the given age

      # points of intersection of age and score
      plotScoreAll <- eventReactive(input$ageInput, {

        req(score_glht())

        estimate <- lapply(score_glht(), function(df) {
          df %>%
            dplyr::select(estimate)
        })  %>% do.call(cbind, .)
        colnames(estimate) <- input$ageInput
        estimate <- estimate %>%
          gather(age, score, 1:ncol(estimate)) %>%
          mutate(age = as.numeric(age))

        conf.low <- lapply(score_glht(), function(df) {df %>% dplyr::select(conf.low)}) %>% do.call(cbind, .)
        colnames(conf.low) <- input$ageInput
        conf.low  <- conf.low %>%
          gather(age, conf.low, 1:ncol(conf.low)) %>%
          mutate(age = as.numeric(age))

        conf.high <- lapply(score_glht(), function(df) {df %>% dplyr::select(conf.high)}) %>% do.call(cbind, .)
        colnames(conf.high) <- input$ageInput
        conf.high  <- conf.high %>%
          gather(age, conf.high, 1:ncol(conf.high)) %>%
          mutate(age = as.numeric(age))%>%
          dplyr::select(-age)

        conf <- cbind(conf.low, conf.high, "age")

        ggplot() +
          geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred  ) , na.rm=T) +
          theme(legend.text = element_text(color = "black")) +
          geom_errorbar(data = conf, aes(x = age, ymin = conf.low, ymax = conf.high), width = 0.5) +
          geom_point(data = estimate, aes(x = age, y = score), col = "#1D86C7", size = 5) +
          ylab(paste0("Score (", traj(), ")")) +
          xlab("Age")

      })

      output$plot <- renderPlot({
        plotScoreAll()
      })

      # ------------------------------------------
      # Return a table of the score for all the ages
      # --- Age | Score -----
      # Change "Score" to the actual column name from the dataframe - which the user previously specified
      datExAltTable <- reactive({
        req(score_glht())

        estimateCI <- lapply(score_glht(), function(df) {
          df %>%
            mutate(estimateCI = paste0(round(estimate,3) , " (", round(conf.low,3) , " - ", round(conf.high,3) , ")")) %>%
            dplyr::select(estimateCI)
        })  %>% do.call(cbind, .)
        colnames(estimateCI) <- input$ageInput
        estimateCI
      })

      output$table <- renderTable({
        datExAltTable()
      }, colnames = TRUE, rownames = TRUE)
      # ------------------------------------------

      return(list(
            datExAltTable = datExAltTable,
            datExAltPlot = plotScoreAll
        ))

    }
  )
}

