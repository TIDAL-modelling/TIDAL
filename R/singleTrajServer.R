#' Server for individual trajectories
#'
#' @import broom.mixed
#' @import lme4
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @import stringr
#' @import utils
#' @importFrom stats IQR confint deviance fitted median pnorm qnorm sd
#'
#' @keywords internal
#' @name singleTrajServer
utils::globalVariables(c(".", "pred"))
singleTrajServer <- function(id,
                             subject,
                             age,
                             traj,
                             modelData,
                             modelFit,
                             modelType,
                             cov) {
  # create a module server
  moduleServer(
    # id is the id of the module
    id,
    function(input, output, session) {
      # create namespaced ID
      ns <- NS(id)

      # -----------------------------------------------
      # render UI depending on user input for "choice"
      # random sample of IDs:
      # user then can select a sample size using slider from 1 to nrow(df)
      # specific IDs:
      # IDs in text box (separated by a comma)
      # specific variable:
      # dropdown of likely categorical variables
      # second dropdown of levels for that variable
      observeEvent(input$choice, {
        if (input$choice == "Random Sample"){
          output$random <- renderUI({
            tagList(
              sliderInput(ns("NRand"), "Number of random individuals to sample",
                          value = 3, min = 1, max = 30)
            )
          })
          output$specificIDs <- renderUI({
          })
          output$specificVar <- renderUI({
          })
        } else if (input$choice == "Select specific individuals"){
          output$specificIDs <- renderUI({
            textAreaInput(ns("SelectIDs"), "Input IDs (if multiple IDs separate with a comma)", rows = 2)
          })
          output$random <- renderUI({
          })
          output$specificVar <- renderUI({
          })
        } else if(input$choice == "A specific variable"){

          output$specificVar <- renderUI({
            var <- colnames(modelData())[apply(modelData(), 2, function(x) length(unique(x))) < 40]

            tagList(
              selectInput(ns("catVars"), "Choose a variable of interest:", choices = var),
              selectInput(ns("catLevels"), "Choose a level of interest from that variable:", choices = c()),
              sliderInput(ns("NRandVar"), "Number of random individuals to sample",
                          value = 3, min = 1, max = 30)
            )
          })
          output$specificIDs <- renderUI({
          })
          output$random <- renderUI({
          })
        }
      })

      observeEvent(input$catVars,{

        # allow the user to select a variable from the column names of the dataset
        # critically this should be categorical variables (for the plot to split by a factor)
        # hacky way to choose a categorical variable is below
        # user can only select columns with unique values of length < 40. 40 being an arbitary number
        # but any more than this may not be very useful or visible on a plot, 40 is already a lot.
        updateSelectInput(
          session,
          "catLevels",
          choices =
            modelData() %>%
            drop_na() %>%
            distinct(!!sym(input$catVars)) %>%
            pull(1)
        )
      })

      # -----------------------------------------------
      # add the "prediction"/model col to dataframe
      modelDataEdit <- reactive({

        ageVec <- modelData() %>% pull(!!age())

        if(modelType() == "Linear"){
          adjustedScore <- ageVec * summary(modelFit())$coefficients[2,1] + summary(modelFit())$coefficients[1,1]
        } else if(modelType() == "Quadratic"){
          adjustedScore <- ageVec * summary(modelFit())$coefficients[2,1] + summary(modelFit())$coefficients[1,1] +
            ageVec^2 * summary(modelFit())$coefficients[3,1]
        } else if(modelType() == "Cubic"){
          adjustedScore <- ageVec * summary(modelFit())$coefficients[2,1] + summary(modelFit())$coefficients[1,1]  +
            ageVec^2 * summary(modelFit())$coefficients[3,1] +
            ageVec^3 * summary(modelFit())$coefficients[4,1]
        } else if(modelType() == "Quartic"){
          adjustedScore <- ageVec * summary(modelFit())$coefficients[2,1] + summary(modelFit())$coefficients[1,1]  +
            ageVec^2 * summary(modelFit())$coefficients[3,1] +
            ageVec^3 * summary(modelFit())$coefficients[4,1] +
            ageVec^4 * summary(modelFit())$coefficients[5,1]
        }

        # Estimate individual trajectories

        pred_individual <- fitted(modelFit())

        pred_individual_df <- cbind(modelFit()@frame, pred_individual) %>%
          select(c(!!sym(subject()), !!sym(age()), pred_individual))

        # Not all participants were included in the prediction,
        # I assume because they didn't have enough data?
        modelDataPred <- modelData() %>%
          mutate(pred = adjustedScore)

        modelDataEdit <- merge(modelDataPred, pred_individual_df, by = c(subject(), age()))

        return(modelDataEdit)
      })

      # -----------------------------------------------
      # Select the IDs from input (for individuals that we can get model estimates for):
      IDs <- reactive({
        if(input$choice == "Random Sample"){
          IDs <- modelDataEdit() %>%
            sample_n(input$NRand) %>%
            pull(!!sym(subject()))
        }else if(input$choice == "Select specific individuals"){
          IDs <- strsplit(as.character(input$SelectIDs), ",")[[1]] %>% # split string by commas
            str_remove_all(., "\n") %>%  # remove any new lines (if present)
            str_trim()                   # remove any white space (if present)
          IDs <- IDs[nchar(IDs) != 0] # Remove any empty values due to spurious commas
        }else if(input$choice  == "A specific variable"){
          IDs <- modelDataEdit() %>%
            filter(!!sym(input$catVars) == input$catLevels) %>%
            sample_n(input$NRandVar) %>%
            pull(!!sym(subject()))
        }
        return(sort(IDs))
      })

      # UI output of list of IDs:
      output$textIDs <- renderText({
        paste0("The following IDs are plotted: ", paste0(IDs(), collapse = ", "))
      })


      # -----------------------------------------------
      # Plot the individual trajectories:
      plot <- reactive({
        req(IDs())

        ggplot() +
          geom_line(data = modelDataEdit(), aes(x= age_original,  y = pred), na.rm=T) +
          geom_line(data = filter(modelDataEdit(), !!sym(subject()) %in% IDs())  ,
                    aes(x=age_original,  y = pred_individual, color = as.factor(!!sym(subject()))), na.rm=T, linetype="dashed")+
          ylab(paste0("Score (", traj(), ")")) +
          xlab("Age") +
          guides(color=guide_legend(title=" "))
      })

      output$trajPlot <-  renderPlot({
          plot()
      })

      # -----------------------------------------------
      # Plot the individual trajectories of the original data:
      plot_original <- reactive({
        req(IDs())

        ggplot() +
          geom_line(data = modelDataEdit(), aes(x= age_original,  y = pred), na.rm=T) +
          geom_line(data = filter(modelDataEdit(), !!sym(subject()) %in% IDs())  ,
                    aes(x=age_original,  y = !!sym(traj()), color = as.factor(!!sym(subject()))), na.rm=T, linetype="dashed")+
          ylab(paste0("Score (", traj(), ")")) +
          xlab("Age") +
          guides(color=guide_legend(title=" "))
      })

      output$trajPlot_original <-  renderPlot({
        plot_original()
      })

    }
  )
}

