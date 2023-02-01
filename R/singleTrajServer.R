singleTrajServer <- function(id,
                             ID,
                             modelData,
                             modelFit) {
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

        # ------------------------------------------
        # add the "prediction"/model col to dataframe
        modelDataEdit <- reactive({
          modelData() %>%
            mutate(pred = predict(modelFit(), ., re.form = NA))
        })
       #
       # -----------------------------------------------
       # Select the IDs from input:
        IDs <- reactive({
          if(input$choice == "Random Sample"){
          IDs <- modelDataEdit() %>%
            sample_n(input$NRand) %>%
            pull(!!sym(ID()))
          }else if(input$choice == "Select specific individuals"){
            IDs <- strsplit(input$SelectIDs, ",")[[1]] %>% # split string by commas
                      str_remove_all(., "\n") %>%  # remove any new lines (if present)
                      str_trim()                   # remove any white space (if present)
          }else if(input$choice  == "A specific variable"){
            IDs <- modelDataEdit() %>%
              filter(!!sym(input$catVars) == input$catLevels) %>%
              sample_n(input$NRandVar) %>%
              pull(!!sym(ID()))
          }
          return(sort(IDs))
        })

       # UI output of list of IDs:
        output$textIDs <- renderText({
          paste0("The following IDs are plotted: ", paste0(IDs(), collapse = ", "))
        })

        output$table <- renderDataTable({
          modelDataEdit() %>%
            filter(!!sym(ID()) %in% IDs())
        })


       # # -----------------------------------------------
       # Estimate the individual trajectories:


       # -----------------------------------------------
       # Plot the individual trajectories:
       # output$trajPlot

    }
  )
}

