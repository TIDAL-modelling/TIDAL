#' Shiny module - Selecting and reading in the dataset
#'
#' @noRd
#' @keywords internal
#' @export
selectDataServer <- function(id, dataFormatted) {
  moduleServer(
    id,
    # Below is the module function
    function(input, output, session) {
      ns <- NS(id)

      # if the user wants to upload a file, add a UI for this
      data <- reactive({
        req(input$select)
        if (input$select == "Upload a long format dataset"){
          output$uploadFile <- renderUI({
            tagList(
              fileInput(ns("uploadFile"), NULL),
              p("Please wait for file to upload.")
            )
          })
          req(input$uploadFile)
          data <- fread(input$uploadFile$datapath)

        }
        else {
          output$uploadFile <- renderUI({
          })
          data <- dataFormatted()
        }

        return(data)
      })

      # ------------------------------
      # Ensure column names go back to blank when input$select changes

      # Create a reactiveValues object to store the previous value of input$select
      prev_select <- reactiveValues(value = NULL)

      observeEvent(input$select, {
        # Check if input$select has changed from the previous value
        if (!is.null(prev_select$value) && prev_select$value != input$select) {
          # Check if input$select changed to or from the desired options
          if ((prev_select$value == "Upload a long format dataset" && input$select == "Data formatted on previous page") ||
              (prev_select$value == "Data formatted on previous page" && input$select == "Upload a long format dataset")) {
            # Code to run when input$select changes between the desired options
            updateSelectInput(session, ns("ID"), "Participant ID variable:", choices =  c())
            updateSelectInput(session, ns("traj"), "Variable to model trajectory on, eg. depression scores (continuous):", choices =  c())
            updateSelectInput(session, ns("age"), "Variable for age at time point (continous):", choices =  c())
            updateSelectInput(session, ns("timePoint"), "Variable for time point (categorical):", choices =  c())
          }
        }

        # Update the previous value of input$select
        prev_select$value <- input$select
      })



      # ------------------------------

      # Update UI of ID and timePoint with drop down choices of all the column names in data (output from selectDataServer.R)
      colVarUpdate <- function(colVar, i){
        observeEvent(data(), {
          updateSelectInput(
            session,
            colVar,
            choices = names(data()),
            selected = names(data() [i])
          )
        })
      }

      varNames <- list("ID", "timePoint")
      varIndex <- list(NULL,  NULL)
      purrr::map2(varNames, varIndex, \(varNames, varIndex) colVarUpdate(varNames, varIndex))

      # Update UI of ID, traj, age and timePoint with drop down choices of all the column names in data (output from selectDataServer.R)
      colVarUpdateNumeric <- function(colVar, i){
        observeEvent(data(), {
          updateSelectInput(
            session,
            colVar,
            choices = names(select(data(), where(is.numeric)) ),
            selected = names(select(data(), where(is.numeric)) [i])
          )
        })
      }

      varNames <- list("traj", "age")
      varIndex <- list(NULL, NULL)
      purrr::map2(varNames, varIndex, \(varNames, varIndex) colVarUpdateNumeric(varNames, varIndex))

      # add what type of model to run and input the different formula here:
      modelForm <- reactive({
        if(input$modelType == "Linear"){
          paste0(input$traj," ~ ", input$age, " + ", "(", input$age, "|" , input$ID, ")")
        } else if(input$modelType == "Quadratic"){
          paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2) + (1 + ", input$age, " + I(",input$age, "^2) |" , input$ID, ")" )
        } else if(input$modelType == "Cubic"){
          paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2)", " + I(", input$age   ,"^3)" ," + (1 + ", input$age, " + I(",input$age, "^2) |" , input$ID, ")")
        } else if(input$modelType == "Quartic"){
          paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2)", " + I(", input$age   ,"^3)" , " + I(", input$age   ,"^4)" ," + (1 + ", input$age, " + I(",input$age, "^2) |" , input$ID, ")")
        }
      })


      return(
        list(
          data = data,
          modelForm = modelForm,
          ID = reactive({ input$ID }),
          traj = reactive({ input$traj }),
          age = reactive({ input$age }),
          timePoint = reactive({ input$timePoint }),
          modelType = reactive({ input$modelType })
        )
      )
    }
  )
}
