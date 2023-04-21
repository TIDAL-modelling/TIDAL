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
        output$uploadFile <- renderUI({
          if (input$select == "Upload a long format dataset"){
          tagList(
            fileInput(ns("uploadFile"), NULL),
            p("Please wait for file to upload.")
          )
      }
      })

      data <- reactive ({
        if (input$select == "Upload a long format dataset"){
          req(input$uploadFile)
          data <- fread(input$uploadFile$datapath)
        }
        else {
          data <- dataFormatted()
        }
        return(data)
      })

      # ------------------------------
      output$additional <- renderUI({
          req(data())
        # Render UI elements
        tagList(
          selectInput(ns("ID"), "Participant ID variable:", choices = names(data())),
          selectInput(ns("traj"), "Variable to model trajectory on, eg. depression scores (continuous):", choices = names(data())),
          selectInput(ns("age"), "Variable for age at time point (continous):", choices = names(data())),
          selectInput(ns("timePoint"), "Variable for time point (categorical):", choices = names(data())),
          selectInput(ns("modelType"), "Model Type:", choices = c("Linear", "Quadratic", "Cubic", "Quartic"))
        )
      })
      # ------------------------------
      # add what type of model to run and input the different formula here:
      modelForm <- eventReactive(input$button, {
        req(data())
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
          button = reactive({ input$button }),
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
