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
            fileInput(ns("uploadFile"), NULL)
          )
      }
      })
      # if the user wants to use data formatted on previous page, from wide to long format then save this as "data()"
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

      # Allow the user to assign the variables below from the column names in their dataframe
      output$additional <- renderUI({
          req(data())
        # Render UI elements
        tagList(
          selectInput(ns("ID"), "Participant ID variable:", choices = names(data()), selected = names(select(data(), where(is.numeric)) )[1]),
          selectInput(ns("traj"), "Variable to model trajectory on, eg. depression scores (continuous):", choices = names(select(data(), where(is.numeric)) ) , selected = names(select(data(), where(is.numeric)) )[3]),
          selectInput(ns("age"), "Variable for age at time point (continous):", choices = names(select(data(), where(is.numeric)) ) , selected = names(select(data(), where(is.numeric)) )[2]),
          selectInput(ns("timePoint"), "Variable for time point (categorical):", choices = names(data()) , selected = names(data())[2]),
          selectInput(ns("covars"), "Covariates (optional):",
                      choices = names(data())[ !names(data()) %in% c(input$ID, input$traj, input$age, input$timePoint) ] ,
                      selected = NULL,
                      multiple = TRUE),
          selectInput(ns("modelType"), "Model Type:", choices = c("Linear", "Quadratic", "Cubic", "Quartic")),
          actionButton(ns("button"), "Run Model")
        )
      })

      # add what type of model to run and input the different formula here
      # This only runs when the user clicks on the button
      modelForm <- eventReactive(input$button, {
        if(input$modelType == "Linear"){
          form <- paste0(input$traj," ~ ", input$age, " + ", "(", input$age, "|" , input$ID, ")")
        } else if(input$modelType == "Quadratic"){
          form <- paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2) + (1 + ", input$age, " + I(",input$age, "^2) |" , input$ID, ")" )
        } else if(input$modelType == "Cubic"){
          form <- paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2)", " + I(", input$age   ,"^3)" ," + (1 + ", input$age, " + I(",input$age, "^2) |" , input$ID, ")")
        } else if(input$modelType == "Quartic"){
          form <- paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2)", " + I(", input$age   ,"^3)" , " + I(", input$age   ,"^4)" ," + (1 + ", input$age, " + I(",input$age, "^2) |" , input$ID, ")")
        }
        form
      })

      modelFormCovars <- reactive({
        req(modelForm())
      if(!is.null(input$covars)){
        form <- paste0(modelForm(), " + ", paste0(input$covars, collapse = " + "))
      } else {
        form <- modelForm()
      }
        form
      })


      return(
        list(
          button = reactive({ input$button }),
          data = data,
          modelForm = modelForm,
          modelFormCovars = modelFormCovars,
          ID = reactive({ input$ID }),
          traj = reactive({ input$traj }),
          age = reactive({ input$age }),
          timePoint = reactive({ input$timePoint }),
          modelType = reactive({ input$modelType }),
          covars = reactive({ input$covars })
        )
      )
    }
  )
}
