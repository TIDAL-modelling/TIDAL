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
          # check for spaces in column names and change them to underscores
          colnames(data) <- gsub(" ", "_", colnames(data))
          # Replace special characters with an empty string in column names of the data frame
          colnames(data) <- gsub("[\\(\\)\\*]", "", colnames(data))
        }
        else {
          data <- dataFormatted()
          # check for spaces in column names and change them to underscores
          colnames(data) <- gsub(" ", "_", colnames(data))
          # Replace special characters with an empty string in column names of the data frame
          colnames(data) <- gsub("[\\(\\)\\*]", "", colnames(data))
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
          selectInput(ns("age"), "Variable for age at time point (continuous):", choices = names(select(data(), where(is.numeric)) ) , selected = names(select(data(), where(is.numeric)) )[2]),
          selectInput(ns("timePoint"), "Variable for time point (categorical):", choices = names(data()) , selected = names(data())[2]),
          selectInput(ns("covarsCat"), "Categorical Confounders (optional):",
                      choices = names(data()) ,
                      selected = NULL,
                      multiple = TRUE),
          selectInput(ns("covarsCont"), "Continuous Confounders (optional):",
                      choices = names(data()) ,
                      selected = NULL,
                      multiple = TRUE),
          selectInput(ns("modelType"), "Model Type:", choices = c("Linear", "Quadratic", "Cubic", "Quartic")),
          actionButton(ns("button"), "Run Model")
        )
      })

      # edit dataframe so that categorical covariates are factorised and polynomial age columns are added
      dataEdit <- reactive({
        dataEdit <- data() %>%
          mutate_at(vars(all_of(input$covarsCat)), factor)

      })

      covars <- reactive({
        c(input$covarsCat, input$covarsCont)
      })

      covariateChoice <- reactive({
        ( any(c(input$ID, input$traj, input$age, input$timePoint) %in% c(input$covarsCat, input$covarsCont )) |
              any(input$covarsCat %in% input$covarsCont) )
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
        if( (!is.null(input$covarsCat)) & (!is.null(input$covarsCont)) ){
          form <- paste0(modelForm(),
                         " + ",
                         paste0(input$covarsCat, collapse = " + "), " + ",
                         paste0(input$covarsCont, collapse = " + ")
          )
        }else if((is.null(input$covarsCat)) & (!is.null(input$covarsCont))){
          form <- paste0(modelForm(),
                         " + ",
                         paste0(input$covarsCont, collapse = " + ")
          )
        }else if((!is.null(input$covarsCat)) & (is.null(input$covarsCont))){
          form <- paste0(modelForm(),
                         " + ",
                         paste0(input$covarsCat, collapse = " + ")
          )
        }else {
          form <- modelForm()
        }
        form
      })


      return(
        list(
          button = reactive({ input$button }),
          data = dataEdit,
          modelForm = modelForm,
          modelFormCovars = modelFormCovars,
          ID = reactive({ input$ID }),
          traj = reactive({ input$traj }),
          age = reactive({ input$age }),
          timePoint = reactive({ input$timePoint }),
          modelType = reactive({ input$modelType }),
          covariateChoice = covariateChoice,
          covars = covars
        )
      )
    }
  )
}
