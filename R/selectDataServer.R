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
          checkboxInput(ns("toYears"), "Convert age in months to years?", value = FALSE),
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


        )
      })

      randomFX_choices <- reactive({
        req(data())
        if(input$modelType == "Linear"){
          randomFX_choices <- c("No random slope", "Linear")
        } else if(input$modelType %in% c("Quadratic", "Cubic","Quartic") ){
          randomFX_choices <- c("No random slope", "Linear", "Linear and Quadratic")
        }
        randomFX_choices
      })

      output$randomFX_UI <- renderUI({
        req(data())
        selectInput(ns("randomFX"), "Choose random slope terms:", choices = randomFX_choices(), multiple = FALSE)
      })

      output$button_UI <- renderUI({
        req(data())
        actionButton(ns("button"), "Run Model")
      })


      # edit dataframe age column if convert to years is ticked
      dataEdit <- reactive({
        if (isTRUE(input$toYears)) {
          dataEdit <- data() %>% mutate_at(vars(input$age), ~./12) %>%
            mutate_at(vars(all_of(input$covarsCat)), factor)
        } else {
          dataEdit <- data() %>%
            mutate_at(vars(all_of(input$covarsCat)), factor)
        }
      })


      # edit dataframe so that categorical covariates are factorised and polynomial age columns are added


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
        # Linear model and linear random effect
        if(input$modelType == "Linear" & input$randomFX == "Linear"){
          form <- paste0(input$traj," ~ ", input$age, " + ", "(1 + ", input$age, "|" , input$ID, ")")
        # Linear model and no random effects
        } else if(input$modelType == "Linear" & input$randomFX == "No random slope"){
          form <- paste0(input$traj," ~ ", input$age, " + ", "( 1 |" , input$ID, ")")

          # Quadratic model and no random effects
        } else if(input$modelType == "Quadratic" & input$randomFX == "No random slope" ){
          form <- paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2) + (1 |" , input$ID, ")" )
          # Cubic model and no random effects
        } else if(input$modelType == "Cubic" & input$randomFX == "No random slope"){
          form <- paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2)", " + I(", input$age   ,"^3)" ," + (1 |" , input$ID, ")")
          # Quartic model  and no random effects
        } else if(input$modelType == "Quartic" & input$randomFX == "No random slope"){
          form <- paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2)", " + I(", input$age   ,"^3)" , " + I(", input$age   ,"^4)" ," + (1 |" , input$ID, ")")

          # Quadratic model and linear  random effects
        } else if(input$modelType == "Quadratic" & input$randomFX == "Linear" ){
          form <- paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2) + (1 + ", input$age, " |" , input$ID, ")" )
          # Cubic model and linear random effects
        } else if(input$modelType == "Cubic" & input$randomFX == "Linear"){
          form <- paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2)", " + I(", input$age   ,"^3)" ," + (1 + ", input$age, " |" , input$ID, ")")
          # Quartic model  and linear random effects
        } else if(input$modelType == "Quartic" & input$randomFX == "Linear"){
          form <- paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2)", " + I(", input$age   ,"^3)" , " + I(", input$age   ,"^4)" ," + (1 + ", input$age, " |" , input$ID, ")")

        # Quadratic model and linear and quadratic random effects
        } else if(input$modelType == "Quadratic" & input$randomFX == "Linear and Quadratic" ){
          form <- paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2) + (1 + ", input$age, " + I(",input$age, "^2) |" , input$ID, ")" )
        # Cubic model and linear and quadratic random effects
        } else if(input$modelType == "Cubic" & input$randomFX == "Linear and Quadratic"){
          form <- paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2)", " + I(", input$age   ,"^3)" ," + (1 + ", input$age, " + I(",input$age, "^2) |" , input$ID, ")")
        # Quartic model  and linear and quadratic random effects
        } else if(input$modelType == "Quartic" & input$randomFX == "Linear and Quadratic"){
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
