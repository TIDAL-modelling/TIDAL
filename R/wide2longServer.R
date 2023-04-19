#' This function takes a wide format dataset and converts it to a long format dataset.
#'
#' @param id The id of the module server
#'
#' @return Returns a long format dataset
#'
#' @import broom.mixed
#' @import lme4
#' @import dplyr
#' @import ggplot2
#' @import data.table
#' @import shinyjs
#' @import tidyr
#'
#' @noRd
#' @keywords internal
#' @export
#'
#' @examples
#' wide2longServer("server1")
wide2longServer <- function(id) {
  # create a module server
  moduleServer(
    # id is the id of the module
    id,
    function(input, output, session) {
      # create namespaced ID
      ns <- NS(id)

      # -------------------------------
      # When file has uploaded create extra UIs to interact with the uploaded file
      # choices are NULL for options we want to update with column names of the file uploaded
      info <- eventReactive(input$upload, {
        output$moreControls <- renderUI(
          tagList(
            selectInput(ns("ageCols"), "Select columns for age at each time point:", choices = NULL, multiple = TRUE),
            selectInput(ns("depCols"), "Select columns for the phenotype (eg. depression) at each time point:", choices = NULL, multiple = TRUE),
            textInput(ns("age"), "Name of new column for age:", value = "age"),
            textInput(ns("occ"), "Name of new column for time point:", value = "occ"),
            textInput(ns("dep"), "Name of new column for phenotype:", value = "dep"),
            textInput(ns("dep_cat"), "Name of new column for phenotype category:", value = "dep_cat"),
            checkboxInput(ns("ageImpute"), "Do you want to impute missing age?", value = TRUE ),
            downloadButton(ns("downloadData"), "Download .csv")
          )
        )
        # check file is uploaded and read it in
        inFile <- input$upload

        req(inFile)

        f <- fread(inFile$datapath)
        # make a new variable of the column names (this will be what the user selects from a drop down menu)
        vars <- colnames(f)

        # Update select input immediately after uploading file.
        updateSelectInput(session, "ageCols","Select columns for age at each time point:", choices = vars)
        updateSelectInput(session, "depCols","Select columns for the phenotype (eg. depression) at each time point:", choices = vars)

        # add a message to user on some instructions
        output$warningMsgEmpty <- renderText({
          ifelse(is.null(input$ageCols) | is.null(input$depCols), '<b style="color:black">Select columns for age and depression, in chronological order.</b>', '')
        })

        f
      })

      # -------------------------------
      # We want the user to select the same number of age and depression columns,
      # so include a message telling them to do this when they are not equal
      output$warningMsgLen <- renderText({
        ifelse(length(input$ageCols) != length(input$depCols), '<b style="color:red">Select the same number of columns for age and phenotype.</b>', '')
      })

      # -------------------------------
      # We want the user to not type in names that match exisiting column names
      output$warningMsgColName <- renderText({
        ifelse(c(input$dep_cat, input$dep,input$occ,input$age) %in%  colnames(info())  ,
               '<b style="color:red">Please type in colum names that are unique and do not already exist in your dataset.</b>', '')
      })
      # -------------------------------
      # Convert the wide dataframe to long format
      dataLong <- reactive({
        # only run when the user has inputted all the variables correctly (it's up to the user to input them in the correct order)
        validate(
          need(length(input$ageCols) == length(input$depCols), "")
        )

        # default is to impute age with the mean for each time point if it's missing
        if(isTRUE(input$ageImpute)){
        # Impute mean age where age is missing
        dataWide <- info() %>%
          mutate_at(vars( !!input$ageCols ), ~replace(., is.na(.), mean(., na.rm = T)))
        }else{
          dataWide <- info()
        }

        # now convert wide to long
        dataDep <- dataWide %>%
          gather(!!input$dep_cat, !!input$dep, all_of(input$depCols))

        dataLong <- dataWide %>%
          gather(!!input$occ, !!input$age, all_of(input$ageCols))

        dataLong[,input$dep_cat] <- dataDep[,input$dep_cat]
        dataLong[,input$dep] <- dataDep[,input$dep]

        dataLong <- dataLong %>%
          relocate(c(!!input$occ, !!input$age, !!input$dep_cat, !!input$dep), .after = 1)

        dataLong
      })

      # show a preview of the new long dataframe
      output$preview <- renderTable({
        dataLong() %>%
          select(1:5) %>%
          head()
      })

      # -------------------------------
      # add an option to download the long format dataframe
      output$downloadData <- downloadHandler(
        filename = function(){
          paste0(Sys.time(), 'LongFormat.csv')
        },
        content = function(file){
          write.csv(dataLong(), file)
        }
      )

      return(dataLong)
    }
  )
}

