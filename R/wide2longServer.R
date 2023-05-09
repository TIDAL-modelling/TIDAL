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
#' @import stringr
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
      # Depending on dataSource either a file is uploaded or synthetic data is used (loaded object in package)
      # Add an upload button
      # Render other UIs

      # -------------------------------
      # Render upload
      output$uploadControls <- renderUI(
        if(input$dataSource == "Upload data"){
          tagList(
            p("Upload a wide format longitudinal dataset:"),
            fileInput(ns("upload"), NULL)
          )
        }
      )

      # -------------------------------
      # If synthetic then use synthetic
      # If upload then upload data
      info <- reactive({
        if(input$dataSource == "Upload data"){
          # check file is uploaded and read it in
          inFile <- input$upload
          req(inFile)
          f <- fread(inFile$datapath)
        }else{
          f <- TIDAL::emot_reg_emot_simulated
        }
        f
      })

      # -------------------------------
      # Create variables options from column names as these will be choices for the user
      vars <- reactive({
        colnames(info())
      })

      # -------------------------------
      # Render additional UI with options as col names
      output$moreControls <- renderUI({
        tagList(
          selectInput(ns("ageCols"), "Select columns for age at each time point:", choices = vars(), multiple = TRUE),
          selectInput(ns("depCols"), "Select columns for the phenotype (eg. depression) at each time point:", choices = vars(), multiple = TRUE),
          textInput(ns("age"), "Name of new column for age:", value = "age"),
          textInput(ns("time_point"), "Name of new column for time point:", value = "time_point"),
          textInput(ns("dep"), "Name of new column for phenotype:", value = "dep"),
          textInput(ns("dep_cat"), "Name of new column for phenotype category:", value = "dep_cat"),
          checkboxInput(ns("ageImpute"), "Do you want to impute missing age?", value = TRUE )
        )
      })

      # add a message to user on some instructions
      output$warningMsgEmpty <- renderText({
        ifelse(is.null(input$ageCols) | is.null(input$depCols) & nrow(info()) > 0, '<b style="color:black">Select columns for age and phenotype, in chronological order.</b>', '')
      })


      # -------------------------------
      # We want the user to select the same number of age and depression columns,
      # so include a message telling them to do this when they are not equal
      output$warningMsgLen <- renderText({
        ifelse(length(input$ageCols) != length(input$depCols), '<b style="color:red">Select the same number of columns for age and phenotype.</b>', '')
      })

      # -------------------------------
      # We want the user to not type in names that match existing column names
      output$warningMsgColName <- renderText({
        ifelse(c(input$dep_cat, input$dep,input$time_point,input$age) %in%  colnames(info())  ,
               '<b style="color:red">Please type in column names that are unique and do not already exist in your dataset.</b>', '')
      })
      # -------------------------------
      # Convert the wide dataframe to long format
      dataLong <- reactive({
        # only run when the user has inputted all the variables correctly (it's up to the user to input them in the correct order)
        validate(
          need(length(input$ageCols) == length(input$depCols), "")
        )

        validate(
          need(length(input$ageCols) > 0, "")
        )

        validate(
          need(length(input$depCols) > 0, "")
        )

        # default is to impute age with the mean for each time point if it's missing
        if(isTRUE(input$ageImpute)){
        # Impute mean age where age is missing
        dataWide <- info() %>%
          mutate_at(all_of( input$ageCols ), ~replace(., is.na(.), mean(., na.rm = T)))
        }else{
          dataWide <- info()
        }

        # now convert wide to long
        dataDep <- dataWide %>%
          mutate(!!input$dep := as.numeric(!!input$dep)) %>%
          mutate(!!input$dep_cat := as.factor(!!input$dep_cat)) %>%
          gather(!!input$dep_cat, !!input$dep, all_of(input$depCols))

        dataLong <- dataWide %>%
          mutate(!!input$age := as.numeric(!!input$age)) %>%
          mutate(!!input$time_point := as.factor(!!input$time_point)) %>%
          gather(!!input$time_point, !!input$age, all_of(input$ageCols))

        dataLong[,input$dep_cat] <- dataDep[,input$dep_cat]
        dataLong[,input$dep] <- dataDep[,input$dep]

        dataLong <- dataLong %>%
          relocate(c(!!input$time_point, !!input$age, !!input$dep_cat, !!input$dep), .after = 1)

        dataLong
      })

      # show a preview of the new long dataframe
      output$preview <- renderTable({
        dataLong() %>%
          select(1:5) %>%
          head()
      })

      # -------------------------------
      # Download button appears only when dataframe has been converted to long format and dataLong() exists
      output$downloadDataButton <- renderUI({
      req(dataLong())
      downloadButton(ns("downloadData"), "Download .csv")
      })

      # -------------------------------
      # add an option to download the long format dataframe
      output$downloadData <- downloadHandler(
        filename = function(){
          if(input$dataSource == "Upload data"){
            paste0(word(input$upload$name, 1, sep = "\\."), "_LongFormat_", Sys.Date(), ".csv")
          }else{
            "emot_reg_emot_simulated_LongFormat.csv"
          }
        },
        content = function(file){
          write.csv(dataLong(), file, row.names = F, quote = F)
        }
      )

      return(dataLong)
    }
  )
}

