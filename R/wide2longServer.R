#' Shiny module
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
wide2longServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

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
            verbatimTextOutput(ns("value")),
            downloadButton(ns("downloadData"), "Download .csv")
          )
        )

        inFile <- input$upload

        req(inFile)

        f <- fread(inFile$datapath)
        vars <- colnames(f)


        # Update select input immediately after uploading file.
        updateSelectInput(session, "ageCols","Select columns for age at each time point:", choices = vars)
        updateSelectInput(session, "depCols","Select columns for the phenotype (eg. depression) at each time point:", choices = vars)

        output$warningMsgEmpty <- renderText({
          ifelse(is.null(input$ageCols) | is.null(input$depCols), '<b style="color:black">Select columns for age and depression, in chronological order.</b>', '')
        })

        f

      })

      output$value <- renderText({ input$ageImpute })


      dataLong <- reactive({
        validate(
          need(length(input$ageCols) == length(input$depCols), "")
        )

        # if(input$ageImpute){
        # # Impute mean age where age is missing
        # dataWide <- info() #%>%
        #   # mutate_at(vars( !!input$occ ), ~replace(., is.na(.), mean(., na.rm = T)))
        # }else{
          dataWide <- info()
        # }

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

      output$preview <- renderTable({
        dataLong() %>%
          select(1:5) %>%
          head()
      })

      output$downloadData <- downloadHandler(
        filename = function(){
          paste0(Sys.time(), 'LongFormat.csv')
        },
        content = function(file){
          write.csv(dataLong(), file)
        }
      )

      output$warningMsgLen <- renderText({
        ifelse(length(input$ageCols) != length(input$depCols), '<b style="color:red">Select same number of columns for age and depression.</b>', '')
      })

      return(dataLong)
    }
  )
}

