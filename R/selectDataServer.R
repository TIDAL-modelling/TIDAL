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
            fileInput(ns("Upload a long format dataset"), NULL),
            p("Please wait for file to upload.")
            )
          })
          req(input$`Upload a long format dataset`)
          data <- fread(input$`Upload a long format dataset`$datapath)
        }
        else { # otherwise the user can use the data from the previous wide2long module using the dataFormatted argument
          output$uploadFile <- renderUI({
          })
          data <- dataFormatted()
        }
      })

      return(data)
    }
  )
}
