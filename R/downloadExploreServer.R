#' Server to download results from explore data page
#'
#' @import broom.mixed
#' @import lme4
#' @import dplyr
#' @import ggplot2
#' @import data.table
#' @import shinyjs
#' @import tidyr
#' @import purrr
#' @import tinytex
#' @import rmarkdown
#' @import kableExtra
#'
#' @noRd
#' @keywords internal
#' @export
downloadExploreServer <- function(id,
                            descTable,
                            warningMsg,
                            formCodeRender,
                            statement,
                            fixedTab,
                            randomTab,
                            N,
                            mainPlot,
                            phenotype,
                            modelType,
                            datExAltTable,
                            datExAltPlot,
                            AUC,
                            plotAUC,
                            tableAUC
                            ) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      # ------------------------------------------
      # Suffix for name:
      name <- reactive({
        if(input$suffix != ""){
          paste0("_", input$suffix, "_")
        }else{
          "_"
        }
      })

      # ------------------------------------------
      # Add UI to download results
      output$buttonHere <- renderUI({
        req(mainPlot())
        tagList(
          textInput(ns("suffix"),
                    "File name suffix:"
          ),
          downloadButton(ns("downloadReport"), "Download report")
          )
      })

      output$downloadReport <- downloadHandler(
        filename = function(){
          paste0("Explore_Data", name(), Sys.Date(), ".pdf")
        },
        content = function(file) {
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path("www/exploreData.Rmd")
          file.copy("exploreData.Rmd", tempReport, overwrite = TRUE)

          # Set up parameters to pass to Rmd document


          # if datExAltTable/Plot plotAUC/tableAUC don't exist then save those variables as NA values

          if( is.null(datExAltTable()) & length(AUC()) == 0 ){
            params <- list(
              descTable = descTable(),
              warningMsg = warningMsg(),
              formCodeRender = formCodeRender(),
              statement = statement(),
              fixedTab = fixedTab(),
              randomTab = randomTab(),
              N = N(),
              mainPlot = mainPlot(),
              phenotype = phenotype(),
              modelType = modelType(),
              datExAltTable = NA,
              datExAltPlot = NA,
              plotAUC = NA,
              tableAUC = NA
            )
          }else if( is.null(datExAltTable()) & length(AUC()) > 0 ){
            params <- list(
              descTable = descTable(),
              warningMsg = warningMsg(),
              formCodeRender = formCodeRender(),
              statement = statement(),
              fixedTab = fixedTab(),
              randomTab = randomTab(),
              N = N(),
              mainPlot = mainPlot(),
              phenotype = phenotype(),
              modelType = modelType(),
              datExAltTable = NA,
              datExAltPlot = NA,
              plotAUC = plotAUC(),
              tableAUC = tableAUC()
            )
          }else if( !is.null(datExAltTable()) & length(AUC()) == 0 ){
            params <- list(
              descTable = descTable(),
              warningMsg = warningMsg(),
              formCodeRender = formCodeRender(),
              statement = statement(),
              fixedTab = fixedTab(),
              randomTab = randomTab(),
              N = N(),
              mainPlot = mainPlot(),
              phenotype = phenotype(),
              modelType = modelType(),
              datExAltTable = datExAltTable(),
              datExAltPlot = datExAltPlot(),
              plotAUC = NA,
              tableAUC = NA
            )
          }else{
          params <- list(
            descTable = descTable(),
            warningMsg = warningMsg(),
            formCodeRender = formCodeRender(),
            statement = statement(),
            fixedTab = fixedTab(),
            randomTab = randomTab(),
            N = N(),
            mainPlot = mainPlot(),
            phenotype = phenotype(),
            modelType = modelType(),
            datExAltTable = datExAltTable(),
            datExAltPlot = datExAltPlot(),
            plotAUC = plotAUC(),
            tableAUC = tableAUC()
          )
          }

          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(tempReport, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }
      )

    }
  )
}

