# R Shiny app code, calls module functions exported in R/

library(shinythemes)

# Run module functions
appDir <- system.file("shinyApp", package = "trajMods")
source(paste0(appDir, "/modules.R"))

# Increase maximum file size that can be uploaded
options(shiny.maxRequestSize = 20*1024^2)

# User interface
format_page <- tabPanel(
  title = "Data Preparation",
  fluidPage(
    wide2longUI("wide2long")
  )
)


overview_page <-   tabPanel(
  title = "Data Exploration",
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectDataUI("select"),
        varsSelectUI("varsSelect")),
      mainPanel(
        modelRunUI("modelRun"),
        modelPlotUI("modelPlot")
        )
    )
  )
)

intervention_page <- tabPanel(
  title = "Analysis",
  fluidPage(
    modelCondUI("modelCond")
  )
)


ui <- navbarPage(
  title = "Multi-Level Modelling",
  theme = shinytheme('united'),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  format_page,
  overview_page,
  intervention_page
)

# Main server
server <- function(input, output, session) {
  wide2longServer <- wide2longServer("wide2long")
  selectedDataServer <- selectDataServer("select", dataFormatted=wide2longServer)
  varsSelectServer <- varsSelectServer("varsSelect", varsSelectData=selectedDataServer)
  modelRunServer <- modelRunServer("modelRun", modelData = selectedDataServer, formCode = varsSelectServer)
  modelPlotServer <- modelPlotServer("modelPlot", modelData = selectedDataServer, modelFit = modelRunServer)
  modelCondServer <- modelCondServer("modelCond", modelData = selectedDataServer, formCode = varsSelectServer, dfPlot = modelPlotServer)
}

# Run the application
shinyApp(ui = ui, server = server)




