singleTrajUI <- function(id, label = "Model") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      tagList(
        selectInput(ns("choice"),
                    "Who's trajectories would you like to look at?",
                    c("Random Sample", "Select specific individuals", "A specific variable")),
        uiOutput(ns("random")),
        uiOutput(ns("specificIDs")),
        uiOutput(ns("specificVar"))
      )
    ),
    mainPanel(
      tagList(
      textOutput(ns("textIDs")),
      plotOutput(ns("trajPlot")),
      dataTableOutput(ns("table"))
      )
    )
  )

}


