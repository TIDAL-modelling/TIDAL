#' UI for individual trajectories
#'
#' @import broom.mixed
#' @import lme4
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @import shinycssloaders
#'
#' @keywords internal
#' @export
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
        withSpinner(plotOutput(ns("trajPlot")), proxy.height = "100px"),
        p("Original data:"),
        withSpinner(plotOutput(ns("trajPlot_original")), proxy.height = "100px")
      )
    )
  )

}


