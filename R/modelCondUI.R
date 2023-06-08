#' Split model by variable
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
modelCondUI <- function(id, label = "Model Condition Run") {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      tagList(
        radioButtons(ns("varType"), "Explore a categorical or continuous variable?",
                     c("Categorical" = "cat",
                       "Continuous" = "cont")),
        selectInput(ns("condition"), "Select the variable:", choices = c()  ),
        p("Be aware that it may take a while for the model to run."),
        actionButton(ns("button"), "Run Model")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Instructions",
          tagList(
            h4("Split by variable analysis"),
            p("Using the model made on the previous page explore some variables which may influence the trajectory. Select from a list of variables (ie. column names) of the dataset to explore the trajectory by. If you have included variables as covariates in the previous page a warning message will occur if you try and explore the trajectory further by this variable.")
          )
        ),
        tabPanel(
          "Output",
          tabsetPanel(
            tabPanel("Model Results",
                     tagList(
                       textOutput(ns("warningCov")),
                       htmlOutput(ns("form")),
                       h4("Fixed Effects"),
                       tableOutput(ns("modelStatsFixed")),
                       h4("Random Effects"),
                       tableOutput(ns("modelStatsRandom"))
                     )
            ),
            tabPanel("Plot",
                     plotOutput(ns("modelCondPlot"))
            ),
            tabPanel("Scores At Ages",
                     tagList(
                       tagList(
                         uiOutput(ns("selectAgeScore")),
                         plotOutput(ns("plotScore")),
                         tableOutput(ns("tableScore"))
                       )
                     ))
          )
        )
      )
    )
  )


  }

