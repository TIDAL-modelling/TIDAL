#' Split model by variable
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
                       br(),
                       htmlOutput(ns("warning")),
                       textOutput(ns("zScore")),
                       h4("Fixed Effects"),
                       withSpinner(tableOutput(ns("modelStatsFixed")), proxy.height = "100px"),
                       withSpinner(tableOutput(ns("modelIntFixed")), proxy.height = "100px"),
                       h4("Random Effects"),
                       withSpinner(tableOutput(ns("modelStatsRandom")), proxy.height = "100px"),
                       withSpinner(htmlOutput(ns("interRandom")), proxy.height = "100px")
                     )
            ),
            tabPanel("Plot",
                     tagList(
                     checkboxInput(ns("plotCheckbox"), "Do you want an overlay of the descriptive plot?", TRUE, width = '100%'),
                     uiOutput(ns("plotCheckboxLevelsUI")),
                     withSpinner(plotOutput(ns("modelCondPlot")), proxy.height = "100px")
                     )
            ),
            tabPanel("Scores At Ages",
                       tagList(
                         uiOutput(ns("selectAgeScore")),
                         uiOutput(ns("levelsScoresUI")),
                         withSpinner(plotOutput(ns("plotScore")), proxy.height = "100px"),
                         withSpinner(tableOutput(ns("tableScore")), proxy.height = "100px"),
                         tableOutput(ns("scoresDif"))
                       )
            ),
            tabPanel("Area Under Curve",
                     tagList(
                       textOutput(ns("AUCoverview")),
                       br(),
                       fluidRow(
                         column(width = 6, uiOutput(ns("levelsAUCUI"))),
                         column(width = 6, uiOutput(ns("AUCagesUI")))
                       ),
                       br(),
                       fluidRow(
                         column(width = 6, withSpinner(plotOutput(ns("AUCplot")), proxy.height = "100px")),
                         column(width = 6,
                                tagList(
                                  withSpinner(tableOutput(ns("AUCtable")), proxy.height = "100px"),
                                textOutput(ns("AUCdifText"))
                                )
                         )
                       ),
                       br(),
                       br()
                     )
            ),
            tabPanel("Download Results",
                     tagList(
                       p("When you have run the model with your interaction variable of choice, please download a pdf report of the interaction variable analyses."),
                       p('Note the download button will only appear once you have clicked "Run Model" in the side panel.'),
                       br(),
                       uiOutput(ns("buttonHere")))
          )
        )
      )
    )
  )
)

}

