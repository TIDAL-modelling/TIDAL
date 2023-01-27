#' Shiny module for data exploration page
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
dataExplorationUI <- function(id, label = "data") {
  ns <- NS(id)


  tabsetPanel(
    tabPanel("Instructions",
             tagList(
               h2("Initial data exploration"),
               p("Either upload a long format dataframe (csv or tsv) or use the data frame you formatted on the previous page. Then select the columns you wish to use as variables in your model. Inspect the descriptive statistics of your trajectory variable at each time point. Select the model type (eg. linear or a polynomial model) and view the plot of the mean trajectory against these models.")
             )),
    tabPanel("Output",
             sidebarLayout(
               sidebarPanel(

                 tagList(
                   selectInput(ns("select"), "Select a dataset:", c("Upload a long format dataset", "Data formatted on previous page") ),
                   uiOutput(ns("uploadFile"))
                 ),
                 tagList(
                   selectInput(ns("ID"), "Participant ID variable:", choices = c()),
                   selectInput(ns("traj"), "Variable to model trajectory on, eg. depression scores:", choices =  c()),
                   selectInput(ns("age"), "Variable for time point:", choices =  c()),
                   # selectInput(ns("covars"), "Select any covariates to use in the model", choices =  c(), multiple = TRUE),
                   selectInput(ns("modelType"), "Model Type:", choices = c("Linear", "Quadratic", "Cubic", "Quartic"))
                 )
                 ),

               mainPanel(

                 tagList(
                   htmlOutput(ns("formulaText")),
                   br(),
                   tableOutput(ns("desc")),
                   tableOutput(ns("modelStatsFixed")),
                   tableOutput(ns("modelStatsRandom"))
                 ),

                 tagList(
                   plotOutput(ns("mainPlot"))
                 )

               ))
    )
  )


}
