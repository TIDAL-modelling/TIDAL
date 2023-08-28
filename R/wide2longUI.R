#' This function takes a wide format dataset and converts it to a long format dataset.
#'
#' @import broom.mixed
#' @import lme4
#' @import dplyr
#' @import ggplot2
#' @importFrom shinyjs useShinyjs
#' @import tidyr
#' @import shinyBS
#'
#' @keywords internal
#' @export
wide2longUI <- function(id, label = "wide2long") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  useShinyjs()

  sidebarLayout(
    sidebarPanel(
      tagList(
        radioButtons(ns("dataSource"), "Select data source:",
                    choices = c("Upload data", "Use demo synthetic data"),
                    selected = "Upload data"),
        uiOutput(ns("uploadControls")),
        uiOutput(ns("moreControls")),
        uiOutput(ns("downloadDataButton"))
        )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Instructions",
                 tagList(
                   h2("Convert longitudinal data from wide format to long format"),
                   tags$div(
                     HTML('<strong style="color:red">Please only use synthetic data if using the application online. Details of this data is available on the
                        <a href="https://github.com/AmeliaES/TIDAL#synthetic-datasets" style="color:blue" target="_blank">
                        TIDAL GitHub repository</a>
                        .</strong>')
                   ),
                   p('In order to model trajectories R requires the data frame to be in "long" format. It\'s likely that the data is initially inputted as a "wide" format. This page allows you to upload wide formatted data and converts it to long. Similar to the image displayed below.'),
                   img(src = "wide2long.png", height = 350, width = 550),
                   h4("Steps:"),
                   tags$ol(
                     tags$li('Upload a wide format comma separated *.csv or tab delimited *txt or *.tsv file of your dataset. It is recommended your longitudinal dataset has at least 4 time points. This must be less than 30 MB, column names must not have any spaces in them and missing data must be coded as "NA". Click on the "Output" tab and proceed with the following steps.'),
                     tags$li("Select which columns correspond to age and the variable you want to model, eg. depression, at each time point. Make sure that you select these in the correct chronological order and have the same corresponding time points for the variable you want to model and age."),
                     tags$li('Default names for the new columns are "age", "time_point" and "score". You can change these in the text boxes provided if you wish, but make sure they are unique and also are not names of columns that already exist in your dataset.'),
                     tags$li('If you have missing data for age there is an option to impute the mean from each time point for this variable. There is a tick box you can check to do this.'
                     ),
                     tags$li('Now you can see a preview of the newly formatted long dataframe ("Output" tab). You have the option to download it (in .csv format) and also use for analysis on the subsequent pages.')
                   ),
                 )
        ),
        tabPanel("Output",
                 tagList(
                   htmlOutput(ns("warningMsgEmpty")),
                   htmlOutput(ns("warningMsgLen")),
                   htmlOutput(ns("warningMsgColName")),
                   p('A preview of the first few columns of this long-formatted data is shown below. Please click the download button to see all columns and rows. Explore this data further on the next page "Data Exploration".'),
                   tableOutput(ns("preview"))
                 )
        )
      )
    )
  )
}
