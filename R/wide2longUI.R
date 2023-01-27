#' Shiny module - wide2longUI
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
#' @export
wide2longUI <- function(id, label = "wide2long") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  shinyjs::useShinyjs()

  sidebarLayout(
    sidebarPanel(
      tagList(
        p("Upload a wide format longitudinal dataset:"),
        fileInput(ns("upload"), NULL),
        uiOutput(ns("moreControls"))
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Instructions",
                 tagList(
                   h2("Convert longitudinal data from wide format to long format"),
                   p('In order to model trajectories R requires the data frame to be in "long" format. It\'s likely that the data is initially inputted as a "wide" format. This page allows you to upload wide formatted data and converts it to long. Similar to the image displayed below.'),
                   img(src = "wide2long.png", height = 350, width = 550),
                   h4("Steps:"),
                   tags$ol(
                     tags$li('Upload a wide format csv or tsv file of your dataset. This must be less than 30 MB. Click on the "Output" tab and proceed with the following steps.'),
                     tags$li("Select which columns correspond to age and the phenotype you want to model, eg. depression, at each time point. Make sure that you select these in the correct chronological order and have the same corresponding time points for the phenotype and age."),
                     tags$li('Default names for the new columns are "age", "occ", "dep" and "dep_cat". You can change these in the text boxes provided if you wish.'),
                     tags$li('If you have missing data for age it is recomended to impute the mean from each time point for this variable. There is a tick box you can check to do this. Default is to impute missing age with the mean.'
                     ),
                     tags$li('Now you can see a preview of the newly formatted long dataframe ("Output" tab). You have the option to download it (in .csv format) and also use for analysis on the subsequent pages.')
                   ),
                 )
        ),
        tabPanel("Output",
                 tagList(
                   htmlOutput(ns("warningMsgEmpty")),
                   htmlOutput(ns("warningMsgLen")),
                   tableOutput(ns("preview"))
                 )
        )
      )
    )
  )
}
