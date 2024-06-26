# R Shiny app code, calls module functions exported in R/

library(shinythemes)
library(ggplot2)
library(bslib)

# Increase maximum file size that can be uploaded
options(shiny.maxRequestSize = 400*1024^2)

# Set default ggplot colour palette to colourblind-friendly Okabe Ito (2008)
options(ggplot2.discrete.colour= c("#E69F00","#56B4E9","#009E73","#F5C710",
                                   "#0072B2","#D55E00","#CC79A7",
                                   "#999999","#000000"))

# Set global theme for ggplot
my_theme <- function(base_size = 16, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_line(colour = "grey88"),
      panel.grid.minor = element_line(colour = "grey99"),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      panel.ontop = FALSE
    )
}

theme_set(my_theme())


## Accessibility features - dark mode, high contrast, large font
light <- bs_theme(version = version_default(), bootswatch = "cerulean")
dark <- bs_theme(version = version_default(), bootswatch = "darkly")
contrast <- bs_theme(version = version_default(),
                     bg = "#000000",
                     fg = "#FFFFFF",
                     primary = "#FFFF00",
                     secondary = "#EA80FC",
                     success = "#00FF00",
                     info = "#00ffff",
                     warning = "#FFCF00",
                     danger = "#FFFF00")
large <- bs_theme(version = version_default(), bootswatch = "cerulean", font_scale = 3)
largecontrast <- bs_theme(version = version_default(),
                          bg = "#000000",
                          fg = "#FFFFFF",
                          primary = "#FFFF00",
                          secondary = "#EA80FC",
                          success = "#00FF00",
                          info = "#00ffff",
                          warning = "#FFCF00",
                          danger = "#FFFF00",
                          font_scale = 3)




# User interface
welcome_page <- tabPanel(
  title = "Overview",

  #### this bit should allow the user to select from a list of themes
  radioButtons("Theme",
               "",
               choiceNames = list(tags$span(style = "font-size: 25px;", "Default Theme"),
                                  tags$span(style = "font-size: 25px;", "Dark Mode") ,
                                  tags$span(style = "font-size: 25px;", "High Contrast"),
                                  tags$span(style = "font-size: 25px;", "Large Font") ,
                                  tags$span(style = "font-size: 25px;", "High Contrast & Large Font") ),
               choiceValues = list("Default Theme", "Dark Mode", "High Contrast", "Large Font", "High Contrast & Large Font"),
               inline = TRUE),
  fluidPage(
    theme = bs_theme(version = version_default(), bootswatch = "cerulean"),
    HTML('<center><img src="TIDAL.png" width="550"></center>'),
    h1("Tool to Implement Developmental Analyses of Longitudinal Data"),
    p("The aim is for this digital tool to facilitate trajectories work and remove barriers to implementing longitudinal research to researchers without specialist statistical backgrounds. The following pages guide trajectory modelling and capture clinically meaningful features from mental health trajectories for specific individuals and/or specific groups of people."),
    p("These features will include:"),
    tags$ul(
      tags$li("How mental health is changing over specific periods of time."),
      tags$li("When mental health is improving or worsening at the fastest rate (points of acceleration)."),
      tags$li("How variable mental health responses are over time within individuals (stability).")
    ),
    tags$div(
      HTML('<strong style="color:red">Please only use synthetic data if using the application online. Details of this data is available on the
                        <a href="https://github.com/TIDAL-modelling/TIDAL#synthetic-datasets" style="color:blue" target="_blank">
                        TIDAL GitHub repository</a>
                        .</strong>')
    ),
    HTML('To use this tool please read the "Instructions" tab on each page to guide you through the process. There is also an instruction manual on the GitHub repository <a href="https://github.com/TIDAL-modelling/TIDAL/blob/main/Documentation/Instructions.pdf" style="color:blue" target="_blank">here</a>. In brief the main aims of each page:'),
    h4("Data Preparation"),
    p("This allows the user to upload a wide format of their longitudinal dataset. Select which columns measure time and the phenotype they want to model trajectories on. Converts the dataframe to long format. Allows the user to download the long format dataset."),
    h4("Data Exploration"),
    p("This is the first stage of the trajectory modelling. Here the user either uploads a long format dataset or uses the dataset formatted on the previous page (Data Preparation). They specify the columns relatated to the variables to include in the model. There is a choice of model type and the user can see which model type looks like it best fits their data to explore further."),
    h4("Interaction Variable"),
    p("Explore the effect of a categorical or continuous variable on the trajectories."),
    h4("Individual Trajectories"),
    p("View trajectories for specific individuals. Choose from a random sample, specific individuals of interest, individuals within a specific variable, eg. a random sample of females only."),
    # h4("Points of acceleration (In Development)"),
    # p("Examine timing of peak velocity of trajectories. This feature highlights a critical period at which further support or interventions could be introduced to dramatically shift an individual’s illness trajectory."),
    # h4("Stability (In Development)"),
    # p("Captures within-individual variability in depressive symptoms over time and compare how this varies by different forms of interventions or combinations of interventions.")
    HTML('<left><img src="wellcome-logo-black.jpg" width="100"></left>'),
    p("Funded by The Wellcome Trust and Social Finance, Grant Ref: 226686/Z/22/Z."),
    p("This package is provided solely for educational and informational purposes. Users understand and agree that any data uploaded and utilised with this package is done at their own risk. Users are solely responsible for the accuracy, legality, and ethical considerations of the data they upload. Additionally, users are responsible for the interpretation of results obtained through the use of this package. The creators and maintainers of this package shall not be held liable for any consequences arising from the use, interpretation, or implications of the package or the data uploaded.")
  )
)

format_page <- tabPanel(
  title = "Data Preparation",
  fluidPage(
    theme = bs_theme(version = version_default(), bootswatch = "cerulean"),
    TIDAL:::wide2longUI("wide2long")
  )
)


overview_page <-   tabPanel(
  title = "Data Exploration",
  fluidPage(
    theme = bs_theme(version = version_default(), bootswatch = "cerulean"),
    tabsetPanel(
      tabPanel("Instructions",
               tagList(
                 h2("Initial data exploration"),
                 tags$div(
                   HTML('<strong style="color:red">Please only upload synthetic datasets available on the
                        <a href="https://github.com/TIDAL-modelling/TIDAL#synthetic-datasets" style="color:blue" target="_blank">
                        TIDAL GitHub repository</a>
                        if using the application online.</strong>')
                 ),
                 p("Either upload a long format dataframe (.csv or .tsv) or use the data frame you formatted on the previous page. If you are uploading a long format dataframe then columns must have unique names.
                   Then select the columns you wish to use as variables in your model. Inspect the descriptive statistics of your trajectory variable at each time point.
                   Select the model type (eg. linear or a polynomial model) and view the plot of the mean trajectory against these models. You are also able to add covariates to the model which are plotted with any categorical covariates set to zero in the plot.")
               )),
      tabPanel("Output",
               sidebarLayout(
                 sidebarPanel(
                   TIDAL:::selectDataUI("select")),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Descriptive Statistics",
                              TIDAL:::modelRunUI("modelRun")),
                     tabPanel("Model Results",
                              TIDAL:::modelResultsUI("modelResults")),
                     tabPanel("Plot",
                              TIDAL:::modelPlotUI("modelPlot")),
                     tabPanel("Scores At Ages",
                              TIDAL:::datExAltUI("datExAlt")),
                     tabPanel("Area Under Curve",
                              TIDAL:::datExAUCUI("datExAUC")),
                     tabPanel("Download Results",
                              TIDAL::downloadExploreUI("downloadExplore"))
                   )
                 ))
      )
    )
  )
)

intervention_page <- tabPanel(
  title = "Interaction Variable",
  fluidPage(
    theme = bs_theme(version = version_default(), bootswatch = "cerulean"),
    TIDAL:::modelCondUI("modelCond")
  )
)


singeTraj_page <-  tabPanel(
  title = "Individual Trajectories",
  fluidPage(
    theme = bs_theme(version = version_default(), bootswatch = "cerulean"),
    tabsetPanel(
      tabPanel("Instructions",
               tagList(
                 h4("Explore individual trajectories using model estimates"),
                 p("Using the model specified in the Data Exploration page, we can have a look at individuals trajectories.")
               )
      ),
      tabPanel("Analysis",
               TIDAL:::singleTrajUI("singeTraj")
      )
    )
  )
)

# importantTimepoint_page <- tabPanel(
#   title = "Important Time Points",
#   fluidPage(
#     theme = bs_theme(version = version_default(), bootswatch = "cerulean"),
#     tabsetPanel(
#       tabPanel("Instructions",
#                tagList(
#                  h4("Important Time Points"),
#                  p("Determine the age where symptoms or scores are their maximum.
#               No user input is required as the model determined on the Data Exploration page is carried forward.")
#                )),
#
#       TIDAL:::importantAgeUI("importantAge")
#
#     )
#   )
# )


ui <- navbarPage(
  title = "TIDAL",
  theme = bs_theme(version = version_default(), bootswatch = "cerulean"),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ), #### i thought this might be a problem but removing it doesn't help #### HELP PLS
  ### setting it to bs_theme(version = 5, bootswatch = "cerulean") doesn't fix it though
  welcome_page,
  format_page,
  overview_page,
  intervention_page,
  singeTraj_page#,
  # importantTimepoint_page
)

# Main server
server <- function(input, output, session) {
  wide2longServer <- TIDAL:::wide2longServer("wide2long")
  selectDataServer <- TIDAL:::selectDataServer("select", dataFormatted=wide2longServer)
  modelRunServer <- TIDAL:::modelRunServer("modelRun",
                                           covariateChoice = selectDataServer$covariateChoice,
                                           button = selectDataServer$button,
                                           modelData = selectDataServer$data,
                                           formCodeCovars = selectDataServer$modelFormCovars,
                                           traj = selectDataServer$traj,
                                           age = selectDataServer$age,
                                           timePoint = selectDataServer$timePoint,
                                           weights = selectDataServer$weights,
                                           weightCol  = selectDataServer$weightCol)
  modelResultsServer <- TIDAL:::modelResultsServer("modelResults",
                                                   modelFit = modelRunServer$fit,
                                                   warningMsg = modelRunServer$warning,
                                                   modelData = selectDataServer$data,
                                                   modelType = selectDataServer$modelType,
                                                   randomFX = selectDataServer$randomFX,
                                                   age = selectDataServer$age,
                                                   traj = selectDataServer$traj,
                                                   covars = selectDataServer$covars)
  modelPlotServer <- TIDAL:::modelPlotServer("modelPlot",
                                             modelData = modelRunServer$data,
                                             modelFit = modelRunServer$fit,
                                             age = selectDataServer$age,
                                             traj = selectDataServer$traj,
                                             timePoint = selectDataServer$timePoint,
                                             modelType = selectDataServer$modelType,
                                             button = selectDataServer$button
  )
  datExAltServer <- TIDAL:::datExAltServer("datExAlt",
                                           modelDataEdit = modelPlotServer$modelDataEdit,
                                           modelFit = modelRunServer$fit,
                                           modelType = selectDataServer$modelType,
                                           traj = selectDataServer$traj
                                           )
  datExAUCServer <- TIDAL:::datExAUCServer("datExAUC",
                                           modelDataEdit = modelPlotServer$modelDataEdit,
                                           modelFit = modelRunServer$fit,
                                           modelType = selectDataServer$modelType,
                                           traj = selectDataServer$traj,
                                           button = selectDataServer$button
  )

  downloadExploreServer <- TIDAL:::downloadExploreServer("downloadExplore",
                                             descTable = modelRunServer$mainTable,
                                             warningMsg = modelRunServer$warning,
                                             formCodeRender = modelResultsServer$modelFormRender,
                                             statement = modelResultsServer$statement,
                                             fixedTab = modelResultsServer$fixedTab,
                                             interpretation = modelResultsServer$interpretation,
                                             interpretationRand = modelResultsServer$interpretationRand,
                                             randomTab = modelResultsServer$randomTab,
                                             N = modelResultsServer$N,
                                             mainPlot = modelPlotServer$mainPlot,
                                             phenotype = selectDataServer$traj,
                                             modelType = selectDataServer$modelType,
                                             datExAltTable = datExAltServer$datExAltTable,
                                             datExAltPlot = datExAltServer$datExAltPlot,
                                             AUC = datExAUCServer$AUC,
                                             plotAUC = datExAUCServer$plotAUC,
                                             tableAUC = datExAUCServer$tableAUC

  )
  modelCondServer <- TIDAL:::modelCondServer("modelCond",
                                             modelData = modelRunServer$data,
                                             formCodeCovars = selectDataServer$modelFormCovars,
                                             dfPlot = modelPlotServer$df.plot,
                                             traj = selectDataServer$traj,
                                             age = selectDataServer$age,
                                             covars = selectDataServer$covars,
                                             timePoint = selectDataServer$timePoint,
                                             modelType = selectDataServer$modelType,
                                             randomFX = selectDataServer$randomFX)

  singleTrajServer <- TIDAL:::singleTrajServer("singeTraj",
                                               subject = selectDataServer$ID,
                                               age = selectDataServer$age,
                                               traj = selectDataServer$traj,
                                               modelData = modelRunServer$data,
                                               modelFit = modelRunServer$fit,
                                               modelType = selectDataServer$modelType,
                                               cov = selectDataServer$covars)
  importantAgeServer <- TIDAL:::importantAgeServer("importantAge",
                                                   modelDataEdit = modelPlotServer$modelDataEdit,
                                                   modelType = selectDataServer$modelType,
                                                   modelFit = modelRunServer$fit,
                                                   age = selectDataServer$age)
  observe(session$setCurrentTheme(
    if (input$Theme == "Dark Mode"){
        dark
      }else if(input$Theme == "High Contrast"){
        contrast
      }else if(input$Theme == "Large Font"){
        large
      }else if(input$Theme == "High Contrast & Large Font"){
        largecontrast
      }else{
        light
      }
    ))
}

# Run the application
shinyApp(ui = ui, server = server)
