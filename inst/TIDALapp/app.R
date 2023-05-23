# R Shiny app code, calls module functions exported in R/

library(shinythemes)

# Increase maximum file size that can be uploaded
options(shiny.maxRequestSize = 30*1024^2)

# User interface
welcome_page <- tabPanel(
  title = "Overview",
  fluidPage(
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
                        <a href="https://github.com/AmeliaES/TIDAL#synthetic-datasets" style="color:blue" target="_blank">
                        TIDAL GitHub repository</a>
                        .</strong>')
    ),
    p('To use this tool please read the "Instructions" tab on each page to guide you through the process. In brief the main aims of each page:'),
    h4("Data Preparation"),
    p("This allows the user to upload a wide format of their longitudinal dataset. Select which columns measure time and the phenotype they want to model trajectories on. Converts the dataframe to long format. Allows the user to download the long format dataset."),
    h4("Data Exploration"),
    p("This is the first stage of the trajectory modelling. Here the user either uploads a long format dataset or uses the dataset formatted on the previous page (Data Preparation). They specify the columns relatated to the variables to include in the model. There is a choice of model type and the user can see which model type looks like it best fits their data to explore further on the following pages."),
    h4("Group Interactions"),
    p("Split the trajectories by varaibles to examine the differences in trajectories."),
    h4("Individual Trajectories"),
    p("View trajectories for specific individuals. Choose from a random sample, specific individuals of interest, individuals within a specific variable, eg. a random sample of females only."),
    h4("Points of acceleration (In Development)"),
    p("Examine timing of peak velocity of trajectories. This feature highlights a critical period at which further support or interventions could be introduced to dramatically shift an individual’s illness trajectory."),
    h4("Stability (In Development)"),
    p("Captures within-individual variability in depressive symptoms over time and compare how this varies by different forms of interventions or combinations of interventions.")
  )
)

format_page <- tabPanel(
  title = "Data Preparation",
  fluidPage(
    TIDAL:::wide2longUI("wide2long")
  )
)


overview_page <-   tabPanel(
  title = "Data Exploration",
  fluidPage(
    tabsetPanel(
      tabPanel("Instructions",
               tagList(
                 h2("Initial data exploration"),
                 tags$div(
                   HTML('<strong style="color:red">Please only upload synthetic datasets available on the
                        <a href="https://github.com/AmeliaES/TIDAL#synthetic-datasets" style="color:blue" target="_blank">
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
    TIDAL:::modelCondUI("modelCond")
  )
)


singeTraj_page <-  tabPanel(
  title = "Individiual Trajectories",
  fluidPage(
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


ui <- navbarPage(
  title = "TIDAL",
  theme = shinytheme('cerulean'),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  welcome_page,
  format_page,
  overview_page,
  intervention_page,
  singeTraj_page
)

# Main server
server <- function(input, output, session) {
  wide2longServer <- TIDAL:::wide2longServer("wide2long")
  selectedDataServer <- TIDAL:::selectDataServer("select", dataFormatted=wide2longServer)
  modelRunServer <- TIDAL:::modelRunServer("modelRun",
                                           button = selectedDataServer$button,
                                           modelData = selectedDataServer$data,
                                           formCode = selectedDataServer$modelForm,
                                           formCodeCovars = selectedDataServer$modelFormCovars,
                                           traj = selectedDataServer$traj,
                                           age = selectedDataServer$age,
                                           timePoint = selectedDataServer$timePoint)
  modelResultsServer <- TIDAL:::modelResultsServer("modelResults",
                                                   modelFit = modelRunServer$fit,
                                                   warningMsg = modelRunServer$warning,
                                                   modelData = selectedDataServer$data,
                                                   age = selectedDataServer$age)
  modelPlotServer <- TIDAL:::modelPlotServer("modelPlot",
                                             modelData = modelRunServer$data,
                                             modelFit = modelRunServer$fit,
                                             age = selectedDataServer$age,
                                             traj = selectedDataServer$traj,
                                             timePoint = selectedDataServer$timePoint,
                                             modelType = selectedDataServer$modelType,
                                             button = selectedDataServer$button
  )
  downloadExploreServer <- TIDAL:::downloadExploreServer("downloadExplore",
                                                         descTable = modelRunServer$mainTable,
                                                         warningMsg = modelRunServer$warning,
                                                         formCodeRender = modelResultsServer$modelFormRender,
                                                         statement = modelResultsServer$statement,
                                                         fixedTab = modelResultsServer$fixedTab,
                                                         randomTab = modelResultsServer$randomTab,
                                                         N = modelResultsServer$N,
                                                         mainPlot = modelPlotServer$mainPlot
  )
  modelCondServer <- TIDAL:::modelCondServer("modelCond",
                                             modelData = modelRunServer$data,
                                             formCodeCovars = selectedDataServer$modelFormCovars,
                                             dfPlot = modelPlotServer$df.plot,
                                             traj = selectedDataServer$traj,
                                             age = selectedDataServer$age,
                                             timePoint = selectedDataServer$timePoint,
                                             modelType = selectedDataServer$modelType)
  singleTrajServer <- TIDAL:::singleTrajServer("singeTraj",
                                               subject = selectedDataServer$ID,
                                               age = selectedDataServer$age,
                                               modelData = modelRunServer$data,
                                               modelFit = modelRunServer$fit)
}

# Run the application
shinyApp(ui = ui, server = server)




