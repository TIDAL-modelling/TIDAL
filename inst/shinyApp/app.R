# R Shiny app code, calls module functions exported in R/

library(shinythemes)

# Increase maximum file size that can be uploaded
options(shiny.maxRequestSize = 30*1024^2)

# User interface
welcome_page <- tabPanel(
  title = "Overview",
  fluidPage(
  h1("Multilevel Modelling of Longitudinal Data"),
  p("The aim is for this digital tool to facilitate trajectories work and remove barriers to implementing longitudinal research to researchers without specialist statistical backgrounds. The following pages guide trajectory modelling and capture clinically meaningful features from mental health trajectories for specific individuals and/or specific groups of people."),
  p("These features include:"),
  tags$ul(
    tags$li("How mental health is changing over specific periods of time."),
    tags$li("When mental health is improving or worsening at the fastest rate (points of acceleration)."),
    tags$li("How variable mental health responses are over time within individuals (stability).")
  ),
  p('To use this tool please read the "Instructions" tab on each page to guide you through the process. In brief the main aims of each page:'),
  h4("Data Preparation"),
  p("This allows the user to upload a wide format of their longitudinal dataset. Select which columns measure time and the phenotype they want to model trajectories on. Converts the dataframe to long format. Allows the user to download the long format dataset."),
  h4("Data Exploration"),
  p("This is the first stage of the trajectory modelling. Here the user either uploads a long format dataset or uses the dataset formatted on the previous page (Data Preparation). They specify the columns relatated to the variables to include in the model. There is a choice of model type and the user can see which model type looks like it best fits their data to explore further on the following pages."),
  h4("Group Interactions"),
  p("Split the trajectories by varaibles to examine the differences in trajectories."),
  h4("Points of acceleration"),
  p("Examine timing of peak velocity of trajectories. This feature highlights a critical period at which further support or interventions could be introduced to dramatically shift an individual’s illness trajectory."),
  h4("Stability"),
  p("Captures within-individual variability in depressive symptoms over time and compare how this varies by different forms of interventions or combinations of interventions.")
)
)

format_page <- tabPanel(
  title = "Data Preparation",
  fluidPage(
    trajMods:::wide2longUI("wide2long")
  )
)


overview_page <-   tabPanel(
  title = "Data Exploration",
  fluidPage(
    tabsetPanel(
      tabPanel("Instructions",
               tagList(
                 h2("Initial data exploration"),
                 p("Either upload a long format dataframe (csv or tsv) or use the data frame you formatted on the previous page. Then select the columns you wish to use as variables in your model. Inspect the descriptive statistics of your trajectory variable at each time point. Select the model type (eg. linear or a polynomial model) and view the plot of the mean trajectory against these models.")
               )),
      tabPanel("Output",
    sidebarLayout(
      sidebarPanel(
        trajMods:::selectDataUI("select"),
        trajMods:::varsSelectUI("varsSelect")),
      mainPanel(
        tabsetPanel(
          tabPanel("Descriptive Statistics",
        trajMods:::modelRunUI("modelRun")),
        tabPanel("Model Results",
                 trajMods:::modelResultsUI("modelResults")),
        tabPanel("Plot",
        trajMods:::modelPlotUI("modelPlot")))
      ))
        )
    )
  )
)

intervention_page <- tabPanel(
  title = "Analysis",
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        trajMods:::modelCondUI("modelCond")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Instructions",
            tagList(
              h4("Split by variable analysis"),
              p("Using the model made on the previous page explore some factors which may influence the trajectory. Select from a list of variables (ie. column names) of the dataset to split the trajectory by. There is also the option to include covariates in the model.")
            )
          ),
          tabPanel(
          "Output",
          trajMods:::modelCondPlotUI("modelCondPlot")
          )
      )
    )
    )
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
               trajMods:::singleTrajUI("singeTraj")
      )
    )
  )
)


ui <- navbarPage(
  title = "Multi-Level Modelling",
  theme = shinytheme('united'),
  # tags$style(type="text/css",
  #            ".shiny-output-error { visibility: hidden; }",
  #            ".shiny-output-error:before { visibility: hidden; }"
  # ),
  welcome_page,
  format_page,
  overview_page,
  intervention_page,
  singeTraj_page
)

# Main server
server <- function(input, output, session) {
  wide2longServer <- trajMods:::wide2longServer("wide2long")
  selectedDataServer <- trajMods:::selectDataServer("select", dataFormatted=wide2longServer)
  varsSelectServer <- trajMods:::varsSelectServer("varsSelect", varsSelectData=selectedDataServer)
  modelRunServer <- trajMods:::modelRunServer("modelRun",
                                              modelData = selectedDataServer,
                                              formCode = varsSelectServer$modelForm,
                                              traj = varsSelectServer$traj,
                                              age = varsSelectServer$age,
                                              timePoint = varsSelectServer$timePoint)
  modelResultsServer <- trajMods:::modelResultsServer("modelResults",
                                                      modelFit = modelRunServer)
  modelPlotServer <- trajMods:::modelPlotServer("modelPlot",
                                                modelData = selectedDataServer,
                                                modelFit = modelRunServer,
                                                SubjectID = varsSelectServer$ID,
                                                traj = varsSelectServer$traj,
                                                age = varsSelectServer$age,
                                                timePoint = varsSelectServer$timePoint)
  modelCondServer <- trajMods:::modelCondServer("modelCond",
                                                modelData = selectedDataServer)
  modelCondPlotServer <- trajMods:::modelCondPlotServer("modelCondPlot",
                                                modelData = selectedDataServer,
                                                formCode = varsSelectServer$modelForm,
                                                dfPlot = modelPlotServer,
                                                traj = varsSelectServer$traj,
                                                age = varsSelectServer$age,
                                                timePoint = varsSelectServer$timePoint,
                                                conditionVar = modelCondServer$condition,
                                                modelType = varsSelectServer$modelType
                                                # covariates = modelCondServer$covariates,
                                                # covarsLogical = modelCondServer$covarsLogical
                                                )
  singleTrajServer <- trajMods:::singleTrajServer("singeTraj",
                                                  ID = varsSelectServer$ID,
                                                  modelData = selectedDataServer,
                                                  modelFit = modelRunServer)
}

# Run the application
shinyApp(ui = ui, server = server)




