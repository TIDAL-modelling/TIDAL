# Shiny Modules for the Main Page

library(broom.mixed)
library(magrittr)
library(lme4)
library(dplyr)
library(ggplot2)
library(data.table)
library(shinyjs)
library(tidyr)



# -----------------------------------
#### wide2long
wide2longUI <- function(id, label = "wide2long") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  useShinyjs()

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
            tags$li('Now you can see a preview of the newly formatted long dataframe by clicking on the "Output" tab. You have the option to download it (in .csv format) and also use for analysis on the subsequent pages.')
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

wide2longServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      info <- eventReactive(input$upload, {
        output$moreControls <- renderUI(
          tagList(
            selectInput(ns("ageCols"), "Select columns for age at each time point:", choices = NULL, multiple = TRUE),
            selectInput(ns("depCols"), "Select columns for the phenotype (eg. depression) at each time point:", choices = NULL, multiple = TRUE),
            textInput(ns("age"), "Name of new column for age:", value = "age"),
            textInput(ns("occ"), "Name of new column for time point:", value = "occ"),
            textInput(ns("dep"), "Name of new column for phenotype:", value = "dep"),
            textInput(ns("dep_cat"), "Name of new column for depression time point:", value = "dep_cat"),
            downloadButton(ns("downloadData"), "Download .csv")
          )
        )

        inFile <- input$upload

        req(inFile)

        f <- fread(inFile$datapath)
        vars <- colnames(f)
        # Update select input immediately after uploading file.
        updateSelectInput(session, "ageCols","Select columns for age at each time point:", choices = vars)
        updateSelectInput(session, "depCols","Select columns for the phenotype (eg. depression) at each time point:", choices = vars)

        output$warningMsgEmpty <- renderText({
          ifelse(is.null(input$ageCols) | is.null(input$depCols), '<b style="color:black">Select columns for age and depression, in chronological order.</b>', '')
        })

        f

      })

      dataLong <- reactive({
        validate(
          need(length(input$ageCols) == length(input$depCols), "")
        )
        dataDep <- info() %>%
          gather(!!input$dep_cat, !!input$dep, all_of(input$depCols))

        dataLong <- info() %>%
          gather(!!input$occ, !!input$age, all_of(input$ageCols))

        dataLong[,input$dep_cat] <- dataDep[,input$dep_cat]
        dataLong[,input$dep] <- dataDep[,input$dep]

        dataLong <- dataLong %>%
          relocate(c(!!input$occ, !!input$age, !!input$dep_cat, !!input$dep), .after = 1)

        dataLong
      })

      output$preview <- renderTable({
        dataLong() %>%
          select(1:5) %>%
          head()
      })

      output$downloadData <- downloadHandler(
        filename = function(){
          paste0(Sys.time(), 'LongFormat.csv')
        },
        content = function(file){
          write.csv(dataLong(), file)
        }
      )

      output$warningMsgLen <- renderText({
        ifelse(length(input$ageCols) != length(input$depCols), '<b style="color:red">Select same number of columns for age and depression.</b>', '')
      })

      return(dataLong)
    }
  )
}

# ----------------------------------
#### selectData ####################
# Selecting and reading in the dataset (default is ALSPAC from a local path)

selectDataUI <- function(id, label = "data") {
  ns <- NS(id)
  tagList(
    selectInput(ns("select"), "Select a dataset:", c("Upload a long format dataset", "Data formatted on previous page") ),
    uiOutput(ns("uploadFile"))
  )
}

selectDataServer <- function(id, dataFormatted) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ns <- NS(id)

      data <- reactive({
        req(input$select)
        if (input$select == "Upload a long format dataset"){
          output$uploadFile <- renderUI({
            fileInput(ns("Upload a long format dataset"), NULL)
          })
          req(input$`Upload a long format dataset`)
          # data <- data.frame("test" = c(1,2,3))
          data <- fread(input$`Upload a long format dataset`$datapath)
        }
        else {
          output$uploadFile <- renderUI({
          })
          data <- dataFormatted()
        }
      })

      return(data)
    }
  )
}


# ----------------------------------
#### varsSelect ####################
# Choosing variables from a drop down menu of the column names loaded in the selectData module

varsSelectUI <- function(id, label = "Variables Selected") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  tagList(
  selectInput(ns("ID"), "Participant ID variable:", choices = c()),
  selectInput(ns("traj"), "Variable to model trajectory on, eg. depression scores:", choices =  c()),
  selectInput(ns("age"), "Variable for age:", choices =  c()),
  # selectInput(ns("covars"), "Select any covariates to use in the model", choices =  c(), multiple = TRUE),
  selectInput(ns("modelType"), "Model Type:", choices = c("Linear", "Quadratic", "Cubic", "Quartic"))
  )
}

varsSelectServer <- function(id, varsSelectData) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)

      colVarUpdate <- function(colVar, i){
        observeEvent(varsSelectData(), {
          updateSelectInput(
            session,
            colVar,
            choices = names(varsSelectData()),
            selected = names(varsSelectData()[i])
          )
        })
      }

      varNames <- list("ID", "traj", "age")
      varIndex <- list(NULL, NULL, NULL)
      purrr::map2(varNames, varIndex, \(varNames, varIndex) colVarUpdate(varNames, varIndex))

      # special case for covariates that can have nothing selected (figure out a better way for this)
      # observeEvent(varsSelectData(), {
      #   updateSelectInput(
      #     session,
      #     "covars",
      #     choices = c(" ", names(varsSelectData())),
      #     selected = " "
      #   )
      # })


      modelForm <- reactive({
        if(input$modelType == "Linear"){
          paste0(input$traj," ~ ", input$age, " + ", "(", input$age, "|" , input$ID, ")")
        } else if(input$modelType == "Quadratic"){
          paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2) + (", input$age, "|" , input$ID, ") + (I(",input$age, "^2)|" , input$ID, ")" )
        } else if(input$modelType == "Cubic"){
          paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2)", " + I(", input$age   ,"^3)" ," + (", input$age, "|" , input$ID, ") + (I(",input$age, "^2)|" , input$ID, ")")
        } else if(input$modelType == "Quartic"){
          paste0(input$traj," ~ ", input$age, " + I(", input$age   ,"^2)", " + I(", input$age   ,"^3)" , " + I(", input$age   ,"^4)" ," + (", input$age, "|" , input$ID, ") + (I(",input$age, "^2)|" , input$ID, ")")
        }
      })


    return(modelForm)
    }
  )
}

# -----------------------------------
#### modelRun ##########################

modelRunUI <- function(id, label = "Model") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  tagList(
  htmlOutput(ns("formulaText")),
  br(),
  tableOutput(ns("desc")),
  tableOutput(ns("modelStatsFixed")),
  tableOutput(ns("modelStatsRandom"))
  )
}

modelRunServer <- function(id, modelData, formCode) {
  moduleServer(
    id,
    function(input, output, session) {

    fit <- reactive({
      lmer(formula = formCode(), REML=F , data = modelData())
    })


    output$formulaText <- renderText({
      if(nchar(formCode()) > 20 ){            # not the best/hacky way to make sure the formula doesn't show straight away
        paste0("<b>Model Formula:</b> ", formCode())
      }else{
        "<b>Model Formula:</b>"
      }
    })

    # output$desc <- renderTable(
    #
    # )

    output$modelStatsFixed <- renderTable(
      tidy(fit(), "fixed")
    )

    output$modelStatsRandom <- renderTable(
      print(VarCorr(fit()), digits = 2, comp=c("Variance")) %>%
        as.data.frame() %>%
        set_rownames(NULL)
    )
    return(fit)
    }
  )
}

# -----------------------------------
#### modelPlot
modelPlotUI <- function(id, label = "Model Plot") {
  ns <- NS(id)

  tagList(
  plotOutput(ns("mainPlot"))
  )
}

modelPlotServer <- function(id, modelData, modelFit) {
  moduleServer(
    id,
    function(input, output, session) {

      # Get the mean and sd for depression scores at each time point
      #################
      ### THE NAMES IN THE FOLLOWING DATAFRAME MIGHT BE DIFFERENT AND THEN IT WILL THROW AN ERROR!!!
      #################
      df.plot <- reactive({
        m.age = aggregate( age ~ occ, modelData(), mean )
        m.dep = aggregate( dep ~ occ, modelData(), mean )
        sd.dep = aggregate( dep ~ occ, modelData(), sd )
        df.plot = list(m.age, m.dep, sd.dep) %>% reduce(left_join, by = "occ") %>%
          dplyr::rename("Age" = age, "Dep" = dep.x, "sd" = dep.y)

        # sample size for each time point
        n <- sapply(1:nrow(df.plot), function(i) {
          dep <-  modelData() %>%
            filter(occ == df.plot$occ[i]) %>%
            pull(dep)
          length(which(!is.na(dep)))
        })
        df.plot$n <- n

        # Calculate confidence intervals for mean trajectories
        df.plot$upper <- NA
        df.plot$lower <- NA
        for(i in 1:nrow(df.plot)){
          error <- qnorm(0.975)*df.plot$sd[i]/sqrt(df.plot$n[i])
          df.plot$upper[i] <- df.plot$Dep[i] + error
          df.plot$lower[i] <- df.plot$Dep[i] - error
        }
        return(df.plot)
      })

      modelDataEdit <- reactive({
        modelData() %>%
          mutate(pred = predict(modelFit(), ., re.form = NA))
      })

      output$mainPlot <- renderPlot(
        ggplot(df.plot(),aes(x=Age, y=Dep)) +
          theme_light()+
          geom_point()+
          geom_line() +
          geom_errorbar(aes(ymin = lower, ymax = upper)) +
          geom_line(data = modelDataEdit(), aes(x=age,  y = pred), na.rm=T)
      )
      return(df.plot)
    }
  )
}

# -----------------------------------
#### modelCond
modelCondUI <- function(id, label = "Model Condition Run") {
  ns <- NS(id)

  tagList(
    selectInput(ns("condition"), "Select the condition to split trajectory on.", choices = c("CRP_quartile", "IL6_quartile")),
    plotOutput(ns("modelCondPlot")))
  }

modelCondServer <- function(id, modelData, formCode, dfPlot) {
  moduleServer(
    id,
    function(input, output, session) {
      fit <- reactive({
        lmer(formula = paste0(formCode(), "+ ", input$condition), REML=F , data = modelData())
      })

    modelDataEdit <- reactive({
      modelDataEdit <- modelData() %>%
        mutate(pred = predict(fit(), ., re.form = NA))
      modelDataEdit$Group_Level <- as.factor(modelDataEdit[,input$condition])
      return(modelDataEdit)
    })

    cond <- reactive({
      input$condition
    })

    output$modelCondPlot <- renderPlot({
        ggplot(data = dfPlot(),aes(x=Age, y=Dep)) +
         theme_light()+
         geom_point()+
         geom_line() +
        geom_errorbar(aes(ymin = lower, ymax = upper)) +
        geom_line(data = modelDataEdit(), aes(x=age,  y = pred, color = Group_Level ) , na.rm=T)+
        scale_colour_discrete(na.translate = F)
    })

    }
  )
}



# -----------------------------------
#### Template
# templateUI <- function(id, label = "template") {
#   ns <- NS(id)
#
#   tagList(
#
#   )
# }
#
# templateServer <- function(id) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#
#     }
#   )
# }
