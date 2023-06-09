#' AUC for the data exploration page
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
datExAUCServer <- function(id,
                           modelDataEdit,
                           modelFit,
                           modelType,
                           traj) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

###############################################################
# --- AUC tab ------
###############################################################

# ------------------------------------------
# Overview of what AUC does text
output$AUCoverview <- renderText({
  paste0("The area under curve (AUC) represents the proportion of time with phenotype (", traj(), ").")
})

# ------------------------------------------
# Make age slider for user
output$AUCagesUI <- renderUI({
  ageOrig <- modelDataEdit() %>%
    pull(age_original)
  ageOrig <- ageOrig[!is.na(ageOrig)]

  sliderInput(ns("AUCages"),
              "Select the age range to calculate AUC for:",
              min = round(min(ageOrig, na.rm =T)),
              max = round(max(ageOrig, na.rm =T)),
              value = c(round(min(ageOrig, na.rm =T)),round(max(ageOrig, na.rm =T)))
  )
})

# ------------------------------------------
# Calculate the AUC for the ages the user has chosen for the chosen input$condition levels
AUC <- reactive({
  coef <- summary(modelFit())$coefficients
  age1 <- input$AUCages[1]
  age2 <- input$AUCages[2]

  AUC <-
    if(modelType() == "Linear"){
      ((age2*coef[1,1]) + (coef[2,1]*age2^2/2)) - ((age1*coef[1,1]) + (coef[2,1]*age1^2/2)) %>%
        round(2)
    } else if(modelType() == "Quadratic"){
      ((age2*coef[1,1]) + (coef[2,1]*age2^2/2) + (coef[3,1]*age2^3/3)) - ((age1*coef[1,1]) + (coef[2,1]*age1^2/2) + (coef[3,1]*age1^3/3)) %>%
        round(2)
    } else if(modelType() == "Cubic"){
      ((age2*coef[1,1]) + (coef[2,1]*age2^2/2) + (coef[3,1]*age2^3/3) + (coef[4,1]*age2^4/4)) - ((age1*coef[1,1]) + (coef[2,1]*age1^2/2) + (coef[3,1]*age1^3/3) + (coef[4,1]*age1^4/4))  %>%
        round(2)
    } else if(modelType() == "Quartic"){
      ((age2*coef[1,1]) + (coef[2,1]*age2^2/2) + (coef[3,1]*age2^3/3) + (coef[4,1]*age2^4/4) + (coef[5,1]*age2^5/5)) - ((age1*coef[1,1]) + (coef[2,1]*age1^2/2) + (coef[3,1]*age1^3/3) + (coef[4,1]*age1^4/4) +  (coef[5,1]*age1^5/5)) %>%
        round(2)
    }
    return(AUC)
})

# ------------------------------------------
# Plot AUC
plotAUC <- eventReactive(input$AUCages, {

    req(AUC())

    ggplot() +
      theme_light()+
      geom_ribbon(data = modelDataEdit(),
                  aes(x = age_original, ymax = pred, ymin = 0),
                  alpha = 0.1, show.legend = FALSE, fill = "deepskyblue") +
      geom_line(data = modelDataEdit(), aes(x= age_original ,  y = pred ) , na.rm=T)+
      coord_cartesian(xlim = c(input$AUCages[1], input$AUCages[2])) +
      scale_colour_discrete(na.translate = F) +
      theme(legend.text = element_text(color = "black", size = 10),
            text = element_text(size = 14)) +
      ylab(paste0("Score (", traj(), ")")) +
      xlab("Age") +
      scale_x_continuous(breaks = seq(round(min(modelDataEdit()$age_original, na.rm =T)), round(max(modelDataEdit()$age_original, na.rm =T)), by = 1),
                         expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0))

})

output$AUCplot <- renderPlot({
  plotAUC()
})


tableAUC <- eventReactive(input$AUCages, {

    req(AUC())
    df <- t(
      data.frame(paste0(input$AUCages[1], " - ", input$AUCages[2]),
                 round(AUC(), 2))
    )
    rowname <- paste0("AUC (", traj(), ")")
    rownames(df) <- c("Age Range", rowname)
    df

})


output$AUCtable <- renderTable({
  tableAUC()
}, colnames = FALSE, rownames = TRUE)


return(list(
  modelDataEdit = modelDataEdit
))

}
)
}
