#' Run model - data exploration page
#'
#' @import broom.mixed
#' @import lme4
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @import stringr
#'
#' @keywords internal
#' @name modelResultsServer
utils::globalVariables(c("upper", "statistic", "p.z"))
modelResultsServer <- function(id,
                           modelFit,
                           warningMsg,
                           modelData ,
                           modelType,
                           randomFX,
                           age,
                           traj,
                           covars
) {

  moduleServer(
    id,
    function(input, output, session) {

      # ------------------------------------------
      # paste the formula text
      modelFormRender <- reactive({
        if(!inherits(modelFit(), "try-error")){
        paste0("<b>Model Formula:</b> ",  gsub(".*formula = (.+) , data =.*", "\\1", summary(modelFit())$call)[2])
        }else{
          paste0("")
        }
      })

      output$formulaText <- renderText({
        modelFormRender()
      })
      # ------------------------------------------
      # Message that "The time variable `age` has been mean centred to `meanAge`, which is the mean age across all time assessments. This aids model convergence."
      statement <- reactive({
        req(modelData())
        ageVar <- age()
        ageMeanVal <- modelData() %>%
          pull(sym(age())) %>%
          mean(na.rm = T) %>%
          round(2)

        statement <- paste0('The time variable "',ageVar,'" has been mean centred to ',ageMeanVal,', which is the mean value across all time assessments. This aids model convergence.')
        return(statement)
      })

      output$ageMean <- renderText({
        if(!inherits(modelFit(), "try-error")){
        statement()
        }
      })

      # ------------------------------------------
      # Warning text
      output$warning <- renderText({
        warningMsg()
      })

      # ------------------------------------------
      # number of observations (measurements) and the number of groups (people)
      N <- reactive({
        paste0("The number of observations (measurements) is ",
               format(summary(modelFit())$devcomp$dims[[1]], big.mark=",", scientific=FALSE),
               " and the number of groups (people with at least one observation) is ",
               format(summary(modelFit())$ngrps[[1]], big.mark=",", scientific=FALSE) ,
               ".")
      })
      output$Ndims <- renderText(
        N()
      )

      # ------------------------------------------
      # model results
      fixedTab <- reactive({
        cbind(
          tidy(modelFit(), "fixed"),
          confint(modelFit(), "beta_", method = "Wald")) %>%
          mutate(p.z = 2 * (1 - pnorm(abs(statistic)))) %>%
          mutate(p.z = ifelse(p.z <= 0, "p < 0.001", round(p.z, 3)))
      })

      output$modelStatsFixed <- renderTable({
        if(!inherits(modelFit(), "try-error")){
        fixedTab()
        }
      }, digits = 3)

      # ------------------------------------------
      # Interpretation of fixed effects
      interpretation <- eventReactive(modelFit(), {
        req(modelData())

        # Intercept:
        intercept <- round(fixedTab()$estimate[1], 3)

        # Age variable name:
        ageVar <- age()

        # y-axis name:
        trajVar <- traj()

        # Mean age:
        ageMeanVal <- modelData() %>%
          pull(sym(age())) %>%
          mean(na.rm = T) %>%
          round(3)

        # Slope:
        slope <- round(fixedTab()$estimate[2], 3)

        if(modelType() %in% c("Quadratic", "Cubic", "Quartic") ){
          slope2 <- round(fixedTab()$estimate[3], 3)
          direction2 <- ifelse(slope2 >= 0 , "an increase", "a decrease")
        }
        if(modelType() %in% c("Cubic", "Quartic")){
          slope3 <- round(fixedTab()$estimate[4], 3)
          direction3 <- ifelse(slope3 >= 0 , "an increase", "a decrease")
        }
        if(modelType() == "Quartic"){
          slope4 <- round(fixedTab()$estimate[5], 3)
          direction4 <- ifelse(slope4 >= 0 , "an increase", "a decrease")
        }

        # Direction
        direction <- ifelse(slope >= 0 , "an increase", "a decrease")

        # Confounders:
        confounders <- paste0(covars(), collapse = ", ")

        # Confounder level names and estimates
        if(length(covars()) > 0){
        i <- which(str_detect(fixedTab()$term, paste0(covars(), collapse = "|")))
        confounderLevels <- paste0(fixedTab()$term[i] , collapse = ", ")
        confounderEst <- paste0(round(fixedTab()$estimate[i],3) , collapse = ", ")
        }

        # Model fit (deviance)
        modelDeviance <- round(deviance(modelFit()), 3)

        # Text:
        paste0(
          'The score at the intercept is ', intercept, '. The intercept here has been shifted to the mean age of all the assessments which is ', ageMeanVal, '. You could interpret this as the score at ', ageVar,' ', ageMeanVal,' is ', intercept, '. ', paste0(ifelse(length(covars()) > 0, paste0('(Please note: if you have adjusted for any confounders or covariates, this score reflects individuals with the lowest categorical covariate(s) and/or the average of the continuous covariate(s))'), '')), '<br/><br/> Every unit increase in ',ageVar,' is associated with ', direction ,' of ', trajVar ,' by ', abs(slope),'. ', ifelse(modelType() ==  "Quadratic", paste0('As you have specified a quadratic model, every unit increase in ', ageVar ,'^2 is also associated with ', direction2 ,' of ', trajVar,' by ',abs(slope2),'. However, it can be difficult to interpret these two estimates in isolation, so we would recommend exploring your trajectories with the "Plot" and "Scores At Ages" tabs for more information.'),paste0('')),ifelse(modelType() == "Cubic",paste0('As you have specified a cubic model, every unit increase in ',ageVar,'^2 is also associated with ', direction2 ,' of ', trajVar ,' by ', abs(slope2) ,'. Furthermore, every unit increase in ',ageVar,'^3 is also associated with ', direction3 ,' of ', trajVar,' by ',abs(slope3),'. However, it can be difficult to interpret these three estimates in isolation, so we would recommend exploring your trajectories with the "Plot" and "Scores At Ages" tabs for more information.'),paste0('')), ifelse(modelType() == "Quartic",paste0('As you have specified a quartic model, every unit increase in ',ageVar,'^2 is also associated with ', direction2 ,' of ', trajVar ,' by ', abs(slope2) ,'. Furthermore, every unit increase in ',ageVar,'^3 is also associated with ', direction3 ,' of ', trajVar,' by ',abs(slope3),' and every unit increase in ',ageVar,'^4 is associated with ', direction4 ,' in ',trajVar,' by ',abs(slope4), '. However, it can be difficult to interpret these four estimates in isolation, so we would recommend exploring your trajectories with the "Plot" and "Scores At Ages" tabs for more information.'),paste0('')),'<br/>
        <br/>',ifelse(length(covars()) > 0,paste0('These estimates are adjusted for the following confounders/covariates: ', confounders ,'. (Please note that continuous covariates are averaged across the values of that covariate within the sample. Categorical covariates are set to their lowest level by default.) <br/><br/>In addition, the following confounders/covariates: ', confounderLevels ,' are associated with an increase or decrease of ', trajVar ,' by ',confounderEst,' respectively.<br/><br/>Please note, this section does not estimate group specific trajectories. See the Interaction Variable tab for group specific interactions and trajectories.<br/><br/>'), paste0('')), 'The model fit (deviance) is ', modelDeviance, ', you can compare this value to other similar models to determine which model has a better fit.<br/><br/>')
      })

      output$interFixed <- renderText({
        if(!inherits(modelFit(), "try-error")){
          interpretation()
        }
      })

      # ------------------------------------------
      # Extract variance & correlation components for the random effects. In the
      # resulting data frame, vcov indicates the variances and covariances, and sdcor
      # indicates the SDs and correlations. "lower.tri" returns the estimates in the
      # order of the lower triangle of the variance-covariance matrix (can
      # alternatively request "cov.last" to return correlations / covariances last).

      randomTab <- reactive({
        randomDF <- as.data.frame(VarCorr(modelFit()),
                      order = "lower.tri") %>%
                      mutate(across(where(is.numeric), round, 3))
        colnames(randomDF) <- c("Level", "Variable1", "Variable2", "Variance/Covariance", "SD Variance/Covariance")
        randomDF
      })

      output$modelStatsRandom <- renderTable({
        if(!inherits(modelFit(), "try-error")){
        randomTab()
        }
      }, digits = 3)


      # ------------------------------------------
      # Interpretation of fixed effects
      interpretationRand <- eventReactive(modelFit(), {
        req(modelData())

        # age/time
        ageName <- age()

        # Intercept variance
        intVar <- randomTab()[1,4]

        # Covariance between intercept and age/time covariance
        intAgeVar <- randomTab()[2,4]

        if(modelType() ==  "Linear"){
        # age/time variance
        ageVar <- randomTab()[3,4]

        # residual variance
        resVar <- randomTab()[nrow(randomTab()),4]

        # Text:
          if(randomFX() == "No random slope"){
            paste0(
              'The intercept variance (how much variability there is between individuals for their intercepts) for your model is ', intVar ,'.<br/><br/>The residual variance (how much variability there is within individuals) from your model is ',resVar,'.<br/><br/>')
          }else if(randomFX() == "Linear"){
          paste0(
            'The intercept variance (how much variability there is between individuals for their intercepts) for your model is ', intVar ,'. The covariance between the intercept and ', ageName ,' is ', intAgeVar ,'. The ',ageName,' variance (how much variability there is between individuals for their ',ageName,') is ',ageVar,'.<br/><br/>The residual variance (how much variability there is within individuals) from your model is ',resVar,'.<br/><br/>')
          }
        }else if (modelType() %in%  c("Quadratic", "Cubic", "Quartic")){
          # age/time variance
          intAgeVar2 <- randomTab()[3,4]

          # age/time variance
          ageVar <- randomTab()[4,4]

          # age/time covariance
          ageAge2CoVar <- randomTab()[5,4]

          # age/time ^2 variance
          age2Var <- randomTab()[6,4]

          # residual variance
          resVar <- randomTab()[nrow(randomTab()),4]

          if(randomFX() == "No random slope"){
          paste0('The intercept variance how much variability there is between individuals for their intercepts) for your model is ', intVar,'.<br/><br/>The residual variance (how much variability there is within individuals) from your model is ',resVar,'.<br/><br/>')
          }else if(randomFX() == "Linear"){
            # age/time variance
            ageVar <- randomTab()[3,4]
            paste0('The intercept variance how much variability there is between individuals for their intercepts) for your model is ', intVar,'. The covariance between the intercept and ',ageName,' is ',intAgeVar,'.<br/><br/>The ',ageName,' variance (how much variability there is between individuals for their ',ageName,') is ',ageVar,'.<br/><br/>The residual variance (how much variability there is within individuals) from your model is ',resVar,'.<br/><br/>')
          }else if(randomFX() == "Linear and Quadratic"){
            # age/time variance
            ageVar <- randomTab()[4,4]
            paste0('The intercept variance how much variability there is between individuals for their intercepts) for your model is ', intVar,'. The covariance between the intercept and ',ageName,' is ',intAgeVar,'. The covariance between the intercept and ',ageName,'^2 is ',intAgeVar2,'.<br/><br/>The ',ageName,' variance (how much variability there is between individuals for their ',ageName,') is ',ageVar,'. The covariance between ',ageName,' and ',ageName,'^2 is ',ageAge2CoVar,'. The ',ageName,'^2 variance (how much variability there is between individuals for their ',ageName,'^2) is ',age2Var,'.<br/><br/>The residual variance (how much variability there is within individuals) from your model is ',resVar,'.<br/><br/>')
          }
        }
      })

      output$interRandom  <- renderText({
        if(!inherits(modelFit(), "try-error")){
          interpretationRand()
        }
      })


      return(
        list(
        statement = statement,
        modelFormRender = modelFormRender,
        fixedTab = fixedTab,
        interpretation = interpretation,
        randomTab = randomTab,
        interpretationRand = interpretationRand,
        N = N
        )
      )

    }
  )
}

