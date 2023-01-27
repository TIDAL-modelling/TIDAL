#' Model plot - data exploration page
#'
#' @import broom.mixed
#' @import lme4
#' @import dplyr
#' @import ggplot2
#' @import data.table
#' @import shinyjs
#' @import tidyr
#' @import purrr
#'
#' @noRd
#' @keywords internal
#' @export
modelPlotServer <- function(id,
                            modelData,
                            modelFit,
                            SubjectID,
                            traj,
                            timePoint,
                            age
                            ) {
  moduleServer(
    id,
    function(input, output, session) {

      # Get the mean and sd for depression scores at each time point
      #################
      ### THE NAMES IN THE FOLLOWING DATAFRAME MIGHT BE DIFFERENT AND THEN IT WILL THROW AN ERROR!!!
      #################
      df.plot <- reactive({
        m.age = aggregate( age() ~ timePoint(), modelData(), mean )
        m.dep = aggregate( traj() ~ timePoint(), modelData(), mean )
        sd.dep = aggregate( traj() ~ timePoint(), modelData(), sd )
        df.plot = list(m.age, m.dep, sd.dep) %>% purrr::reduce(left_join, by = "occ") %>%
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

