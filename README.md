## `trajMods` package

### R package and Shiny app for modelling longitudinal trajectory models

<!-- badges: start -->
![my badge](https://badgen.net/badge/Status/In%20Development/orange)
<!-- badges: end -->

### Installation and useage

```{r eval=FALSE}
# install.packages("remotes")
remotes::install_github("AmeliaES/trajMods")
library("trajMods")
# Launch the R Shiny app
runShinyTraj()
```

### Developing to do:

- edit data exploration page
  - make pretty table of random and fixed effects
  - tab the output into descriptive stats, model summary and plot
- edit analysis page to split trajectory by a condition, eg. sex
  - add UI input for covariates
  - UI for categorical value to split on
  - output of final model that is run
  - output of table of model rand and fix effects
  - output of plot
  - add documention/instructions
- add a page that shows individual level trajectories (struggling with the R code to get these trajectories from lme4)
  - add documention/instructions
- add a page to show peak velocity (when code available)
  - add documention/instructions
- add a synthetic dataset to package and Shiny, so people can use this dataset to explore the tool if they don't have their own
- make gifs/videos to demostrate the main features of the app
- add a feature that returns an R script at the end of analysis with the code for the analysis that has been conducted.


