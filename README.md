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


- edit analysis page to split trajectory by a condition, eg. sex
  - add documention/instructions
- add a page that shows individual level trajectories (struggling with the R code to get these trajectories from lme4)
  - add documention/instructions
- add a page to show peak velocity (when code available)
  - add documention/instructions
- add a synthetic dataset to package and Shiny, so people can use this dataset to explore the tool if they don't have their own
- make gifs/videos to demostrate the main features of the app


