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

- add some documentation and user instructions to page 1 and 2.
- check my code against what Ellen has done, see if I've missed anything.
- edit analysis page to split trajectory by a condition, eg. sex
- add a page that shows individual level trajectories (struggling with the R code to get these trajectories from lme4)
  - this page includes imputing missing age with the mean for each time point. Check if we need this imputed previously for the data exploration page.
- add a page to show peak velocity (when code available)
- change module script into a series of internal functions https://blog.r-hub.io/2019/12/12/internal-functions/

