## `trajMods` package

### R package and Shiny app for modelling longitudinal trajectory models

<!-- badges: start -->
![my badge](https://badgen.net/badge/Status/In%20Development/orange)
<!-- badges: end -->

### Installation

```{r eval=FALSE}
# install.packages("remotes")
remotes::install_github("AmeliaES/trajMods")
```

### Usage

First, load the package:

```{r eval=FALSE}
library("trajMods")
```

Now run the function `runShinyTraj()` to start the R Shiny app.

### Developing to do:

- add some documentation and user instructions to page 1 and 2.
- edit analysis page to split trajectory by a condition, eg. sex
- add a page that shows individual level trajectories (struggling with the R code to get these trajectories from lme4)
- add a page to show peak velocity (when code available)

- add a module to run when closing the shiny app using `onStop` to remove the functions in modules.R:
```
# Remove functions
# rm(list = setdiff(ls(), c("modelCondServer", "modelCondUI", "modelPlotServer", "modelPlotUI",
#                           "modelRunServer", "modelRunUI", "selectDataServer", "selectDataUI",
#                           "varsSelectServer", "varsSelectUI", "wide2longServer", "wide2longUI")
#                   )
#    )
```

