% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/launchTIDAL.R
\name{launchTIDAL}
\alias{launchTIDAL}
\title{Run Shiny app from local R environment}
\usage{
launchTIDAL(display = "default")
}
\arguments{
\item{display}{Character vector. Can be either "default" or "browser". Default is "default".
"default" opens the Shiny app in a pop-out window in RStudio, while "browser" opens it in a web browser.}
}
\description{
Opens an interactive Shiny GUI.
}
\examples{
\dontrun{# Launch Shiny app with default display
launchTIDAL()

# Launch Shiny app with browser display
launchTIDAL(display = "browser")
}
}
