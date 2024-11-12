# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest
# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    texlive \
    texlive-latex-extra \
    texlive-fonts-recommended \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev 
# install R packages required 
RUN R -e "install.packages(c('shiny', 'shinythemes', 'shinyjs', 'stringr', 'tidyr', 'tibble', 'rlang', 'dplyr', 'broom.mixed', 'lme4', 'data.table', 'ggplot2', 'shinyBS', 'tinytex', 'rmarkdown', 'kableExtra', 'shinycssloaders', 'multcomp', 'car'), repos='http://cran.rstudio.com/')"
# Install TIDAL
RUN R -e "install.packages('remotes', repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_github('TIDAL-modelling/TIDAL', upgrade = 'never')"
# copy the app to the image
COPY inst/TIDALapp/app.R /srv/shiny-server/
COPY R /srv/shiny-server/R
COPY data /srv/shiny-server/data
COPY inst/TIDALapp/www /srv/shiny-server/www  
# select port
EXPOSE 3838
# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server
# run app
CMD ["/usr/bin/shiny-server"]