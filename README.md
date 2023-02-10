# Tool to Implement Developmental Analyses of Longitudinal Data (`TIDAL`)

<p align="center">
<img width="600" alt="Screenshot 2023-02-03 at 13 00 15" src="https://user-images.githubusercontent.com/24313187/216609683-bac9e15c-6860-4441-a9ae-936f81940b1b.png">
</p>

<!-- badges: start -->
![my badge](https://badgen.net/badge/Status/In%20Development/orange)
<!-- badges: end -->

## Installation and useage

### Online: https://tidal.shinyapps.io/tidalapp/ 

To use this tool online please do not upload any sensitive data. Only use the [synthetic datasets](data/), described below.

### Locally

Please install the R package and launch the Shiny app locally if you want to upload sensitive data:

```{r eval=FALSE}
# install.packages("remotes")
remotes::install_github("AmeliaES/TIDAL")
library("TIDAL")
# Launch the R Shiny app
launchTIDAL()
```

## Synthetic datasets 
To save the dataset click on the link below (right click to open in new tab) and then click `File` -> `Save Page As...`.

* [ad_use_dep_simulated.csv](https://raw.githubusercontent.com/AmeliaES/TIDAL/main/data/ad_use_dep_simulated.csv)
  * Anti-depressant use at 7 time points.
* [emot_reg_emot_simulated.csv](https://raw.githubusercontent.com/AmeliaES/TIDAL/main/data/emot_reg_emot_simulated.csv)
  * Emotional regulation scores at 5 time points.
* [fp_use_dep_simulated.csv](https://raw.githubusercontent.com/AmeliaES/TIDAL/main/data/fp_use_dep_simulated.csv)
  * Financial problems and GAD-7 scores at 6 time points.
* [sleep_use_dep_simulated.csv](https://raw.githubusercontent.com/AmeliaES/TIDAL/main/data/sleep_use_dep_simulated.csv)
  * Sleep scores and emotional/MFQ scores at 4 time points.

## Main Features

### Overview

The aim is for this digital tool to facilitate trajectories work and remove barriers to implementing longitudinal research to researchers without specialist statistical backgrounds. The following pages guide trajectory modelling and capture clinically meaningful features from mental health trajectories for specific individuals and/or specific groups of people.

<p align="center">
  <img src="https://user-images.githubusercontent.com/24313187/216603041-f4bf851c-72d6-4cb3-b9f7-415047e8abca.png" width="700">
</p>

### Data Preparation
This allows the user to upload a wide format of their longitudinal dataset. Select which columns measure time and the phenotype they want to model trajectories on. Converts the dataframe to long format. Allows the user to download the long format dataset.

<p align="center">
  <video src="https://user-images.githubusercontent.com/24313187/216603909-9868a4e4-35ed-4b09-86ef-38a126a3d6b1.mov" controls="controls" muted="muted" class="d-block rounded-bottom-2 width-fit" style="max-height:640px;">
</video>
</p>

### Data Exploration
This is the first stage of the trajectory modelling. Here the user either uploads a long format dataset or uses the dataset formatted on the previous page (Data Preparation). They specify the columns relatated to the variables to include in the model. There is a choice of model type and the user can see which model type looks like it best fits their data to explore further on the following pages.

<p align="center">
  <video src="https://user-images.githubusercontent.com/24313187/216604313-bc7f3643-35c3-408a-b85a-d302ec0b3e1e.mov" controls="controls" muted="muted" class="d-block rounded-bottom-2 width-fit" style="max-height:640px;">
</video>
</p>

### Group Interactions
Split the trajectories by categorical varaibles to examine the differences in trajectories.

<p align="center">
  <video src="https://user-images.githubusercontent.com/24313187/216604194-9a4f520d-1f62-4822-b463-5e2c04efef65.mov" controls="controls" muted="muted" class="d-block rounded-bottom-2 width-fit" style="max-height:640px;">
</video>
</p>

### Individual Trajectories
View trajectories for specific individuals. Choose from a random sample, specific individuals of interest, individuals within a specific variable, eg. a random sample of females only.

<p align="center">
  <video src="https://user-images.githubusercontent.com/24313187/216604268-332cd5ff-e0fa-4a05-a3b5-f4a993ea13d0.mov" controls="controls" muted="muted" class="d-block rounded-bottom-2 width-fit" style="max-height:640px;">
</video>
</p>


## Other features in development
* **Points of acceleration**
  * Examine timing of peak velocity of trajectories. This feature highlights a critical period at which further support or interventions could be introduced to dramatically shift an individual’s illness trajectory.
* **Stability**
  * Captures within-individual variability in depressive symptoms over time and compare how this varies by different forms of interventions or combinations of interventions. 
* Allow users to input an x-axis value (eg. age) and recieve y-axis value (eg. depression score), for mean values from a user specified model.
* Allow users to download tables and plots (also to edit colours in the plots)
* Return an R script at the end of analysis with the code ran to generate tables and plots downloaded.
  
