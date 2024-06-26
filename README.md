# Tool to Implement Developmental Analyses of Longitudinal Data

<p align="center">
<img width="600" alt="Two-tone blue wave TIDAL logo. Below the wave the text reads Tool to Implement Developmental Analyses of Longitudinal data" src="https://user-images.githubusercontent.com/24313187/216609683-bac9e15c-6860-4441-a9ae-936f81940b1b.png">
</p>

<!-- badges: start -->
![my badge](https://badgen.net/badge/Status/In%20Development/orange)
<!-- badges: end -->


## Installation and usage

### Locally

Please install the R package and launch the Shiny app locally if you want to upload sensitive data. If using R Studio it's recommended to restart your R session before installing.

```{r eval=FALSE}
# install.packages("remotes")
remotes::install_github("TIDAL-modelling/TIDAL")
# Note if prompted to update packages you can select option 3/None.
# Updating all packages (option 1) might take a while to run.
library("TIDAL")
# Launch the R Shiny app
launchTIDAL()
# To get documentation for launchTIDAL()
?launchTIDAL
```

### Online

https://tidal.shinyapps.io/tidalapp/

To use this tool online please do not upload any sensitive data. Only use the [synthetic datasets](data/), described below.

## Synthetic datasets
  * [Documentation and description of simulated data](https://github.com/TIDAL-modelling/TIDAL/blob/main/data/README.md)
  * To save the dataset click on the link below (right click to open in new tab) and then click `File` -> `Save Page As...` and save as a .csv file.
       * [emot_reg_emot_simulated.csv](https://raw.githubusercontent.com/TIDAL-modelling/TIDAL/main/data/emot_reg_emot_simulated.csv)
       * [ad_use_dep_simulated.csv](https://raw.githubusercontent.com/TIDAL-modelling/TIDAL/main/data/ad_use_dep_simulated.csv)
       * [fp_anx_simulated.csv](https://raw.githubusercontent.com/TIDAL-modelling/TIDAL/main/data/fp_anx_simulated.csv)
       * [sleep_probs_mh_simulated.csv](https://raw.githubusercontent.com/TIDAL-modelling/TIDAL/main/data/sleep_probs_mh_simulated.csv)
       * [height_simulated.csv](https://raw.githubusercontent.com/TIDAL-modelling/TIDAL/main/data/height_simulated.csv)

## Main Features

![](_includes/tabs.png)

### Overview

The aim is for this digital tool to facilitate trajectories work and remove barriers to implementing longitudinal research to researchers without specialist statistical backgrounds. The following pages guide trajectory modelling and capture clinically meaningful features from mental health trajectories for specific individuals and/or specific groups of people. A more detailed description of each page is provided below with links to the full [walkthrough video](https://www.youtube.com/watch?v=aWteXAWPBik). An instruction manual is provided [here](https://github.com/TIDAL-modelling/TIDAL/blob/main/Documentation/Instructions.pdf).

<details>
<summary><h2>Data Preparation</h2></summary>

Many longitudinal datasets will be written in "wide" format, with each time point's data stored in a separate column. To analyse this data, it must first be converted into "long" format, with one column containing the time point names and one column containing the measurements.

This page allows the user to upload a wide format dataset and convert it into long format. The user selects which columns measure time and the phenotype they want to model trajectories on. There is also an option to impute mean age for missing age data in this step. Once converted into long format, there is the option to download the long version dataset as a .csv file.

To avoid errors when running, spaces in column names are replaced with underscores "_". If column names contain "(", ")" or "*" these are replaced with empty strings "". If you wish to change this, it is advised to edit your column names prior to uploading your data.

[![TIDAL Data Preparation walkthrough](/_includes/data-prep.png)](https://youtu.be/aWteXAWPBik?t=133)
</details>

<details>
<summary><h2>Data Exploration</h2></summary>

This is the first stage of trajectory modelling. Here the user can either upload a long format dataset or use the dataset formatted on the previous page (Data Preparation). They then select the desired outcome variable to model the trajectory on and a time variable, usually age. Note that the variable which the trajectory is modelled on must be measured on a continuous scale.

There is an option at this stage to add extra covariates or confounders to the model. These can be categorical or continuous and multiple variables can be selected from the drop-down menu. The user can run a linear or non-linear (quadratic, cubic, quartic) model according to which best fits their data.

Once a model has been fitted, TIDAL will output the fixed and random effects in the "Model Results" tab. The user can also examine predicted scores for specific ages in the "Scores At Ages" tab and extract Area Under the Curve metrics in the "Area Under Curve" tab. Finally, the user can download a PDF report from the "Download Results" tab.

[![TIDAL Data Exploration walk through](/_includes/data-explore.png)](https://youtu.be/aWteXAWPBik?t=239)

</details>

<details>
<summary><h2>Interaction Variable</h2></summary>

On this page, the user can examine group differences or the effect of an interacting variable. The user can select a categorical or continuous variable from within the data and fit an updated model with the included interaction variable. The model type is inherited from the "Data Exploration" page alongside any covariates or confounders. Note that the interaction variable should not be included in the initial model if you wish to investigate it on this page. 

The output is similar to that from the "Data Exploration" page. In future we will be implementing features allowing the user to change factor level names and select the reference level to be used for categorical variables.

[![TIDAL Interaction Variable walkthrough](/_includes/interaction-variable.png)](https://youtu.be/aWteXAWPBik?t=475)

</details>

<details>
<summary><h2>Individual trajectories</h2></summary>

This page allows the user to view trajectories for specific individuals. The user can input specific IDs of interest, select a random sample of individuals to plot, or select a random sample from a specific category - e.g. female only. This allows comparison between individual trajectories and the group trajectories and may be useful for identifying whether a particular individual could be "on track" or not.

[![TIDAL Individual Trajectories walkthrough](/_includes/individual-trajectories.png)](https://youtu.be/aWteXAWPBik?t=580)

</details>

<details>
<summary><h2>Other features in development</h2></summary>
 
* **Points of acceleration**
  * Examine timing of peak velocity of trajectories. This feature highlights a critical period at which further support or interventions could be introduced to dramatically shift an individual’s illness trajectory.
 
* **Stability**
  * Captures within-individual variability in depressive symptoms over time and compares how this varies by different forms of interventions or combinations of interventions. 

</details>

## Links

[Instruction Manual](https://github.com/TIDAL-modelling/TIDAL/blob/main/Documentation/Instructions.pdf)

[Examples of the code running in the background](https://github.com/TIDAL-modelling/TIDAL/tree/main/Examples)

[FAQs and troubleshooting](https://github.com/TIDAL-modelling/TIDAL/blob/main/Documentation/FAQs.md)

[Statistics guide](https://github.com/TIDAL-modelling/TIDAL/blob/main/Documentation/StatsGuide.md)

## Bugs and Feature Requests
Please raise an issue if you find a bug or have a feature request using the templates provided.

## Contact
Please contact us at TIDAL@ed.ac.uk

## Authors, contributors and funders
* Amelia J. Edmondson-Stait<sup>1</sup>, Ellen J. Thompson<sup>2</sup>, Eileen Y. Xu<sup>1</sup>, Richard M. A. Parker <sup>3</sup>, Ahmed Elhakeem<sup>3</sup>,  Liana Romaniuk<sup>1</sup>, Iona Beange<sup>1</sup>, Rebecca M. Pearson<sup>3,4</sup>, Andrew M. McIntosh<sup>1</sup>, Thalia C. Eley<sup>2</sup>, Kate Tilling<sup>3</sup>, Heather C. Whalley<sup>1,5</sup>, Alex S. F. Kwong<sup>1</sup>
* Affiliations:
     <ol type="1">
        <li>Division of Psychiatry, University of Edinburgh</li>
        <li>Social Genetic and Developmental Psychiatry Centre, King’s College London</li>
        <li>Medical Research Council Integrative Epidemiology Unit, University of Bristol</li>
        <li>Department of Psychology, Manchester Metropolitan University</li>
        <li>Generation Scotland: Scottish Family Health Study (GS:SFHS), University of Edinburgh</li>
     </ol>
* We would also like to acknowledge cohort participants and those who have given feedback to our tool, including clinicians, teachers, researchers and our young people advisory group.
* This project is funded by The Wellcome Trust and Social Finance, Grant Ref: 226686/Z/22/Z.
<p align="left">
<img width="100" alt="Wellcome Trust Logo" src="inst/TIDALapp/www/wellcome-logo-black.jpg">
</p>

## Disclaimer
This package is provided solely for educational and informational purposes. Users understand and agree that any data uploaded and utilised with this package is done at their own risk. Users are solely responsible for the accuracy, legality, and ethical considerations of the data they upload. Additionally, users are responsible for the interpretation of results obtained through the use of this package. The creators and maintainers of this package shall not be held liable for any consequences arising from the use, interpretation, or implications of the package or the data uploaded.

