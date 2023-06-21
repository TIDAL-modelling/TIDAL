# TIDAL Troubleshooting and FAQs

If you run into any bugs or issues not listed below, please email TIDAL@ed.ac.uk

<details>
    <summary>Can I upload my own data to use with TIDAL?</summary>

If you are using the [online version](https://tidal.shinyapps.io/tidalapp/) then please do not upload *any sensitive data*. You can explore the tool online using the [synthetic data](/data/README.md).

You *must* install TIDAL on your computer if you wish to analyse sensitive data. To do this, you need to first install R and RStudio (though if you prefer to work in the R terminal, you could use a X window system to display the GUI instead). Then run the following code to install and launch TIDAL:

```{r eval=FALSE}
install.packages("remotes")
remotes::install_github("AmeliaES/TIDAL")
library("TIDAL")
# Launch the R Shiny app
launchTIDAL()
# To get documentation for launchTIDAL()
?launchTIDAL
```
</details>

<details>
    <summary>When I select columns from my uploaded data, they have strange names</summary>

It's likely that you have uploaded an .xlsx file or another unsupported file format. Please save your data as a Comma delimited *.csv file or a Tab delimited *.tsv or *.txt file and try again.
</details>


<details>
    <summary>The download results feature is not working</summary>

If you are a Mac user, when you click "download results" your results PDF will open in a pop-out window in your default PDF program. You need to save this report manually (CMD+S) or the file will be lost when closed.

If you are trying to download results and the resulting file is not a .pdf or the file name begins with something other than "Explore_Data", it is likely that you do not have LaTeX installed on your computer. LaTeX is a free software needed to make PDF files in R, and can be downloaded for Linux, MacOS and Windows from [The LaTeX Project](https://www.latex-project.org/get).
</details>


<details>
    <summary>My model isn't running</summary>

If you are *only* running a linear model, ensure that there are at least 3 time points in your data. If you wish to run quadratic, cubic and/or quartic models, ensure that there are at least 4 time points in your data. If you are uploading your own long format data, also check that column names do not have spaces in them, and that no column names are duplicated.
</details>

<details>
    <summary>I am having problems installing TIDAL</summary>

Check that your R version is up-to-date. TIDAL requires you to have R version 2.10 or newer. If you are a Mac user or installing TIDAL within the terminal/command line, you may need to install the compiler [CMake](https://cmake.org/) which should solve any issues with compiling R packages from source. 
</details>

<details>
    <summary> My column names have changed </summary>

    To avoid errors when running, spaces in column names are replaced with underscores "_". If column names contain "(", ")" or "*" these are replaced with empty strings "".
</details>