
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cetaceanbcg

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/FlukeAndFeather/cetaceanbcg/main?urlpath=rstudio)

This repository contains the data and code for our paper:

> Czapanskiy, M. F., Ponganis, P. J., Fahlbusch, J. A., and Goldbogen,
> J. A. (in prep). *An accelerometer-derived ballistocardiogram method
> for detecting heartrates in free-ranging marine mammals*.

### How to cite

Following publication, please cite this compendium as:

> Czapanskiy, M. F., Ponganis, P. J., Fahlbusch, J. A., and Goldbogen,
> J. A. (2021). *Compendium of R code and data for “An
> accelerometer-derived ballistocardiogram method for detecting
> heartrates in free-ranging marine mammals”*. Accessed 27 Oct 2021.

## Contents

The **analysis** directory contains:

-   [:file\_folder: paper](/analysis/paper): R Markdown source document
    for manuscript. Includes code to reproduce the figures and tables
    generated by the analysis. It also has a rendered version,
    `paper.docx`, suitable for reading (the code is replaced by figures
    and tables in this file)
-   [:file\_folder: data](/analysis/data): Data used in the analysis.
-   [:file\_folder: figures](/analysis/figures): Plots and other
    illustrations
-   [:file\_folder:
    supplementary-materials](/analysis/supplementary-materials):
    Supplementary materials including notes and other documents prepared
    and collected during the analysis.

## How to run in your broswer or download and run locally

This research compendium has been developed using the statistical
programming language R. To work with the compendium, you will need
installed on your computer the [R
software](https://cloud.r-project.org/) itself and optionally [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

You can download the compendium as a zip from from this URL:
[main.zip](/archive/main.zip). After unzipping: - open the `.Rproj` file
in RStudio - run `devtools::install()` to ensure you have the packages
this analysis depends on (also listed in the [DESCRIPTION](/DESCRIPTION)
file). - finally, open `analysis/paper/paper.Rmd` and knit to produce
the `paper.docx`, or run `rmarkdown::render("analysis/paper/paper.Rmd")`
in the R console

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse
