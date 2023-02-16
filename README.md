# speciomer

R package to read the output of [speciome](https://github.com/rscherrer/speciome).

<!-- badges: start -->
[![check-release](https://github.com/rscherrer/speciomer/workflows/check-release/badge.svg)](https://github.com/rscherrer/speciomer/actions)
<!-- badges: end -->

## Prerequisites

* [R](https://rstudio-education.github.io/hopr/starting.html) version 3.6.3 or higher
* (optional) [RStudio](https://rstudio-education.github.io/hopr/starting.html)

## Install

From within R, use

```r
install.packages("devtools") # to install devtools
devtools::install_github("rscherrer/speciomer", build_vignettes = TRUE) # install and build the vignette
```

## Use

Usage is exemplified in the vignette. View the vignette in your browser by typing, in R:

```r
browseVignettes("speciomer")
```

and clicking on the HTML link.

The purpose of the package is to produce tibbles (a type of table in R) from raw simulation data. The tibbles can be of different dimensions depending on the data they record. Click [here](docs/FIGURE.md) for an overview of the type of tables produced.

## Permissions

Copyright (c) Raphael Scherrer, 2023 (open source license will be added upon publication).
