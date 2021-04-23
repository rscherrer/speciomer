# speciomer

This is an R package to deal with the output of the `speciome` simulations (see [here](https://github.com/rscherrer/speciome)).

## About

This package was developed in R 3.6.3.

## Installation

You can install the package form within R by running `devtools::install_github("rscherrer/speciomer")`, if you have `devtools` installed. 

Alternatively, you can download this repository and build the package from source by running in a terminal `R CMD build speciomer` from a directory that contains the repository. You can also open the `speciomer.Rproj` project in RStudio and install the package by clicking on "Install and Restart" in the "Build" menu.

## Use

Check out the vignette, which you can build upon installation, either by running `devtools::build()` from within the package in the R console after the package has been downloaded, or by using `devtools::install_github("rscherrer/speciomer", build_vignettes = TRUE)` when installing the package.

The purpose of the package is to produce tibbles (a type of table in R) from raw simulation data. The tibbles can be of different dimensions depending on the data they record:

![overview](extra/overview.png)
