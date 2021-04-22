# speciomer

This is an R package to deal with the output of the `speciome` simulations (see [here](https://github.com/rscherrer/speciome)).

## About

This package was developed in R 3.6.3.

## Installation

You can install the package form within R by running `devtools::install_github("rscherrer/speciomer")`, if you have `devtools` installed. 

Alternatively, you can download this repository and build the package from source by running in a terminal `R CMD build speciomer` from a directory that contains the repository. You can also open the `speciomer.Rproj` project in RStudio and install the package by clicking on "Install and Restart" in the "Build" menu.

## Use

Check out the vignette. Problem: the vignette must be built upon installation, either by running `devtools::build()` from within the package in the R console after the package has been downloaded, or by using `devtools::install_github("rscherrer/speciomer", build_vignettes = TRUE)` to install the package.
