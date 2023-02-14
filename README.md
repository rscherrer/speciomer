# speciomer

R package to read the output of [speciome](https://github.com/rscherrer/speciome).

<!-- badges: start -->
[![R-CMD-check](https://github.com/rscherrer/speciomer/workflows/R-CMD-check/badge.svg)](https://github.com/rscherrer/speciomer/actions)
<!-- badges: end -->

## Prerequisites

## About

This package was developed in R 3.6.3.

## Installation

You can install the package form within R by running `devtools::install_github("rscherrer/speciomer")`, if you have `devtools` installed. 

Alternatively, you can download this repository and build the package from source by running in a terminal `R CMD build speciomer` from a directory that contains the repository. You can also open the `speciomer.Rproj` project in RStudio and install the package by clicking on "Install and Restart" in the "Build" menu.

## Use

Check out the vignette, which you can build upon installation, either by running `devtools::build()` from within the package in the R console after the package has been downloaded, or by using `devtools::install_github("rscherrer/speciomer", build_vignettes = TRUE)` when installing the package.

The purpose of the package is to produce tibbles (a type of table in R) from raw simulation data. The tibbles can be of different dimensions depending on the data they record. Here we outline the different tibbles that can be produced for a given simulation, which function to use for each of them as well as their dimensions.

![overview](extra/overview.png)

where *T* is the number of time points, *N* is the number of individuals, *L* is the number of loci and *E* is the number of edges. Note that the number of individuals can change from one time point to the next, our usage of a seemingly constant *N* here is only for illustratory purpose.
