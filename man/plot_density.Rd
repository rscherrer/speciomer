% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_density.R
\name{plot_density}
\alias{plot_density}
\title{Plot density across loci}
\usage{
plot_density(data, variable, time, bins = 20)
}
\arguments{
\item{data}{A data frame with locus-specific data (see \code{?read_loci})}

\item{variable}{Name of the variable to plot (a string)}

\item{time}{Time point at which to show the variable}

\item{bins}{Parameter to be passed to \code{ggplot2::geom_histogram}}
}
\value{
A ggplot
}
\description{
Plots histograms of a genetic variable across loci at a specific time point,
facetted by trait and with the variable on the y-axis.
}
\examples{

\dontrun{

root <- system.file("extdata", "sim-example", package = "speciomer")
data <- read_loci(root, "locus_Fst")
plot_density(data, "Fst", time = -1000)

}

}
\seealso{
\code{read_loci}, \code{ggplot2::geom_histogram}
}
