% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_barcode.R
\name{plot_barcode}
\alias{plot_barcode}
\title{Plot encoded traits}
\usage{
plot_barcode(arch, xaxis = "location", size = 0.1)
}
\arguments{
\item{arch}{A data frame containing locus-wise genetic architecture
data (see \code{?read_architecture})}

\item{xaxis}{What to show on the x-axis. Either of "location" for genomic
location (continuous between 0 and 1) or "locus" for locus index.}

\item{size}{Size of the segments showing the loci, to be passed to
\code{geom_segment}}
}
\value{
A ggplot
}
\description{
Plots a barcode showing each locus in the genome and the trait they code for.
}
\examples{

root <- system.file("extdata", "sim-example", package = "speciomer")
arch <- read_architecture(root)$nodes
arch <- arch \%>\% dplyr::mutate(trait = recode_traits(trait))
plot_barcode(arch)

}
\seealso{
\code{read_architecture}
}
