#' Plot density across loci
#'
#' Plots histograms of a genetic variable across loci at a specific time point,
#' facetted by trait and with the variable on the y-axis.
#'
#' @param data A data frame with locus-specific data (see \code{?read_loci})
#' @param variable Name of the variable to plot (a string)
#' @param time Time point at which to show the variable
#' @param bins Parameter to be passed to \code{ggplot2::geom_histogram}
#'
#' @return A ggplot
#'
#' @seealso \code{read_loci}, \code{ggplot2::geom_histogram}
#'
#' @examples
#'
#' \dontrun{
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' data <- read_loci(root, "locus_Fst")
#' plot_density(data, "Fst", time = -1000)
#'
#' }
#'
#' @export

# Function to plot the density of a genetic variable
plot_density <- function(data, variable, time, bins = 20) {

  curr_time <- time

  data %>%
    dplyr::filter(time == curr_time) %>%
    ggplot2::ggplot(ggplot2::aes(y = get(variable), fill = trait)) +
    ggplot2::geom_histogram(alpha = 0.5, position = "identity", bins = bins) +
    ggplot2::scale_fill_manual(values = trait_colors()) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylab(variable) +
    ggplot2::xlab("Count") +
    ggplot2::facet_grid(. ~ trait)

}
