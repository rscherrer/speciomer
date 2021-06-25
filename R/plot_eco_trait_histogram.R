#' Ecological trait histogram
#'
#' Plots a histogram of the ecological trait values across the population
#' at a certain time point.
#'
#' @param data A data frame containing individual-wise data with a column named
#' "trait1" (see \code{read_individuals}).
#' @param time The time point at which to plot the distribution
#'
#' @return A ggplot
#'
#' @seealso \code{read_individuals}
#'
#' @examples
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' data <- read_individuals(root, "traits", ncol = 3)
#' plot_eco_trait_histogram(data, -1000)
#'
#' @export

# Function to plot a histogram of ecological trait values at a certain time
plot_eco_trait_histogram <- function(data, time) {

  curr_time <- time

  data %>%
    dplyr::filter(time == curr_time) %>%
    ggplot2::ggplot(ggplot2::aes(x = trait1)) +
    ggplot2::geom_histogram(bins = 100, fill = "forestgreen", alpha = 0.5) +
    ggplot2::ylab("Count") +
    ggplot2::xlab("Ecological trait")

}
