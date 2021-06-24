#' Plot individual traits through time
#'
#' Plot the densities of each trait across the population and through time using
#' \code{geom_bin2d}.
#'
#' @param data An individual-level dataset containing trait values for each
#' individual (see \code{?read_individuals}).
#' @param burnin_bar Whether to add a vertical bar at time point zero
#'
#' @return A ggplot
#'
#' @seealso \code{read_individuals}
#'
#' @examples
#'
#' \dontrun{
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' data <- read_individuals(root, "individual_traits", ncol = 3)
#' plot_traits(data)
#'
#' }
#'
#' @export

# Function to plot individual traits through time
plot_traits <- function(data, burnin_bar = TRUE) {

  plot <- data %>%
    tidyr::pivot_longer(trait1:trait3, names_to = "trait") %>%
    dplyr::mutate(trait = stringr::str_remove(trait, "trait")) %>%
    dplyr::mutate(trait = recode_traits(trait)) %>%
    ggplot2::ggplot(ggplot2::aes(x = time / 1000, y = value)) +
    ggplot2::geom_bin2d(bins = 100) +
    ggplot2::facet_grid(. ~ trait) +
    ggplot2::scale_fill_continuous(type = "viridis") +
    ggplot2::xlab(parse(text = "'Time ('*10^3~'generations)'")) +
    ggplot2::ylab("Trait value") +
    ggplot2::labs(fill = "Count")

  if (burnin_bar) plot  <- plot +
      ggplot2::geom_vline(xintercept = 0, linetype = 4)

  return(plot)

}
