#' Plot locus-specific variables through time
#'
#' Generates a line plot of one variable through time, with one line for each
#' locus, facetted by trait.
#'
#' @param data A data frame with locus-specific data through time (see
#' \code{?read_loci}).
#' @param variable Name of the variable to plot (a string)
#' @param burnin_bar Whether to add a vertical bar at time point zero
#'
#' @return A ggplot
#'
#' @seealso \code{read_loci}
#'
#' @examples
#'
#' \dontrun{
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' data <- read_loci(root, "locus_Fst")
#' data <- data %>% dplyr::mutate(trait = recode_traits(trait))
#' plot_loci_tt(data, "Fst")
#'
#' }
#'
#' @export

# Function to plot loci through time
plot_loci_tt <- function(data, variable, burnin_bar = TRUE) {

  plot <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = time / 1000)) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(group = locus, y = get(variable), color = trait),
      alpha = 0.3
    ) +
    ggplot2::facet_grid(. ~ trait) +
    ggplot2::xlab(parse(text = "'Time ('*10^3~'generations)'")) +
    ggplot2::ylab(parse(text = variable)) +
    ggplot2::scale_color_manual(values = trait_colors()) +
    ggplot2::guides(color = FALSE, linetype = FALSE)

  if (burnin_bar) plot <- plot +
      ggplot2::geom_vline(xintercept = 0, linetype = 4)

  return(plot)

}
