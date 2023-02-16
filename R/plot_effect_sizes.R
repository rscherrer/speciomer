#' Plot locus effect sizes
#'
#' Barplot of the effect sizes of loci across the genome.
#'
#' @param arch A data frame containing locus-wise genetic architecture
#' data (see \code{?read_architecture})
#' @param xaxis What to show on the x-axis. Either of "location" for genomic
#' location (continuous between 0 and 1) or "locus" for locus index.
#' @param rm_x Whether to remove the x-axis entirely
#'
#' @return A ggplot
#'
#' @seealso \code{read_architecture}
#'
#' @examples
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' arch <- read_architecture(root)$nodes
#' plot_effect_sizes(arch)
#'
#' @export

# Function to plot gene additive effect sizes
plot_effect_sizes <- function(arch, xaxis = "location", rm_x = TRUE) {

  .data <- NULL # hack for check to pass

  if (xaxis == "location") arch <- arch %>% dplyr::mutate(locus = .data$location)

  # Plot effect sizes
  plot <- arch %>%
    ggplot2::ggplot() +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$locus, xend = .data$locus, yend = abs(.data$effect)),
      y = 0,
      color = "steelblue"
    ) +
    ggplot2::ylab(parse(text = "'|'*eta*'|'"))

  if (rm_x) plot <- plot + rm_axis("x")

  return(plot)

}
