#' Plot encoded traits
#'
#' Plots a barcode showing each locus in the genome and the trait they code for.
#'
#' @param arch A data frame containing locus-wise genetic architecture
#' data (see \code{?read_architecture})
#' @param xaxis What to show on the x-axis. Either of "location" for genomic
#' location (continuous between 0 and 1) or "locus" for locus index.
#' @param size Size of the segments showing the loci, to be passed to
#' \code{geom_segment}
#'
#' @return A ggplot
#'
#' @seealso \code{read_architecture}
#'
#' @examples
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' arch <- read_architecture(root)$nodes
#' arch <- arch %>% dplyr::mutate(trait = recode_traits(trait))
#' plot_barcode(arch)
#'
#' @export

# Function to plot the traits encoded by each locus
plot_barcode <- function(arch, xaxis = "location", size = 0.1) {

  .data <- NULL # hack for check to pass

  if (xaxis == "location") arch <- arch %>% dplyr::mutate(locus = .data$location)

  # Barcode plot
  arch %>%
    dplyr::select(.data$locus, .data$trait) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$locus, xend = .data$locus, color = .data$trait)) +
    ggplot2::geom_segment(y = 0, yend = 1, size = size) +
    ggplot2::scale_color_manual(values = trait_colors()) +
    ggplot2::theme_void() +
    ggplot2::labs(color = "Trait")

}
