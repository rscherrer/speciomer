#' Plot locus degrees
#'
#' Barplot of the degrees of loci across the genome.
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
#' plot_degrees(arch)
#'
#' @export

# Function to plot gene degrees
plot_degrees <- function(arch, xaxis = "location", rm_x = TRUE) {

  .data <- NULL # hack for check to pass

  if (xaxis == "location") arch <- arch %>% dplyr::mutate(locus = .data$location)

  # Plot degrees
  plot <- arch %>%
    ggplot2::ggplot() +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$locus, xend = .data$locus, yend = .data$degree),
      y = 0,
      color = "firebrick"
    ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank()
    ) +
    ggplot2::ylab("Degree")

  if (rm_x) plot <- plot + rm_axis("x")

  return(plot)

}
