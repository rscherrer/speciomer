#' Plot a gene network
#'
#' Plots a gene regulatory network with a semi-circular layout. Good for
#' plotting on top of genome scans. Interaction weights are mapped to
#' edge transparency.
#'
#' @param arch A \code{tbl_graph} containing the genetic architecture (the
#' output of \code{read_architecture} passed through
#' \code{tidyrgraph::as_tbl_graph})
#' @param trait Name of the trait for which to show the network (as it appears
#' in \code{arch})
#' @param xaxis What to show on the x-axis. Either of "location" for genomic
#' location (continuous between 0 and 1) or "locus" for locus index.
#'
#' @return A ggraph
#'
#' @note Somehow the \code{ggraph} library needs to be loaded for this function
#' to produce the expected output (use \code{library(ggraph)}).
#'
#' @seealso \code{read_architecture}, \code{tidygraph::as_tbl_graph}
#'
#' @examples
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' arch <- read_architecture(root)
#' arch <- tidygraph::as_tbl_graph(arch)
#' plot_network(arch, trait = 1)
#'
#' @export

# Function to plot a gene network
plot_network <- function(arch, trait, xaxis = "location") {

  .data <- NULL # hack for check to pass

  if (xaxis == "location") arch <- arch %>% dplyr::mutate(locus = .data$location)

  curr_trait <- trait
  color <- trait_colors()[trait]

  # Plot the network
  arch %>%
    dplyr::filter(.data$trait == curr_trait) %>%
    ggraph::ggraph(layout = 'linear', sort.by = .data$locus, use.numeric = TRUE) +
    ggraph::geom_edge_arc(
      mapping = ggplot2::aes(alpha = abs(.data$weight)),
      color = color,
      fold = TRUE
    ) +
    ggplot2::labs(edge_alpha = parse(text = "'|'*omega*'|'")) +
    ggplot2::theme(legend.position = "right")

}
