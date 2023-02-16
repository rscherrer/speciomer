#' Plot genetic values against genotypes at a certain locus
#'
#' Plots the regression of genetic values against allele counts (genotypes) for
#' a particular locus, facetted by time point. Genetic values are represented
#' both by boxplots and dots (individuals are binned by genetic value such that
#  genetic values with more individuals appear bigger), colored by ecotype.
#'
#' @param data Individual whole genome data with information about individual
#' ecotypes (\code{read_individual_genomes} with \code{individual_trait =
#' "ecotypes"}).
#' @param locus Index of the locus to plot
#'
#' @return A ggplot
#'
#' @seealso \code{read_individual_genomes}
#'
#' @examples
#'
#' root <- system.file("extdata", "sim-indiv-genomes", package = "speciomer")
#' data <- read_individual_genomes(root, individual_variables = "ecotypes")
#' plot_gene_regression(data, locus = 1)
#'
#' @export

# Function to plot a regression of genetic values against genotype
plot_gene_regression <- function(data, locus) {

  .data <- NULL # hack for check to pass

  curr_locus <- locus

  # Bin by genetic value to avoid crowding of the plot
  data_binned <- data %>%
    dplyr::filter(.data$locus == curr_locus) %>%
    dplyr::mutate(
      time_lab = factor(
        paste0("t = ", .data$time),
        levels = paste0("t = ", unique(.data$time))
      )
    ) %>%
    dplyr::group_by(.data$time, .data$time_lab, .data$genotype, .data$ecotype, .data$genvalue) %>%
    dplyr::summarize(n = dplyr::n())

  # Plot genetic values against genotype at various time points
  data %>%
    dplyr::filter(.data$locus == curr_locus) %>%
    dplyr::mutate(
      time_lab = factor(
        paste0("t = ", .data$time),
        levels = paste0("t = ", unique(.data$time))
      )
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = factor(.data$genotype), y = .data$genvalue, fill = factor(.data$ecotype))
    ) +
    ggplot2::geom_jitter(
      data = data_binned,
      mapping = ggplot2::aes(size = .data$n),
      shape = 21,
      alpha = 0.3,
      position = ggplot2::position_jitterdodge(
        jitter.width = 0.2, dodge.width = 0.7
      )
    ) +
    ggplot2::geom_boxplot(outlier.color = NA) +
    ggplot2::scale_fill_manual(values = ecotype_colors()) +
    ggplot2::xlab("Allele count") +
    ggplot2::ylab("Genetic value") +
    ggplot2::labs(fill = "Ecotype", size = "Count") +
    ggplot2::facet_wrap(. ~ time_lab)

}
