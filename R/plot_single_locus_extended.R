#' Plot a single locus in details
#'
#' Plots several locus-specific variables through time for a specific locus,
#' together with information about the regression of genetic values against
#' genotypes across the population, at various time points.
#'
#' @param genome_data Data frame containing locus-specific data (see
#' \code{?read_loci} and \code{?plot_single_locus})
#' @param indiv_genomes Data frame containing individual whole genome data
#' (see \code{?read_individual_genomes} and \code{plot_gene_regression})
#' @param locus Index of the locus to plot
#'
#' @return A patchwork
#'
#' @note The regression of genetic values against genotypes requires
#' whole-genome data at the individual-level, which may likely come from
#' a separate simulation. This is because it uses a lot of memory to record
#' individual whole genomes as often as one might record locus-specific
#' variables, which are summaries across the population.
#'
#' See https://github.com/rscherrer/speciome for more details.
#'
#' @seealso \code{read_loci}, \code{read_individual_genomes},
#' \code{plot_single_locus}, \code{plot_gene_regression}
#'
#' @examples
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' root_indiv_genomes <- system.file(
#'   "extdata", "sim-indiv-genomes", package = "speciomer"
#' )
#' genome_data <- read_loci(root, c("freq", "Fst", "Qst", "Cst", "alpha"))
#' indiv_genomes <- read_individual_genomes(
#'   root_indiv_genomes, individual_variables = "ecotypes"
#' )
#'
#' plot_single_locus_extended(genome_data, indiv_genomes, locus = 1)
#'
#' @export

# Plot a single locus, with details at the individual level
plot_single_locus_extended <- function(genome_data, indiv_genomes, locus) {

  # Plot a single locus through time
  locus_plot <- plot_single_locus(genome_data, locus = locus)

  # Plot genetic value against genotype
  regression_plot <- plot_gene_regression(indiv_genomes, locus = locus)

  # Combine the plots
  patchwork::wrap_plots(
    locus_plot, regression_plot, heights = c(1, 3), guides = "collect"
  )

}
