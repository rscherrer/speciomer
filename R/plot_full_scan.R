#' Plot extended genome scan
#'
#' Plots an assemblage of various genome scans at a given time point, locus-
#' specific elements of the genetic architecture and the gene network for one
#' of the three traits. Genome scans are shown for allele frequencies, Fst,
#' Qst, Cst and average mutational effects.
#'
#' @param data A data frame containing locus-specific data (see
#' \code{?read_loci})
#' @param grn A list of two data frames containing the genetic architecture
#' (the output of \code{read_architecture})
#' @param time Which time point to plot the variables for in the genome scans.
#' @param trait Which trait to show the network for (1, 2 or 3)
#' @param trait_data Data frame with individual-wise trait data
#' (see \code{?read_indviduals}). If provided, a histogram of ecological trait
#' values at the specified time point will be added to the patchwork.
#'
#' @return A patchwork
#'
#' @note \code{grn} is internally converted to a \code{tbl_graph} before being
#' passed to \code{plot_network}.
#'
#' @seealso \code{read_architecture}, \code{read_loci}, \code{plot_genome_scan},
#' \code{plot_barcode}, \code{plot_effect_sizes}, \code{plot_degrees},
#' \code{plot_network}, \code{read_individuals}, \code{plot_eco_trait_histogram}
#'
#' @examples
#'
#' \dontrun{
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' data <- read_loci(root, c("freq", "Fst", "Qst", "Cst", "alpha"))
#' grn <- read_architecture(root)
#' plot_full_scan(data, grn, time = -1000, trait = 1)
#'
#' }
#'
#' @export

# Function to plot a complete genome scan at a given time
plot_full_scan <- function(data, grn, time, trait, trait_data = NULL) {

  curr_trait <- trait

  # Various layers of genome scans
  genome_fst_scan <- plot_genome_scan(data, variable = "Fst", time = time, ylab_parsed = "F[ST]")
  genome_freq_scan <- plot_genome_scan(data, variable = "rare_freq", time = time, ylab_parsed = "p[min]")
  genome_alpha_scan <- plot_genome_scan(data, variable = "abs_alpha", time = time, ylab_parsed = "'|'*alpha*'|'")
  genome_qst_scan <- plot_genome_scan(data, variable = "Qst", time = time, ylab_parsed = "Q[ST]")
  genome_cst_scan <- plot_genome_scan(data, variable = "Cst", time = time, ylab_parsed = "C[ST]")

  # Fix the scale on some of them
  genome_fst_scan <- genome_fst_scan + ggplot2::ylim(c(0, 1))
  genome_qst_scan <- genome_qst_scan + ggplot2::ylim(c(0, 1))
  genome_cst_scan <- genome_cst_scan + ggplot2::ylim(c(0, 1))
  genome_freq_scan <- genome_freq_scan + ggplot2::ylim(c(0, 0.5))

  # Extract genome architecture from gene regulatory network
  arch <- grn[["nodes"]]

  # Traits encoded by each locus
  barcode <- plot_barcode(arch)

  # Genetic architecture for a given trait (to avoid clutter)
  effect_size_plot <- plot_effect_sizes(dplyr::filter(arch, trait == curr_trait))
  degree_plot <- plot_degrees(dplyr::filter(arch, trait == curr_trait))
  network_plot <- plot_network(tidygraph::as_tbl_graph(grn), trait = curr_trait)

  # Combine all layers of the plot
  plot <- patchwork::wrap_plots(
    network_plot, degree_plot, effect_size_plot, barcode, genome_fst_scan,
    genome_qst_scan, genome_cst_scan, genome_freq_scan, genome_alpha_scan,
    heights = c(5, 2, 2, 1, 2, 2, 2, 2, 2), guides = "collect"
  )

  if (is.null(trait_data)) return(plot)

  # Add trait histogram if needed
  eco_trait_hist <- plot_eco_trait_histogram(trait_data, time = time) +
    ggplot2::xlim(-5, 5)

  patchwork::wrap_plots(eco_trait_hist, plot, ncol = 1, heights = c(1, 10))

}
