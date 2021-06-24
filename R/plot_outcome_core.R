#' Plot simulation outcome
#'
#' Plots an overview of a simulation outcome. Shows at least trait density
#' plots through time, and different locus-level plots depending on the option.
#' The locus-level plots show allele frequencies, Fst, Qst, Cst and average
#' mutational effect. Plots are facetted by trait. The time window to plot can
#' be set.
#'
#' @param root Path to the simulation folder
#' @param from,to Range of time points to plot (defaults to the whole range)
#' @param by Frequency of time points to sub-sample when plotting locus-specific
#' lines. Defaults to 1, all time points are plotted. Use e.g. 100 to plot
#' every 100 generations.
#' @param true_start Shifts the time points by some amount. For example, if an
#' extra simulation was run with 1,000 generations to record the beginning of
#' the burn-in with a high resolution of a simulation that would otherwise be
#' 20,000 generation-long in burn-in time, use \code{true_start = -20000}.
#' @param full Whether to plot the full figure, i.e. with all locus-specific
#' plots and not just the trait density plots and the average mutational effect
#' plots.
#' @param approx Path to pass to \code{add_approx}. Leave \code{NULL} for no
#' overlay approximation plot.
#'
#' @details Details about how the function works.
#'
#' @return A patchwork
#'
#' @note To keep in mind
#'
#' @seealso \code{read_individuals}, \code{read_loci}, \code{read_traits},
#' \code{plot_traits}, \code{plot_loci_tt}, \code{plot_density},
#' \code{add_line}, \code{add_approx}
#'
#' @examples
#'
#' \dontrun{
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' plot_outcome_core(root)
#'
#' }
#'
#' @export

plot_outcome_core <- function(
  root, from = NULL, to = NULL, by = 1, true_start = NULL, full = FALSE,
  approx = NULL
) {

  # Read trait data
  trait_data <- read_individuals(root, "traits", ncols = 3)

  # Change the time points if needed
  if (!is.null(true_start)) trait_data <- trait_data %>%
      dplyr::mutate(time = time + true_start - min(time))

  if (is.null(from)) from <- min(unique(trait_data$time))
  if (is.null(to)) to <- max(unique(trait_data$time))

  # Keep only the range of interest
  trait_data <- trait_data %>% dplyr::filter(time >= from, time <= to)

  # Should we plot the burn-in bar?
  burnin_bar <- from <= 0 & to >= 0

  # Plot traits through time
  trait_plot <- plot_traits(trait_data, burnin_bar = burnin_bar)

  # Add deterministic approximation if available
  if (!is.null(approx)) trait_plot <- trait_plot + add_approx(approx)

  variables <- "alpha"
  if (full) variables <- c(variables, "Fst", "Qst", "Cst", "freq")

  # Read genome data
  genome_data <- read_loci(root, variables) %>%
    dplyr::mutate(trait = recode_traits(trait))

  # Change the time points if needed
  if (!is.null(true_start)) genome_data <- genome_data %>%
    dplyr::mutate(time = time + true_start - min(time))

  # Keep only the range of interest
  genome_data <- genome_data %>% dplyr::filter(time >= from, time <= to)

  # Subsample
  genome_data_sub <- genome_data %>% dplyr::filter(time %% by == 0)

  # Plot loci through time
  if (full) {

    fst_plot <- genome_data_sub %>% plot_loci_tt("Fst", burnin_bar = burnin_bar) + ggplot2::ylab(parse(text = "F[ST]"))
    qst_plot <- genome_data_sub %>% plot_loci_tt("Qst", burnin_bar = burnin_bar) + ggplot2::ylab(parse(text = "Q[ST]"))
    cst_plot <- genome_data_sub %>% plot_loci_tt("Cst", burnin_bar = burnin_bar) + ggplot2::ylab(parse(text = "C[ST]"))
    freq_plot <- genome_data_sub %>% plot_loci_tt("freq", burnin_bar = burnin_bar) + ggplot2::ylab(parse(text = "p"))

    # Read genome-wise "ST" statistics if needed
    genome_wide_xst_data <- read_traits(root, c("Fst", "Qst", "Cst")) %>%
      dplyr::mutate(trait = recode_traits(trait))

    # Add average to some of the plots
    fst_plot <- fst_plot + add_line(genome_wide_xst_data, "Fst")
    qst_plot <- qst_plot + add_line(genome_wide_xst_data, "Qst")
    cst_plot <- cst_plot + add_line(genome_wide_xst_data, "Cst")

    # Histograms
    fst_hist <- genome_data_sub %>% plot_density("Fst", time = dplyr::last(.$time)) + ggplot2::ylab(parse(text = "F[ST]"))
    qst_hist <- genome_data_sub %>% plot_density("Qst", time = dplyr::last(.$time)) + ggplot2::ylab(parse(text = "Q[ST]"))
    cst_hist <- genome_data_sub %>% plot_density("Cst", time = dplyr::last(.$time)) + ggplot2::ylab(parse(text = "C[ST]"))
    freq_hist <- genome_data_sub %>% plot_density("freq", time = dplyr::last(.$time)) + ggplot2::ylab(parse(text = "p"))

    # Customize the histograms
    fst_hist <- fst_hist + rm_axis("y") + rm_strips() + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 3))
    qst_hist <- qst_hist + rm_axis("y") + rm_strips() + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 3))
    cst_hist <- cst_hist + rm_axis("y") + rm_strips() + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 3))
    freq_hist <- freq_hist + rm_axis("y") + rm_strips() + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 3))

  }

  # Same but for alpha
  alpha_plot <- genome_data_sub %>% plot_loci_tt("alpha", burnin_bar = burnin_bar) + ggplot2::ylab(parse(text = "alpha"))
  alpha_hist <- genome_data_sub %>% plot_density("alpha", time = dplyr::last(.$time)) + ggplot2::ylab(parse(text = "alpha"))
  alpha_hist <- alpha_hist + rm_axis("y") + rm_strips() + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 3))

  # Separate trait plot from its legend
  trait_plot_legend <- cowplot::get_legend(trait_plot)
  trait_plot <- trait_plot + ggplot2::theme(legend.position = "none")

  # Combine the plots
  row1 <- (trait_plot + trait_plot_legend) + patchwork::plot_layout(widths = c(4, 1))
  row6 <- (alpha_plot | alpha_hist) + patchwork::plot_layout(widths = c(4, 1))

  if (full) {
    row2 <- (fst_plot | fst_hist) + patchwork::plot_layout(widths = c(4, 1))
    row3 <- (qst_plot | qst_hist) + patchwork::plot_layout(widths = c(4, 1))
    row4 <- (cst_plot | cst_hist) + patchwork::plot_layout(widths = c(4, 1))
    row5 <- (freq_plot | freq_hist) + patchwork::plot_layout(widths = c(4, 1))
    return(row1 / row2 / row3 / row4 / row5 / row6)
  }

  return(row1 / row6)

}