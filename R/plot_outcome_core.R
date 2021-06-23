plot_outcome_core <- function(
  root, from = NULL, to = NULL, by = 1, true_start = NULL, full = FALSE,
  approx = NULL
) {

  # Read trait data
  trait_data <- read_individuals(root, "traits", ncols = 3)

  # Change the time points if needed
  if (!is.null(true_start)) trait_data <- trait_data %>%
      mutate(time = time + true_start - min(time))

  if (is.null(from)) from <- min(unique(trait_data$time))
  if (is.null(to)) to <- max(unique(trait_data$time))

  # Keep only the range of interest
  trait_data <- trait_data %>% filter(time >= from, time <= to)

  # Should we plot the burn-in bar?
  burnin_bar <- from <= 0 & to >= 0

  # Plot traits through time
  trait_plot <- plot_traits(trait_data, burnin_bar = burnin_bar)

  # Add deterministic approximation if available
  if (!is.null(approx)) trait_plot <- trait_plot + add_approx(approx)

  variables <- "alpha"
  if (full) variables <- c(variables, "Fst", "Qst", "Cst", "freq")

  # Read genome data
  genome_data <- read_loci(root, variables) %>% mutate(trait = recode_traits(trait))

  # Change the time points if needed
  if (!is.null(true_start)) genome_data <- genome_data %>%
    mutate(time = time + true_start - min(time))

  # Keep only the range of interest
  genome_data <- genome_data %>% filter(time >= from, time <= to)

  # Subsample
  genome_data_sub <- genome_data %>% filter(time %% by == 0)

  # Plot loci through time
  if (full) {

    fst_plot <- genome_data_sub %>% plot_loci_tt("Fst", burnin_bar = burnin_bar) + ylab(parse(text = "F[ST]"))
    qst_plot <- genome_data_sub %>% plot_loci_tt("Qst", burnin_bar = burnin_bar) + ylab(parse(text = "Q[ST]"))
    cst_plot <- genome_data_sub %>% plot_loci_tt("Cst", burnin_bar = burnin_bar) + ylab(parse(text = "C[ST]"))
    freq_plot <- genome_data_sub %>% plot_loci_tt("freq", burnin_bar = burnin_bar) + ylab(parse(text = "p"))

    # Read genome-wise "ST" statistics if needed
    genome_wide_xst_data <- read_traits(root, c("Fst", "Qst", "Cst")) %>%
      mutate(trait = recode_traits(trait))

    # Add average to some of the plots
    fst_plot <- fst_plot + add_line(genome_wide_xst_data, "Fst")
    qst_plot <- qst_plot + add_line(genome_wide_xst_data, "Qst")
    cst_plot <- cst_plot + add_line(genome_wide_xst_data, "Cst")

    # Histograms
    fst_hist <- genome_data_sub %>% plot_density("Fst", time = last(.$time)) + ylab(parse(text = "F[ST]"))
    qst_hist <- genome_data_sub %>% plot_density("Qst", time = last(.$time)) + ylab(parse(text = "Q[ST]"))
    cst_hist <- genome_data_sub %>% plot_density("Cst", time = last(.$time)) + ylab(parse(text = "C[ST]"))
    freq_hist <- genome_data_sub %>% plot_density("freq", time = last(.$time)) + ylab(parse(text = "p"))

    # Customize the histograms
    fst_hist <- fst_hist + rm_axis("y") + rm_strips() + scale_x_continuous(breaks = pretty_breaks(n = 3))
    qst_hist <- qst_hist + rm_axis("y") + rm_strips() + scale_x_continuous(breaks = pretty_breaks(n = 3))
    cst_hist <- cst_hist + rm_axis("y") + rm_strips() + scale_x_continuous(breaks = pretty_breaks(n = 3))
    freq_hist <- freq_hist + rm_axis("y") + rm_strips() + scale_x_continuous(breaks = pretty_breaks(n = 3))

  }

  # Same but for alpha
  alpha_plot <- genome_data_sub %>% plot_loci_tt("alpha", burnin_bar = burnin_bar) + ylab(parse(text = "alpha"))
  alpha_hist <- genome_data_sub %>% plot_density("alpha", time = last(.$time)) + ylab(parse(text = "alpha"))
  alpha_hist <- alpha_hist + rm_axis("y") + rm_strips() + scale_x_continuous(breaks = pretty_breaks(n = 3))

  # Separate trait plot from its legend
  trait_plot_legend <- get_legend(trait_plot)
  trait_plot <- trait_plot + theme(legend.position = "none")

  # Combine the plots
  row1 <- (trait_plot + trait_plot_legend) + plot_layout(widths = c(4, 1))
  row6 <- (alpha_plot | alpha_hist) + plot_layout(widths = c(4, 1))

  if (full) {
    row2 <- (fst_plot | fst_hist) + plot_layout(widths = c(4, 1))
    row3 <- (qst_plot | qst_hist) + plot_layout(widths = c(4, 1))
    row4 <- (cst_plot | cst_hist) + plot_layout(widths = c(4, 1))
    row5 <- (freq_plot | freq_hist) + plot_layout(widths = c(4, 1))
    return(row1 / row2 / row3 / row4 / row5 / row6)
  }

  return(row1 / row6)

}
