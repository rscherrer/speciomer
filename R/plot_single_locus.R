#' Plot a single locus against the rest of the genome
#'
#' Plots the trajectory of one locus through time for some variables, on top
#' of a ribbon showing the 95 percent interval of trajectories across genes
#' coding for the same trait. Shows the allele frequency, Fst, Qst, Cst and
#' average mutational effect.
#'
#' @param data A data frame containing locus-specific data (see
#' \code{?read_loci})
#' @param locus Index of the locus to plot
#'
#' @details The color of the background ribbon corresponds to the trait encoded
#' by the locus.
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
#' data <- read_loci(root, c("freq", "Fst", "Qst", "Cst", "alpha"))
#' plot_single_locus(data, locus = 1)
#'
#' }
#'
#' @export

# Function to plot a single locus through time against the rest
plot_single_locus <- function(data, locus) {

  .data <- NULL # hack for check to pass

  curr_locus <- locus

  # What trait does the locus code for?
  curr_trait <- data$trait[dplyr::first(which(data$locus == locus))]

  # Pick a color
  color <- trait_colors()[curr_trait]

  # Make the plots
  data %>%
    dplyr::filter(.data$trait == curr_trait) %>%
    tidyr::pivot_longer(
      c(.data$Fst, .data$alpha, .data$freq, .data$Qst, .data$Cst),
      names_to = "statistic"
    ) %>%
    dplyr::group_by(.data$time, .data$statistic) %>%
    dplyr::summarize(
      low = stats::quantile(.data$value, 0.025),
      high = stats::quantile(.data$value, 0.975),
      value = .data$value[.data$locus == curr_locus]
    ) %>%
    dplyr::mutate(
      statistic = factor(
        .data$statistic, levels = c("freq", "Fst", "alpha", "Qst", "Cst")
      ),
      statistic = forcats::fct_recode(
        .data$statistic, "p" = "freq", "F[ST]" = "Fst", "alpha" = "alpha",
        "Q[ST]" = "Qst", "C[ST]" = "Cst"
      )
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$time / 1000, y = .data$value)) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(xmax = .data$time / 1000, ymin = .data$low, ymax = .data$high),
      fill = color, alpha = 0.3
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(xintercept = 0, linetype = 4) +
    ggplot2::facet_wrap(
      . ~ statistic, scales = "free_y", labeller = ggplot2::label_parsed
    ) +
    ggplot2::xlab(parse(text = "'Time ('*10^3~'generations)'")) +
    ggplot2::ylab(NULL)

}
