#' Plot a genome scan
#'
#' Plots a variable across the genome at a certain time point.
#'
#' @param data A data frame containing locus-specific data (see
#' \code{?read_loci})
#' @param variable Name of the variable on the y-axis
#' @param time What time point to show the data for
#' @param xaxis What to show on the x-axis. Either of "location" for genomic
#' location (continuous between 0 and 1) or "locus" for locus index.
#' @param ylab_parsed Label of the y-axis, parsed
#' @param rm_x Whether to remove the x-axis entirely
#'
#' @return A ggplot
#'
#' @seealso \code{read_loci}
#'
#' @examples
#'
#' x <- 1 + 1
#'
#' @export

# Function to plot a genome scan
plot_genome_scan <- function(
  data, variable, time, xaxis = "location", ylab_parsed = NULL, rm_x = TRUE
) {

  curr_time <- time

  if (is.null(ylab_parsed)) ylab_parsed <- paste0("'", variable, "'")
  if (xaxis == "location") data <- data %>% dplyr::mutate(locus = location)

  # Genome scan plot
  plot <- data %>%
    dplyr::filter(time == curr_time) %>%
    ggplot2::ggplot(ggplot2::aes(x = locus)) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        xmin = locus, xmax = locus, ymax = get(variable), group = chromosome,
        fill = as.character(chromosome)
      ),
      ymin = 0
    ) +
    ggplot2::scale_fill_manual(values = c("gray40", "gray20", "gray40")) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab(ifelse(xaxis == "locus", "Locus", "Location")) +
    ggplot2::ylab(parse(text = ylab_parsed))

  if (rm_x) plot <- plot + rm_axis("x")

  return(plot)

}
