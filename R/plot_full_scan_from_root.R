#' Plot extended genome scan (from root folder)
#'
#' Plots an extended genome scan with multiple layers at a series of time
#' points, taking only the root folder of the simulation as input.
#'
#' @param root Path to the simulation folder
#' @param times The time points to plot
#' @param show_hist Whether to show a histogram of ecological trait values too
#' @param show_time Whether to show the time point on each plot
#'
#' @details Runs \code{plot_full_scan} in the background
#'
#' @return A list of ggplots (or a single ggplot if only one time point is
#' specified)
#'
#' @seealso \code{plot_full_scan}, \code{read_loci}, \code{read_architecture}
#'
#' @examples
#'
#' \dontrun{
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' plot_full_scan(root, times = -1000)
#'
#' }
#'
#' @export

# Function to plot a full genome scan from the simulation root folder
plot_full_scan_from_root <- function(
  root, times, show_traits = TRUE, show_time = FALSE
) {

  # Read genome data
  genome_data <- read_loci(root, c("Fst", "alpha", "freq", "Qst", "Cst")) %>%
    dplyr::mutate(trait = recode_traits(trait))

  # Add modified variables
  genome_data <- genome_data %>%
    dplyr::mutate(
      abs_alpha = abs(alpha),
      rare_freq = purrr::map_dbl(freq, ~ min(.x, 1 - .x))
    )

  # Read the genetic architecture
  grn_arch <- purrr::map(
    read_architecture(root), dplyr::mutate, trait = recode_traits(trait)
  )

  # Architecture of the nodes only
  genome_arch <- grn_arch[["nodes"]]

  # Read individual trait data if needed
  trait_data <- NULL
  if (show_traits) trait_data <- read_individuals(root, "traits", ncol = 3)

  # Plot complete scans at different times
  plots <- purrr::map(times, function(time) {

    print(paste0("Plotting timepoint ", time, "..."))

    # Plot the full scan
    plot <- plot_full_scan(
      genome_data, grn_arch, time, "Ecological", trait_data
    )

    # Add a time point if needed
    if (show_time) plot <- plot +
      patchwork::plot_annotation(subtitle = sprintf("Generation %s", time))

    return(plot)

  })

  if (length(plots) == 1) return(plots[[1]])

  return(plots)

}
