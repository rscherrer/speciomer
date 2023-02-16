#' Plot a full simulation outcome
#'
#' Plots trait density plots through time as well as locus-specific allele
#' frequencies, Fst, Qst, Cst and average mutational effects through time,
#' and histograms showing the distributions of these variables at the last
#' generation. Variables are shown throughout the whole simulation.
#'
#' @param root Path to the simulation folder
#' @param approx Path to pass to \code{add_approx}. Leave \code{NULL} for no
#' overlay approximation plot.
#'
#' @details Wrapper around \code{plot_outcome_core}
#'
#' @return A patchwork
#'
#' @note Locus-specific average mutational effects are subsampled every 1,000
#' generations to avoid crowding the plots.
#'
#' @seealso \code{plot_outcome_core}
#'
#' @examples
#'
#' \dontrun{
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' plot_outcome_full(root)
#'
#' }
#'
#' @export

# Plot an overview of the simulation outcome
plot_outcome_full <- function(root, approx = NULL) {

  plot_outcome_core(root, full = TRUE, by = 1000, approx = approx)

}
