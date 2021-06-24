#' Plot a simplified simulation outcome
#'
#' Equivalent of \code{plot_outcome_full} but without the plots of allele
#' frequencies, Fst, Qst and Cst through time (so only trait densities and
#' average mutational effects are shown).
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
#' plot_outcome_simple(root)
#'
#' }
#'
#' @export

plot_outcome_simple <- function(root, approx = NULL) {

  plot_outcome_core(root, by = 1000, approx = approx)

}
