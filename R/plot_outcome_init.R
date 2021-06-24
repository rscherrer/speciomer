#' Plot the first generations
#'
#' Plots the first 1,000 generations of a simulation. Supposed to be used
#' on a simulation with only 1,000 generations recorded with high resolution.
#' The start of the x-axis is artificially moved to -20,000 to mimic the early
#' steps of a longer simulation (without this function we would have had to
#' simulate 20,000 generations of burn-in with high resolution, which would
#' have taken a lot of space).
#'
#' @param root Path to the simulation folder
#' @param approx Path to pass to \code{add_approx}. Leave \code{NULL} for no
#' overlay approximation plot.
#'
#' @details Wrapper around \code{plot_outcome_core}
#'
#' @return A patchwork
#'
#' @seealso \code{plot_outcome_core}
#'
#' @examples
#'
#' \dontrun{
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' plot_outcome_init(root)
#'
#' }
#'
#' @export

plot_outcome_init <- function(root, approx = NULL) {

  plot_outcome_core(root, true_start = -20000, approx = approx)

}
