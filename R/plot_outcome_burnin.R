#' Plot the burn-in period
#'
#' Equivalent of \code{plot_outcome_simple} but only shows the burn-in period.
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
#' plot_outcome_burnin(root)
#'
#' }
#'
#' @export

plot_outcome_burnin <- function (root, approx = NULL) {

  plot_outcome_core(root, to = 0, approx = approx)

}
