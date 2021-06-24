#' Plot simulation outcome
#'
#' Plots an overview of a simulation outcome in different ways depending on the
#' option.
#'
#' @param root Path to the simulation folder
#' @param which Which type of outcome to plot? Either of: "full", shows the
#' trait density plots and all the locus-specific variables through the whole
#' simulation; "simple", shows only trait density plots and average mutational
#' effect through time; "burnin", same as "simple" but only during the burn-in
#' period; "init", same as "simple" but during the first 1,000 generations;
#' "custom", for custom arguments to be passed to \code{plot_outcome_core}
#' through \code{...}
#' @param approx Path to pass to \code{add_approx}. Leave \code{NULL} for no
#' overlay approximation plot.
#' @param ... Further arguments to be passed to \code{plot_outcome_core}
#'
#' @details See \code{?plot_outcome_core}
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
#' plot_outcome_root(root)
#'
#' }
#'
#' @export

plot_outcome <- function(root, which = "full", approx = NULL, ...) {

  if (which == "full") plot <- plot_outcome_full(root, approx = approx)
  if (which == "simple") plot <- plot_outcome_simple(root, approx = approx)
  if (which == "burnin") plot <- plot_outcome_burnin(root, approx = approx)
  if (which == "init") plot <- plot_outcome_init(root, approx = approx)
  if (which == "custom") plot <- plot_outcome_core(root, approx = approx, ...)

  return(plot)

}
