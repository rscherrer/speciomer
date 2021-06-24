#' Plot zoomed ecological dynamics
#'
#' Plots different zoomed windows of the dynamics of the ecological trait
#' densities, and the corresponding plots of average mutational effects of
#' ecological loci through time.
#'
#' @param root,root_init Arguments to pass to \code{plot_eco_trait_zoomed} and
#' \code{plot_eco_alpha_zoomed}
#'
#' @return A patchwork
#'
#' @seealso \code{plot_eco_trait_zoomed}, \code{plot_eco_alpha_zoomed}
#'
#' @export

# Function to plot zoomed ecological trait and its mutational effects
plot_eco_zoomed <- function(root, root_init) {

  # Plot zoomed trait densities
  plot_traits <- plot_eco_trait_zoomed(root, root_init)

  # Plot zoomed average mutational effects through time
  plot_alphas <- plot_eco_alpha_zoomed(root, root_init) + rm_strips()

  return(patchwork::wrap_plots(plot_traits, plot_alphas, ncol = 1))

}
