#' Plot trait densities from root folder
#'
#' Wrapper around \code{plot_traits} that reads trait data and plots them from
#' the root folder of a simulation.
#'
#' @param root Path to the simulation folder
#' @param burnin_bar Argument to be passed to \code{plot_traits}
#'
#' @return A ggplot
#'
#' @seealso \code{read_individuals}, \code{plot_traits}
#'
#' @examples
#'
#' \dontrun{
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' plot_traits_from_root(root)
#'
#' }
#'
#' @export

plot_traits_from_root <- function(root, burnin_bar = TRUE) {

  # Read trait data
  data <- read_individuals(root, "individual_traits", ncols = 3)

  # Plot them
  plot_traits(data, burnin_bar)

}
