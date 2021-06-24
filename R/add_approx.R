#' Add deterministic approximation line
#'
#' Adds lines for a deterministic simulation approximating the stochastic
#' simulation, on top of the trait density plot through time.
#'
#' @param path Path to the approximation data file (this should be an RDS file)
#'
#' @note See the \code{speciome-approx} repository and \code{speciomx} package
#' for how to simulate the deterministic model.
#'
#' https://github.com/rscherrer/speciome-approx
#'
#' @examples
#'
#' \dontrun{
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' approx <- file.path(root, "approx.rds")
#' trait_data <- read_individuals(root, "traits", ncols = 3)
#' trait_plot <- plot_traits(trait_data)
#' trait_plot + add_approx(approx)
#'
#' }
#'
#' @export

# Function to add deterministic approximation lines
add_approx <- function(path) {

  # Load the deterministic simulation data
  approx_data <- readRDS(path)

  # Reformat the dataset
  approx_data <- approx_data %>%
    tidyr::pivot_longer(c(x1, x2), names_to = "ecotype") %>%
    dplyr::mutate(trait = "Ecological")

  # Add lines corresponding to the deterministic simulation
  ggplot2::geom_line(
    data = approx_data,
    mapping = ggplot2::aes(x = time / 1000, y = value, group = ecotype)
  )

}
