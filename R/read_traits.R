#' Read trait-wise data
#'
#' Wrapper around \code{read_speciome} for variables with one value per
#' trait per time point.
#'
#' @param root Path to the simulation folder
#' @param variables Vector of names of variable to read (will be interpreted using \code{interpret_variable_names})
#'
#' @return A tibble containing the simulation data
#'
#' @note The function will read "time.dat" as an extra variable, no need to
#' supply it.
#'
#' @seealso \code{read_speciome}
#'
#' @examples
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' read_traits(root, "trait_Fst")
#'
#' @export

read_traits <- function(root, variables) {

  # Add a trait prefix to the variable names if needed
  variables <- interpret_variable_names(variables, type = "trait")

  # Parametrization
  ncols <- rep(1, length(variables))
  variables <- c("time", variables)
  ncols <- c(-3, ncols)

  # Read trait-wise data
  data <- read_speciome(root, variables, ncols = ncols)

  # Remove the prefix "trait", now redundant
  colnames(data) <- stringr::str_remove(colnames(data), "trait_")

  # Add a trait identifier
  data <- data %>%
    tibble::add_column(trait = rep(seq(3), nrow(data) / 3), .after = "time")

  return(data)

}
