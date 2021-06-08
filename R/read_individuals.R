#' Read individual-wise data
#'
#' Wrapper around \code{read_speciome} for variables with one value per
#' individual per time point.
#'
#' @param root Path to the simulation folder
#' @param variables Vector of names of variable to read
#' @param ncols Vector or list indicating how to split or duplicate each variable
#' when constructing the tibble. See \code{read_speciome}.
#'
#' @details The function will read "population_sizes.dat" in order to know
#' by how many individuals to duplicate each time point, as the number of
#' individuals may change from one generation to the next.
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
#' read_individuals(root, "individual_ecotypes")
#' read_individuals(root, c("individual_traits", "individual_ecotypes"), ncols = c(3, 1))
#'
#' @export

read_individuals <- function(root, variables, ncols = NULL) {

  # Add an individual prefix to the variable names if needed
  variables <- interpret_variable_names(variables, type = "individual")

  if (is.null(ncols)) ncols <- rep(1, length(variables))

  # Add time to the list of variables to read
  variables <- c("time", variables)

  # Make sure time points are repeated as many times as there are individuals
  ncols <- c(1, ncols)
  ncols <- as.list(ncols)
  ncols[[1]] <- read_binary(paste0(root, "/population_sizes.dat"))

  # Read the data
  data <- read_speciome(root, variables, ncols)

  # Remove the prefix "individual" from the column names
  colnames(data) <- stringr::str_remove(colnames(data), "individual_")

  # Add an individual identifier
  data <- data %>% tibble::add_column(individual = seq(nrow(data)), .after = "time")

  # Remove plurals in column names
  colnames(data) <- rm_plural_colnames(colnames(data))

  return(data)

}



