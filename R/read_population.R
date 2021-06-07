#' Read population-wide data
#'
#' Wrapper around \code{read_speciome} for variables with one value per time point.
#'
#' @param root Path to the simulation folder
#' @param variables Vector of names of variable to read
#' @param ncols Vector or list indicating how to split or duplicate each variable
#' when constructing the tibble. See \code{read_speciome}.
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
#' root <- "inst/extdata/sim-example"
#' read_population(root, c("EI", "RI", "SI"))
#' read_population(root, c("EI", "trait_Fst"), ncols = c(1, 3))
#'
#' @export

# Function to read population-wide data
read_population <- function(root, variables, ncols = NULL) {

  if (is.null(ncols)) ncols <- rep(1, length(variables))

  # Append time to the vector of variable names
  variables <- c("time", variables)
  ncols <- c(1, ncols)

  # Read the data
  data <- read_speciome(root, variables, ncols)

  # Remove plurals in column names
  colnames(data) <- stringr::str_remove(colnames(data), "s$")

  return(data)

}
