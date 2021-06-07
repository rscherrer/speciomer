#' Turn a data vector into a tibble
#'
#' Reshapes a speciome data vector into a matrix and converts it into a tibble.
#'
#' @param x Vector of data values
#' @param ncol Number of columns by which to split the vector. Use a negative
#' value to duplicate the vector into a longer one-column tibble instead of
#' splitting it.
#'
#' @return A tibble containing the simulation data
#'
#' @seealso `read_speciome`

# Function to turn a vector of values into a tibble
as_tibble_speciome <- function(x, ncol = 1) {

  dupl <- 1

  # If each entry must be duplicated then only one column will be made
  if (ncol < 0) {
    dupl <- abs(ncol)
    ncol <- 1
  }

  # Assemble
  x <- matrix(x, ncol = ncol, nrow = dupl * length(x) / ncol, byrow = TRUE)
  colnames(x) <- paste0("V", seq(ncol(x)))

  return(tibble::as_tibble(x))

}
