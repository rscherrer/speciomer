#' Read speciome simulation data
#'
#' Combine data from one simulation, saved as binary files, into a single
#' tibble.
#'
#' @param root Path to the simulation folder
#' @param variables Vector of names of variable to read (e.g. c("time", "trait_Fst"))
#' @param ncols Vector of numbers of columns by which to arrange each variable
#' (one integer per variable, e.g. c(1, 3)). Use negative values to duplicate
#' a variable-vector into a longer column instead of splitting that vector into
#' multiple columns (e.g. c(-3, 1)).
#'
#' @details Each variable is read as a vector and reshaped into a tibble. To
#' assemble the tibbles of multiple variables together, those tibbles must have
#' the same number of rows. For example, if the "trait_Fst.dat" file contains one
#' value per trait per time point and there are three traits, then there are three
#' times as many values in "trait_Fst.dat" than in "time.dat" and so the former
#' must be split into three columns to be attached to the latter. Alternatively,
#' the vector of time points could be duplicated three times (\code{ncols = c(-3, 1)}),
#' thus making it a column three times as long as the "time.dat" data, but the
#' same size as the non-split "trait_Fst" column.
#'
#' @return A tibble containing the simulation data
#'
#' @note Do not provide the extension of the data files in \code{variables} (".dat").
#'
#' @seealso `read_sim`, `read_pop`, `read_genome`
#'
#' @examples
#'
#' @export

# Function to read speciome data
read_speciome <- function(root, variables, ncols) {

  # Paths to the data files
  data_file_names <- paste0(root, "/", variables, ".dat")

  # Read in the variables as vectors
  data <- purrr::map(data_file_names, read_binary)

  # Transform the data vectors into tibbles
  data <- suppressMessages(purrr::map2_dfc(data, ncols, as_tibble_speciome))

  # Update column names
  colnames(data) <- index_duplicate(variables, ncols)

  return(data)

}
