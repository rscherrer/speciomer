#' Extract data for one locus
#'
#' Extracts a tibble with the data for one locus from a locus-wise dataset
#' through time.
#'
#' @param i Index of the locus to extract data for
#' @param data Locus-wise data frame trough time (see \code{?read_loci})
#' @param variables String vector with the names of the columns to extract
#' @param add_time Whether or not to extract the "time" column as well
#'
#' @return A tibble
#'
#' @seealso \code{read_loci}
#'
#' @examples
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' data <- read_loci(root, "Fst")
#' get_locus_data(1, data, "Fst")
#'
#' @export

# Function to extract a tibble containing the data from a specific locus
get_locus_data <- function(i, data, variables, add_time = FALSE) {

  if (add_time) variables <- c("time", variables)
  data[data$locus == i, variables]

}

