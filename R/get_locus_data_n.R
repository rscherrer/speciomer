#' Extract data for one locus (nested)
#'
#' Same as \code{get_locus_data} but extracting from a data frame nested by
#' locus (which is much faster).
#'
#' @param i Index of the locus to extract data for
#' @param data_n Locus-wise data frame trough time (see \code{?read_loci}),
#' nested by locus (use \code{dplyr::group_by} and \code{tidyr::nest} to get
#' that)
#' @param variables String vector with the names of the columns to extract
#' @param add_time Whether or not to extract the "time" column as well
#'
#' @return A tibble
#'
#' @seealso \code{read_loci}, \code{dplyr::group_by}, \code{tidyr::nest}
#'
#' @examples
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' data <- read_loci(root, "Fst")
#' data <- data %>% dplyr::group_by(locus) %>% tidyr::nest()
#' get_locus_data_n(1, data, "Fst")
#'
#' @export

# Function to extract a tibble containing the data from a specific locus
get_locus_data_n <- function(i, data_n, variables, add_time = FALSE) {

  if (add_time) variables <- c("time", variables)
  with(data_n, data[locus == i])[[1]][variables]

}
