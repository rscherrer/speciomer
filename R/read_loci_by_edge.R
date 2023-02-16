#' Read locus-wise variables on a per-edge basis
#'
#' Reads locus-specific data through time and rearranges the output tibble
#' with one row per edge instead of one row per locus.
#'
#' @param root Path to the simulation folder
#' @param variables What variables to read
#'
#' @details Uses internally \code{read_loci} with \code{architecture = TRUE},
#' then reads the network architecture (the \code{edges} tibble returned by
#' \code{read_architecture}) to reformat.
#'
#' @return A tibble
#'
#' @note Assumes that the genetic architecture file is present.
#'
#' @seealso \code{read_loci}, \code{read_architecture}, \code{get_locus_data_n}
#'
#' @examples
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' read_loci_by_edge(root, "Fst")
#'
#' @export

# Function to read locus-wise data on a per-edge basis
read_loci_by_edge <- function(root, variables) {

  .data <- NULL # hack for check to pass

  # Read locus-wise data, nested by locus
  data <- read_loci(root, variables) %>%
    dplyr::group_by(.data$locus) %>%
    tidyr::nest()

  # Load the network architecture
  arch <- read_architecture(root)$edges

  # Extract data for the two partners of each edge
  arch %>%
    dplyr::mutate(
      from_ = purrr::map(.data$from, get_locus_data_n, data, variables, add_time = TRUE),
      to_ = purrr::map(.data$to, get_locus_data_n, data, variables, add_time = FALSE)
    ) %>%
    tidyr::unnest(c(.data$from_, .data$to_), names_sep = "") %>%
    dplyr::rename(time = "from_time")

}
