#' Read locus-wise data
#'
#' Wrapper around \code{read_speciome} for variables with one value per
#' locus per time point.
#'
#' @param root Path to the simulation folder
#' @param variables Vector of names of variable to read
#' @param architecture Whether to append locus-wise genetic architecture parameters
#' (see \code{?read_architecture}).
#'
#' @details The function will read "paramlog.txt" in order to know
#' by how many loci to duplicate each time point.
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
#' read_loci(root, "locus_Fst")
#'
#' @export

read_loci <- function(root, variables, architecture = TRUE) {

  # Count the number of loci
  parameters <- read_parameters(root)
  nloci <- parameters[["nvertices"]]
  nloci <- sum(nloci)

  # Parametrization
  ncols <- rep(1, length(variables))
  variables <- c("time", variables)
  ncols <- c(-nloci, ncols)

  # Read locus-wise data
  data <- read_speciome(root, variables, ncols = ncols)

  # Remove the prefix "locus", now redundant
  colnames(data) <- stringr::str_remove(colnames(data), "locus_")

  if (!architecture) return(data)

  # Read the genetic architecture
  arch <- read_architecture(root)[["nodes"]]

  # Append it to each time point
  data <- data %>%
    dplyr::group_by(time) %>%
    tidyr::nest() %>%
    dplyr::mutate(arch = purrr::map(time, ~ arch)) %>%
    tidyr::unnest(cols = c(data, arch))

  return(data)

}
