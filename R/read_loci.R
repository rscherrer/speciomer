#' Read locus-wise data
#'
#' Wrapper around \code{read_speciome} for variables with one value per
#' locus per time point.
#'
#' @param root Path to the simulation folder
#' @param variables Vector of names of variable to read (will be interpreted using \code{interpret_variable_names})
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
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' read_loci(root, "locus_Fst")
#'
#' @export

read_loci <- function(root, variables, architecture = TRUE) {

  .data <- NULL # hack for check to pass

  # Add a locus prefix to the variable names if needed
  variables <- interpret_variable_names(variables, type = "locus")

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

  # Add a locus identifier
  data <- data %>%
    tibble::add_column(locus = 1, .after = "time") %>%
    dplyr::group_by(.data$time) %>%
    dplyr::mutate(locus = seq(dplyr::n())) %>%
    dplyr::ungroup()

  if (!architecture) return(data)

  # Read the genetic architecture
  arch <- read_architecture(root)[["nodes"]]

  # Append it to each time point
  data <- data %>% dplyr::left_join(arch)

  return(data)

}
