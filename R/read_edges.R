#' Read edge-wise data
#'
#' Wrapper around \code{read_speciome} for variables with one value per
#' edge per time point.
#'
#' @param root Path to the simulation folder
#' @param variables Vector of names of variable to read (will be interpreted using \code{interpret_variable_names})
#' @param architecture Whether to append edge-wise genetic architecture parameters
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
#' read_edges(root, "edge_corbreed")
#'
#' @export

read_edges <- function(root, variables, architecture = TRUE) {

  # Add an edge prefix to the variable names if needed
  variables <- interpret_variable_names(variables, type = "edge")

  # Count the number of edges
  parameters <- read_parameters(root)
  nedges <- parameters[["nedges"]]
  nedges <- sum(nedges)

  # Parametrization
  ncols <- rep(1, length(variables))
  variables <- c("time", variables)
  ncols <- c(-nedges, ncols)

  # Read edge-wise data
  data <- read_speciome(root, variables, ncols = ncols)

  # Remove the prefix "edge", now redundant
  colnames(data) <- stringr::str_remove(colnames(data), "edge_")

  # Add an edge identifier
  data <- data %>%
    tibble::add_column(edge = 1, .after = "time") %>%
    dplyr::group_by(time) %>%
    dplyr::mutate(edge = seq(dplyr::n())) %>%
    dplyr::ungroup()

  if (!architecture) return(data)

  # Read the genetic architecture
  arch <- read_architecture(root)[["edges"]]

  # Append it to each time point
  data <- data %>% dplyr::left_join(arch)

  return(data)

}
