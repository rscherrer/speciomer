#' Interpret variable names
#'
#' Adds labels to variable names that are provided without labels
#'
#' @param x Vector of names of variables
#' @param type The label to add (either of "locus", "edge", "individual" or "trait")
#'
#' @details The function adds the specified label to the variable names. For example,
#' "Fst" becomes "locus_Fst" if \code{type = "locus"}. It will not add a label
#' if it detects that the label is already there, i.e. "locus_Fst" will stay "locus_Fst".
#'
#' @return A vector of strings
#'
#' @note In the case of \code{type = "trait"} some variable names will be considered
#' exceptions to which not to add a label. These are the variables that are saved
#' per time point and/or per ecotype or habitat but not per trait
#' ("EI", "RI", "SI", "population_sizes", "ecotype_population_sizes" and "habitat_resources")
#'
#' @examples
#'
#' interpret_variable_names(c("Fst", "Qst"), "locus")
#' interpret_variable_names(c("corbreed", "corfreq"), "edge")
#'
#' @export

interpret_variable_names <- function(x, type) {

  if (type == "locus") return(interpret_variable_names_locus(x))
  if (type == "edge") return(interpret_variable_names_edge(x))
  if (type == "individual") return(interpret_variable_names_individual(x))
  if (type == "trait") return(interpret_variable_names_trait(x))

}

interpret_variable_names_locus <- function(x) {

  # Add locus label to the rest that do not have it
  is_locus <- stringr::str_detect(x, "locus_")
  x[!is_locus] <- paste0("locus_", x[!is_locus])

  return(x)

}

interpret_variable_names_edge <- function(x) {

  # Add edge label to the variables that do not have it
  is_edge <- stringr::str_detect(x, "edge_")
  x[!is_edge] <- paste0("edge_", x[!is_edge])

  return(x)

}

interpret_variable_names_individual <- function(x) {

  # Add individual label to the variables that do not have it
  is_individual <- stringr::str_detect(x, "individual_")
  x[!is_individual] <- paste0("individual_", x[!is_individual])

  return(x)

}

interpret_variable_names_trait <- function(x) {

  # Add trait label to the variables that do not have it
  is_trait <- stringr::str_detect(x, "trait_")

  # Except for these exceptions
  exceptions <- c(
    "EI", "RI", "SI", "population_sizes", "ecotype_population_sizes",
    "habitat_resources"
  )

  is_trait[x %in% exceptions] <- TRUE
  x[!is_trait] <- paste0("trait_", x[!is_trait])

  return(x)

}
