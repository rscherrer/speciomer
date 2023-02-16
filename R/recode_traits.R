#' Recode traits
#'
#' Change the names of the traits in a vector from 1, 2 and 3 to "Ecological",
#' "Mating" and "Neutral", respectively.
#'
#' @param x A vector of trait numbers
#'
#' @return A vector of factors
#'
#' @seealso \code{forcats::fct_recode}
#'
#' @examples
#'
#' recode_traits(1:3)
#'
#' @export

# Function to recode traits
recode_traits <- function(x) {

  forcats::fct_recode(as.character(x), !!!trait_numbers())

}
