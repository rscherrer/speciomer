#' Trait numbers
#'
#' Generates a vector of trait numbers (1, 2 and 3) named after their trait
#' names ("Ecological", "Mating" and "Neutral", respectively).
#'
#' @return A vector of strings
#'
#' @examples
#'
#' trait_numbers()
#'
#' @export

# Function to generate a vector of numbers named after the traits
trait_numbers <- function() {

  x <- as.character(1:3)
  names(x) <- trait_names()
  return(x)

}
