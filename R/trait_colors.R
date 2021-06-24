#' Trait colors
#'
#' Generates a vector of trait colors ("forestgreen", "goldenrod" and
#' "darkgray") named after their trait names ("Ecological", "Mating" and
#' "Neutral", respectively).
#'
#' @return A vector of strings
#'
#' @examples
#'
#' trait_colors()
#'
#' @export

# Function to generate a vector of trait colors
trait_colors <- function() {

  x <- c("forestgreen", "goldenrod", "darkgray")
  names(x) <- trait_names()
  return(x)

}
