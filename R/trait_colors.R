# Function to generate a vector of trait colors
trait_colors <- function() {

  x <- c("forestgreen", "goldenrod", "darkgray")
  names(x) <- trait_names()
  return(x)

}
