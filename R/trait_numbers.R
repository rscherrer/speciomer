# Function to generate a vector of numbers named after the traits
trait_numbers <- function() {

  x <- as.character(1:3)
  names(x) <- trait_names()
  return(x)

}
