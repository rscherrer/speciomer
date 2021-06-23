# Function to recode traits
recode_traits <- function(x) {

  forcats::fct_recode(as.character(x), !!!trait_numbers())

}
