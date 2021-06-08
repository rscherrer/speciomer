#' Remove plurals
#'
#' Removes "s" at the end of strings or just before digits at the end of strings
#'
#' @param x A string vector
#'
#' @return A string vector
#'
#' @examples
#'
#' rm_plural_colnames("traits1", "traits2", "traits3", "ecotypes")

rm_plural_colnames <- function(x) {

  # Which strings are numbered
  is_numbered <- stringr::str_detect(x, "s[[:digit:]]+$")

  # For each numbered string...
  if (any(is_numbered)) {

    # Remove the "s" between the singular and the number
    singulars <- stringr::str_remove(x[is_numbered], "s[[:digit:]]+$")
    numbers <- stringr::str_extract(x[is_numbered], "[[:digit:]]+$")
    x[is_numbered] <- purrr::map2_chr(singulars, numbers, paste0)

  }

  # Just remove the "s" at the end for the rest
  x[!is_numbered] <- stringr::str_remove(x[!is_numbered], "s$")

  return(x)

}
