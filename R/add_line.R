#' Add a line to a ggplot
#'
#' @param data A data frame
#' @param y The name of the variable to plot as a line (as a string)
#'
#' @details This function maps variable \code{y} to the \code{y} aesthetic of a
#' \code{geom_line} and adds it to a preexisting ggplot.
#'
#' @note \code{y} cannot be "y".
#'
#' @examples
#'
#' data <- tibble::tibble(x = 1:3, z = 3:1)
#'
#' ggplot2::ggplot(data, ggplot2::aes(x = x, y = x)) +
#'   ggplot2::geom_line() +
#'   add_line(data, "z")
#'
#' @export

# Function to add a line
add_line <- function(data, y) {

  ggplot2::geom_line(data = data, mapping = ggplot2::aes(y = get(y)))

}


