#' Read a binary file
#'
#' @param filename Path to the file
#' @param nbytes The number of bytes constituting one value (e.g. 8 bytes
#' for a double)
#'
#' @return A numeric vector
#'
#' @examples
#'
#' read_binary("inst/extdata/sim-example/time.dat")
#'
#' @export

read_binary <- function(filename, nbytes = 8) {

  # Number of values to read
  n <- file.size(filename) / nbytes

  file <- file(filename, "rb")
  data <- readBin(file, numeric(), n)
  close(file)

  return (data)

}
