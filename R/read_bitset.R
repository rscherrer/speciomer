#' Read a bitset
#'
#' Reads a binary file bit-wise and returns a vector of binary integers (0 or 1)
#'
#' @param filename Path to the file to read
#'
#' @return A vector of integers
#'
#' @examples
#'
#' root <- system.file("extdata", "sim-indiv-genomes", package = "speciomer")
#' read_bitset(paste0(root, "/individual_whole_genomes.dat"))
#'
#' @export

read_bitset <- function(filename) {

  f <- file(filename, "rb")
  n <- file.size(filename)
  bytes <- readBin(f, raw(), n)
  close(f)
  return(as.integer(rawToBits(bytes)))

}
