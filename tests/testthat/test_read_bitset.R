root <- system.file("extdata", "sim-indiv-genomes", package = "speciomer")

test_that("Read a bitset", {

  data <- read_bitset(paste0(root, "/individual_whole_genomes.dat"))
  expect_true("integer" %in% class(data))
  expect_true(all(data %in% c(0, 1)))

})
