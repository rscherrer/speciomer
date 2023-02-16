root <- system.file("extdata", "sim-example", package = "speciomer")

test_that("Read trait-wise data", {

  data <- read_traits(root, "Cst")
  ntimes <- length(read_binary(paste0(root, "/time.dat")))
  expect_equal(nrow(data), 3 * ntimes)

})
