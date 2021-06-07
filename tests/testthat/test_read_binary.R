test_that("Read binary data into a vector", {

  data <- read_binary("../../inst/extdata/sim-example/trait_Fst.dat")
  expect_true(length(data) > 0)
  expect_true(is.numeric(data))

})
