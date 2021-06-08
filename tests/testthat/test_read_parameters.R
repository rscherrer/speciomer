root <- system.file("extdata", "sim-example", package = "speciomer")

test_that("", {

  pars <- read_parameters(root)
  expect_true(is.list(pars))
  expect_true("hsymmetry" %in% names(pars))

})
