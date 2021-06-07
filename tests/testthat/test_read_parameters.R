root <- "../../inst/extdata/sim-example"

test_that("", {

  pars <- read_parameters(root)
  expect_true(is.list(pars))
  expect_true("hsymmetry" %in% names(pars))

})
