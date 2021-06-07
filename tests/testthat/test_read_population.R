root <- "../../inst/extdata/sim-example"

test_that("Read population-wide data", {

  data <- read_population(root, c("EI", "RI", "SI"))
  expect_equal(ncol(data), 4)
  expect_true("time" %in% colnames(data))

})

test_that("Read split population-wide data", {

  data <- read_population(root, c("EI", "trait_Fst"), ncols = c(1, 3))
  expect_equal(ncol(data), 5)

})
