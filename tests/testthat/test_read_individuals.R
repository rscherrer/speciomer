root <- "../../inst/extdata/sim-example"

test_that("Read individual-wise data", {

  data <- read_individuals(root, c("individual_ecotypes"))
  population_sizes <- read_binary(paste0(root, "/population_sizes.dat"))
  expect_equal(nrow(data), sum(population_sizes))
  expect_equal(ncol(data), 2)

})

test_that("Read split individual-wise data", {

  variables <- paste0("individual_", c("traits", "ecotypes"))
  data <- read_individuals(root, variables, ncols = c(3, 1))
  population_sizes <- read_binary(paste0(root, "/population_sizes.dat"))
  expect_equal(nrow(data), sum(population_sizes))
  expect_equal(ncol(data), 5)

})

