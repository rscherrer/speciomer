root <- "../../inst/extdata/sim-example"

test_that("Read one variable into a tibble", {

  data <- read_speciome(root, variables = "time")
  expect_equal(ncol(data), 1)
  expect_true("tbl" %in% class(data))

})

test_that("Read two variables", {

  data <- read_speciome(root, variables = c("time", "EI"))
  expect_equal(ncol(data), 2)

})

test_that("Read two variables and split one", {

  data <- read_speciome(root, variables = c("time", "trait_Fst"), ncols = c(1, 3))
  expect_equal(ncol(data), 4)

})

test_that("Read multiple variables and split some", {

  variables <- c("time", "trait_Fst", "trait_Qst", "trait_Cst")
  data <- read_speciome(root, variables = variables, ncols = c(1, 3, 3, 3))
  expect_equal(ncol(data), 10)

})

test_that("Read multiple variables and duplicate one", {

  variables <- c("time", "trait_Fst", "trait_Qst", "trait_Cst")
  data <- read_speciome(root, variables = variables, ncols = c(-3, 1, 1, 1))
  expect_equal(ncol(data), 4)

})

test_that("Read multiple variables and duplicate several", {

  variables <- c("time", "trait_Fst", "EI", "RI")
  data <- read_speciome(root, variables = variables, ncols = c(-3, 1, -3, -3))
  expect_equal(ncol(data), 4)

})

test_that("Duplicate different elements different numbers of times", {

  n_time_points <- length(read_binary(paste0(root, "/time.dat")))
  ncols <- list(rep(2, n_time_points))
  data <- read_speciome(root, "time", ncols = ncols)
  expect_equal(nrow(data), 2 * n_time_points)

})
