root <- "../../inst/extdata/sim-example"

test_that("Read edge-wise data", {

  data <- read_edges(root, "edge_corbreed", architecture = FALSE)
  expect_equal(ncol(data), 2)
  nedges <- sum(read_parameters(root)[["nedges"]])
  ntimes <- length(read_binary(paste0(root, "/time.dat")))
  expect_equal(nrow(data), nedges * ntimes)

})

test_that("Read edge-wise data with genetic architecture", {

  data <- read_edges(root, "edge_corbreed", architecture = TRUE)
  expect_true("trait" %in% colnames(data))

})
