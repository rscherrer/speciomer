root <- system.file("extdata", "sim-example", package = "speciomer")

test_that("Read edge-wise data", {

  data <- read_edges(root, "edge_corbreed", architecture = FALSE)
  expect_equal(ncol(data), 3)
  nedges <- sum(read_parameters(root)[["nedges"]])
  ntimes <- length(read_binary(paste0(root, "/time.dat")))
  expect_equal(nrow(data), nedges * ntimes)

})

test_that("Read edge-wise data with genetic architecture", {

  data <- suppressMessages(read_edges(root, "corbreed", architecture = TRUE))
  expect_true("trait" %in% colnames(data))
  expect_true(!any(is.na(data$trait)))
  nedges <- sum(read_parameters(root)[["nedges"]])
  ntimes <- length(read_binary(paste0(root, "/time.dat")))
  expect_equal(nrow(data), nedges * ntimes)

})
