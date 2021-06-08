root <- system.file("extdata", "sim-example", package = "speciomer")

test_that("Read locus-wise data", {

  data <- read_loci(root, "locus_Fst", architecture = FALSE)
  expect_equal(ncol(data), 3)
  nloci <- sum(read_parameters(root)[["nvertices"]])
  ntimes <- length(read_binary(paste0(root, "/time.dat")))
  expect_equal(nrow(data), nloci * ntimes)

})

test_that("Read locus-wise data with genetic architecture", {

  data <- suppressMessages(read_loci(root, "Fst", architecture = TRUE))
  expect_true("dominance" %in% colnames(data))
  expect_true(!any(is.na(data$trait)))
  nloci <- sum(read_parameters(root)[["nvertices"]])
  ntimes <- length(read_binary(paste0(root, "/time.dat")))
  expect_equal(nrow(data), nloci * ntimes)

})
