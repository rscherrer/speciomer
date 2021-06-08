root <- system.file("extdata", "sim-example", package = "speciomer")

test_that("Read genetic architecture", {

  arch <- suppressMessages(read_architecture(root))
  expect_true(is.list(arch))
  expect_equal(names(arch), c("nodes", "edges"))
  expect_true(all(purrr::map_lgl(arch, tibble::is_tibble)))

})

test_that("Locus-wise achitecture has details from the network", {

  arch <- suppressMessages(read_architecture(root))
  expect_true("degree" %in% names(arch[["nodes"]]))

})
