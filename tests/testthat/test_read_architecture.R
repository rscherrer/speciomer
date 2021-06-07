root <- "../../inst/extdata/sim-example"

test_that("Read genetic architecture", {

  arch <- read_architecture(root)
  expect_true(is.list(arch))
  expect_equal(names(arch), c("nodes", "edges"))
  expect_true(all(purrr::map_lgl(arch, tibble::is_tibble)))

})
