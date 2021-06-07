test_that("Rearrange data into multiple columns", {

  data <- as_tibble_speciome(1:10, ncol = 5)
  expect_true("tbl" %in% class(data))
  expect_equal(nrow(data), 2)
  expect_equal(ncol(data), 5)

})

test_that("Rearrange data into one duplicated column", {

  data <- as_tibble_speciome(1:10, ncol = -2)
  expect_equal(nrow(data), 20)
  expect_equal(ncol(data), 1)

})
