test_that("Remove plurals in column names", {

  colnames <- rm_plural_colnames(c("traits1", "traits2", "traits3", "ecotypes"))
  expect_equal(colnames, c("trait1", "trait2", "trait3", "ecotype"))

})
