test_that("Interpret locus-wise variable names", {

  expect_equal(interpret_variable_names(c("Fst", "Qst"), "locus"), c("locus_Fst", "locus_Qst"))
  expect_equal(interpret_variable_names(c("Fst", "ecotype_freq"), "locus"), c("locus_Fst", "locus_ecotype_freq"))

})

test_that("Interpret edge-wise variable names", {

  expect_equal(interpret_variable_names(c("corbreed", "corfreq"), "edge"), c("edge_corbreed", "edge_corfreq"))

})

test_that("Interpret individual-wise variable names", {

  expect_equal(interpret_variable_names(c("traits", "ecotypes"), "individual"), c("individual_traits", "individual_ecotypes"))

})

test_that("Interpret trait-wise variable names", {

  expect_equal(interpret_variable_names("Fst", "trait"), "trait_Fst")
  expect_equal(interpret_variable_names("EI", "trait"), "EI")

})
