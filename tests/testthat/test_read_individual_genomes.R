root <- "../../inst/extdata/sim-indiv-genomes"

test_that("Read individual genomes", {

  data <- read_individual_genomes(root)
  expect_true("haplotype_1" %in% colnames(data))
  population_sizes <- read_binary(paste0(root, "/population_sizes.dat"))
  nloci <- sum(read_parameters(root)[["nvertices"]])
  expect_equal(nrow(data), sum(population_sizes) *  nloci)

})

test_that("Read individual genomes with individual metadata", {

  variables <- c("individual_traits", "individual_ecotypes")
  ncols <- c(3, 1)
  data <- read_individual_genomes(root, variables, ncols)
  expect_true("trait1" %in% colnames(data))

})

test_that("Read individual genomes with locus metadata", {

  data <- read_individual_genomes(root, locus_variables = "locus_Fst")
  expect_true("Fst" %in% colnames(data))

})

test_that("Read individual genomes with genetic architecture only", {

  data <- read_individual_genomes(root, locus_architecture = TRUE)
  expect_true("effect" %in% colnames(data))

})
