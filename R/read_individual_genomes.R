#' Read individual whole genome data
#'
#' Reads individual genome data and assembles them into a tibble.
#'
#' @param root Path to the simulation folder
#' @param individual_variables,individual_ncols Variables to be passed to \code{read_individuals}
#' @param locus_variables,locus_architecture Variables to be passed to \code{read_loci}
#'
#' @details The files "individual_whole_genomes.dat" and "individual_locus_genvalues.dat"
#' are read and assembled into an individual-and-locus-wise tibble. The \code{individual_*}
#' and \code{locus_*} arguments allow to read extra individual-wise or locus-wise
#' variables, respectively, and append them to the tibble as metadata. Then, these
#' arguments are passed down to the functions \code{read_individuals} and
#' \code{read_loci}, respectively, as \code{variables}, \code{ncols} and \code{architecture}.
#' See \code{?read_individuals} and \code{?read_loci} for details.
#'
#' @return A tibble containing the simulation data
#'
#' @seealso \code{read_individuals}, \code{read_loci}, \code{read_architecture}
#'
#' @examples
#'
#' root <- system.file("extdata", "sim-indiv-genomes", package = "speciomer")
#' read_individual_genomes(root)
#' read_individual_genomes(root, "individual_ecotypes")
#' read_individual_genomes(root, locus_variables = "locus_Fst")
#'
#' @export

read_individual_genomes <- function(

  root, individual_variables = NULL, individual_ncols = NULL,
  locus_variables = NULL, locus_architecture = FALSE

) {

  .data <- NULL # hack for check to pass

  # Path to the data file
  file_name <- paste0(root, "/individual_whole_genomes.dat")

  # Read genomes as a long bitset
  data <- read_bitset(file_name)

  # Count the number of loci
  nloci <- sum(read_parameters(root)[["nvertices"]])

  # Number of zeros added at the end of each individual genome
  nextra <- 64 - ((2 * nloci) %% 64)

  # Number of bits per individual genome
  nbits <- (2 * nloci) + nextra

  # Number of individuals
  ninds <- length(data) / nbits

  # Make an allele-wise tibble
  data <- tibble::tibble(
    allele = data,
    individual = rep(seq(ninds), each = nbits),
    locus = rep(c(rep(seq(nloci), 2), rep(NA, nextra)), ninds),
    haplotype = rep(c(rep(seq(2), each = nloci), rep(NA, nextra)), ninds)
  )

  # Remove the extra zeros (added to convert individual genomes into 64bit int)
  data <- data %>% tidyr::drop_na()

  # Convert to the locus-wise wide format
  data <- data %>%
    dplyr::mutate(haplotype = stringr::str_replace(.data$haplotype, "^", "haplotype")) %>%
    tidyr::pivot_wider(names_from = "haplotype", values_from = "allele")

  # Read population sizes
  population_sizes <- read_binary(paste0(root, "/population_sizes.dat"))

  # Read time points
  times <- read_binary(paste0(root, "/time.dat"))

  # Repeat each time point for each individual in this time point
  times <- purrr::reduce(purrr::map2(times, population_sizes, ~ rep(.x, .y)), c)

  # Repeat each time point for each locus in each individual
  times <- rep(times, each = nloci)

  # Add time points to the data
  data <- data %>% tibble::add_column(time = times, .before = "individual")

  # Derive allele counts (genotype) from the alleles
  data <- data %>% dplyr::mutate(genotype = .data$haplotype1 + .data$haplotype2)

  # Add genetic values
  genetic_values <- read_binary(paste0(root, "/individual_locus_genvalues.dat"))
  data <- data %>% tibble::add_column(genvalue = genetic_values)

  if (!is.null(individual_variables)) {

    # Add a locus prefix to the variable names if needed
    individual_variables <- interpret_variable_names(
      individual_variables, type = "individual"
    )

    # Read extra individual variables
    individual_data <- read_individuals(
      root, individual_variables, individual_ncols
    )

    # Add them
    data <- data %>% dplyr::right_join(individual_data)

  }

  if (!is.null(locus_variables)) {

    # Add a locus prefix to the variable names if needed
    locus_variables <- interpret_variable_names(locus_variables, type = "locus")

    # Read extra locus variables
    locus_data <- read_loci(root, locus_variables, locus_architecture)

    # Add them
    data <- data %>% dplyr::right_join(locus_data)

  }

  if (is.null(locus_variables) & locus_architecture) {

    # Read only the genetic architecture
    arch <- read_architecture(root)[["nodes"]]

    # Append it
    data <- data %>% dplyr::right_join(arch)

  }

  return(data)

}
