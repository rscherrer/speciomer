#' Read genetic architecture
#'
#' Read the architecture file in a simulation folder and returns a pair of
#' tibbles, one for the loci and one for the edges.
#'
#' @param root Path to the simulation folder
#'
#' @details The function looks for the file "architecture.txt" in the simulation folder
#'
#' @return A named list of two tibbles.
#'
#' @examples
#'
#' root <- system.file("extdata", "sim-example", package = "speciomer")
#' read_architecture(root)
#'
#' @export

read_architecture <- function(root) {

  # Architecture file name
  arch_file_name <- paste0(root, "/architecture.txt")

  # Read the lines from the file
  arch <- readLines(arch_file_name)

  # Find where the architecture starts
  begin <- grep("^--architecture--$", arch)

  # Was the start found?
  if (length(begin) == 0) stop("cannot find start in architecture.txt")

  # Keep only the relevant part
  begin <- begin + 1
  arch <- arch[begin:length(arch)]

  # Split each line into name and values
  arch <- stringr::str_split(arch, " ")

  # Extract field names
  arch_names <- purrr::map_chr(arch, dplyr::first)

  # The second element of each edge-wise field refers to the trait
  is_network_field <- arch_names %in% c("from", "to", "weights")
  traits <- purrr::map_chr(arch[is_network_field], ~ .x[2])

  # Combine them
  arch_names[is_network_field] <- purrr::map2_chr(arch_names[is_network_field], traits, paste, sep = "_")

  # Extract values as numeric (remove the occasional empty string)
  arch <- purrr::map(arch, ~ as.numeric(.x[.x != ""][-1]))

  # Remove the trait index from the edge-wise parameters
  arch[is_network_field] <- purrr::map(arch[is_network_field], ~ .x[-1])

  # Name the fields
  names(arch) <- arch_names

  # Store locus-wise fields in a tibble
  nodes <- with(arch, tibble::tibble(
    locus = seq(length(traits)),
    chromosome = purrr::map_int(locations, ~ which(.x < chromosomes)[1]),
    trait = as.integer(traits + 1),
    location = locations,
    effect = effects,
    dominance = dominances
  ))

  # Keep only edge-wise fields
  arch <- arch[is_network_field]

  # For each trait...
  edges <- purrr::map_dfr(0:2, function(curr_trait) {

    # Wich fields correspond to that trait?
    is_curr_network <- grep(curr_trait, names(arch))

    # Assemble them into a tibble
    curr_network <- purrr::map_dfc(arch[is_curr_network], ~ .x)

    # Rename the columns
    colnames(curr_network) <- c("from", "to", "weight")

    # Correct indexing of loci
    curr_network <- curr_network %>% dplyr::mutate(from = from + 1, to = to + 1)

    # Add a column for the trait
    curr_network <- curr_network %>%
      dplyr::mutate(trait = as.integer(curr_trait + 1))

    return(curr_network)

  })

  # Add edge identifier
  edges <- edges %>%
    tibble::add_column(edge = seq(nrow(edges)), .before = "from")

  # Additional locus-wise data from the network
  nodes_extra <- edges %>%
    tidyr::pivot_longer(cols = c(from, to), values_to = "locus") %>%
    dplyr::group_by(locus) %>%
    dplyr::summarize(
      degree = dplyr::n(),
      max_abs_weight = max(abs(weight))
    )

  # Add them
  nodes <- nodes %>% dplyr::right_join(nodes_extra)

  # Make a list of the two tibbles
  arch <- list(nodes = nodes, edges = edges)

  return(arch)

}

