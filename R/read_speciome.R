# Function to read the speciome simulation data
read_speciome <- function(
  root,
  variables,
  by = 1,
  dupl = 1,
  parnames = NULL,
  combine = FALSE,
  as_numeric = NULL,
  parfile = "paramlog.txt"
) {

  # For each variable...
  data <- purrr::pmap_dfc(
    list(variables, by, dupl),
    function(variable, by, dupl) {

      # Name of the file containing the relevant variable
      data_file_name <- paste0(root, "/", variable, ".dat")

      # Read in the variable as a vector
      data <- read_binary(data_file_name)

      tibble(data)


      # Row index for each entry of this variable
      rows <- rep(seq(length(data) / by), each = by)

      # Split the vector into rows
      data <- split(data, rows)

      # Assemble the rows into a matrix
      data <- do.call("rbind", data)



      do.call("rbind", .)

      data <- read_binary(data_file_name) %>%
        split(rep(seq(length(.) / by), each = by)) %>%
        do.call("rbind", .) %>%
        data.frame

      if (ncol(data) > 1) {
        colnames <- paste0(variable, seq(ncol(data)))
      } else {
        colnames <- variable
      }
      if (is.character(dupl)) {
        dupl <- read_binary(paste0(folder, "/", dupl, ".dat"))
      }
      if (length(dupl) == 1) dupl <- rep(dupl, nrow(data))
      data <- data.frame(data[mrep(seq(nrow(data)), n = dupl), ])
      data <- data %>% rename_str(colnames)

    }
  )


  if (!is.null(parnames)) {

    pars <- read_param(
      folder, parnames, combine = combine, flatten = TRUE,
      as_numeric = as_numeric, filename = parfile
    )
    pars <- pars %>% purrr::map_dfc(rep, nrow(data))
    data <- dplyr::bind_cols(data, pars)

  }

  return (tibble::tibble(data))

}


library(speciomer)

read_speciome(root, variables = c("time", "Fst", "Qst", "Cst"), dupl = c(3, 1, 1, 1))

root <- "../speciome-data-D1/sim_default"
variables <- c("time", "Fst", "Qst", "Cst")

# Paths to the data files
data_file_names <- paste0(root, "/", variables, ".dat")

# Read in the variables as vectors
data <- purrr::map(data_file_names, read_binary)
names(data) <- variables

# Function to turn a vector of values into a matrix
vec_to_tibble <- function(x, ncol = 1) {

  dupl <- 1

  # If each entry must be duplicated then only one column will be made
  if (ncol < 0) {
    dupl <- abs(ncol)
    ncol <- 1
  }

  # Assemble
  x <- matrix(x, ncol = ncol, nrow = dupl * length(x) / ncol, byrow = TRUE)
  colnames(x) <- paste0("V", seq(ncol(x)))

  return(tibble::as_tibble(x))

}


ncols <- c(1, 3, 3, 3)
data <- purrr::map2_dfc(data, ncols, vec_to_tibble)

data
nreps <- ncols
nreps[nreps < 0] <- 1
newnames <- do.call("c", purrr::map2(variables, nreps, ~ rep(.x, each = .y)))
names(data) <- newnames

data
