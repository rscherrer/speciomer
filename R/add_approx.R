# Function to add deterministic approximation lines
add_approx <- function(path) {

  # Load the deterministic simulation data
  approx_data <- readRDS(path)

  # Reformat the dataset
  approx_data <- approx_data %>%
    pivot_longer(c(x1, x2), names_to = "ecotype") %>%
    mutate(trait = "Ecological")

  # Add lines corresponding to the deterministic simulationS
  geom_line(
    data = approx_data,
    mapping = aes(x = time / 1000, y = value, group = ecotype)
  )

}
