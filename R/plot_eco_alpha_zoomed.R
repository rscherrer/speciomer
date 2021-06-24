#' Plot ecological average mutational effects at different times
#'
#' Plots three portions of the plot of average mutational effects per locus
#' and through time, for ecological loci only: over the full
#' simulation time span, at the very start and around branching time.
#'
#' @param root Path to the simulation folder
#' @param root_init Path to the initialization simulation folder, i.e. a
#' simulation that is the equivalent of \code{root}, but simulated only for
#' a few thousand generations and with a high recording resolution. This
#' allows us to see what is happening in detail in the early steps.
#'
#' @details The logic is the same as for \code{plot_eco_trait_zoomed}
#'
#' @return A ggplot
#'
#' @seealso \code{read_individuals}, \code{plot_eco_trait_zoomed}
#'
#' @export

# Function to plot ecological alpha at different time windows
plot_eco_alpha_zoomed <- function(root, root_init) {

  # Read trait data from the main simulation
  data <- read_loci(root, "alpha")

  # Read trait data from the high-resolution equivalent (run only for a short time)
  data_init <- read_loci(root_init, "alpha")

  # Recode the time points to have them start at -20,000
  data_init <- data_init %>% dplyr::mutate(time = time + -20000 - min(time))

  # Subset the portion of the main data surrounding branching
  data_div <- data %>% dplyr::filter(time >= 0, time <= 25000)

  # Assemble the main data, the zoom on initialization and the zoom on branching
  data <- purrr::map_dfr(list(data, data_init, data_div), ~ .x, .id = "zoom")

  # Keep only the ecological trait
  data <- data %>% dplyr::filter(trait == 1)

  # Rename the different zooming levels
  zoom_numbers <- as.character(1:3)
  zoom_names <- c("-20,000 to 100,000", "-20,000 to -17,000", "0 to 25,000")
  names(zoom_numbers) <- zoom_names

  # Do not forget to update their order so they are plotted the right way
  data <- data %>% dplyr::mutate(
    zoom = forcats::fct_recode(zoom, !!!zoom_numbers),
    zoom = factor(zoom, levels = zoom_names)
  )

  # Create a small data frame with intercept values for the vertical lines at zero
  vline_data <- tibble::tibble(
    zoom = factor(zoom_names, levels = zoom_names), # to keep the order
    xintercept = c(0, NA, 0)
  )

  # Plot trait densities through time
  data %>%
    ggplot2::ggplot(ggplot2::aes(x = time / 1000, y = alpha, group = locus)) +
    ggplot2::geom_line(color = "forestgreen", alpha = 0.3) +
    ggplot2::facet_grid(. ~ zoom, scales = "free_x") +
    ggplot2::xlab(parse(text = "'Time ('*10^3~'generations)'")) +
    ggplot2::ylab(parse(text = "alpha")) +
    ggplot2::geom_vline(
      data = vline_data,
      mapping = ggplot2::aes(xintercept = xintercept),
      linetype = 4
    )

}
