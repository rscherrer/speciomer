#' Plot ecological trait density at different times
#'
#' Plots three portions of the ecological trait density plot: over the full
#' simulation time span, at the very start and around branching time.
#'
#' @param root Path to the simulation folder
#' @param root_init Path to the initialization simulation folder, i.e. a
#' simulation that is the equivalent of \code{root}, but simulated only for
#' a few thousand generations and with a high recording resolution. This
#' allows us to see what is happening in detail in the early steps.
#' @param add_rect Whether to add a shading rectangle on the left facet to
#' indicate what part of the plot is zoomed in, in the right facet.
#'
#' @details Because the burn-in in \code{root_init} is not as long as in
#' \code{root}, the starting time is modify so the two simulations match
#' each other (the two simulations should be identical because they are
#' simulated with the same seed).
#'
#' @return A ggplot
#'
#' @seealso \code{read_individuals}
#'
#' @export

# Function to plot different portions of the ecological trait density plot
plot_eco_trait_zoomed <- function(root, root_init, add_rect = FALSE) {

  .data <- NULL # hack for check to pass

  # Read trait data from the main simulation
  data <- read_individuals(root, "traits", ncols = 3)

  # Read trait data from the high-resolution equivalent (run only for a short time)
  data_init <- read_individuals(root_init, "traits", ncols = 3)

  # Assemble the main data and the zoom on initialization
  data <- purrr::map_dfr(list(data, data_init), ~ .x, .id = "zoom")

  # Rename the different zooming levels
  zoom_numbers <- as.character(1:2)
  zoom_names <- c("-20,000 to 100,000", "-20,000 to -17,000")
  names(zoom_numbers) <- zoom_names

  # Do not forget to update their order so they are plotted the right way
  data <- data %>% dplyr::mutate(
    zoom = forcats::fct_recode(.data$zoom, !!!zoom_numbers),
    zoom = factor(.data$zoom, levels = zoom_names)
  )

  # Create a small data frame with intercept values for the vertical lines at zero
  vline_data <- tibble::tibble(
    zoom = factor(zoom_names, levels = zoom_names), # to keep the order
    xintercept = c(0, NA)
  )

  # Plot trait densities through time
  plot <- data %>%
    ggplot2::ggplot()

  if (add_rect) {

    # Prepare a shading rectangle
    rect_data <- tibble::tibble(
      xmin = -20,
      xmax = -17,
      ymin = min(data$trait1),
      ymax = max(data$trait1),
      zoom = factor(zoom_names, levels = zoom_names)
    )

    # Add rectangle shade
    plot <- plot + ggplot2::geom_rect(
      data = rect_data,
      mapping = ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax
      ),
      color = NA,
      fill = "gray90"
    )

  }

  # Finish the plot
  plot <- plot +
    ggplot2::geom_bin2d(
      mapping = ggplot2::aes(x = .data$time / 1000, y = .data$trait1),
      bins = 100
    ) +
    ggplot2::facet_grid(. ~ zoom, scales = "free_x") +
    ggplot2::xlab(parse(text = "'Time ('*10^3~'generations)'")) +
    ggplot2::ylab("Ecological trait") +
    ggplot2::labs(fill = "Count") +
    ggplot2::scale_fill_continuous(type = "viridis") +
    ggplot2::geom_vline(
      data = vline_data,
      mapping = ggplot2::aes(xintercept = .data$xintercept),
      linetype = 4
    )

  return(plot)

}
