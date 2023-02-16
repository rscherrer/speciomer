#' Save many ggplots
#'
#' Loops through ggplots and their respective figure names and saves them.
#'
#' @param plots A list of ggplot objects
#' @param names A vector of file names (where to save each plot)
#' @param width,height,dpi Single values to be passed to \code{ggsave}
#'
#' @note The same \code{width}, \code{height} and \code{dpi} is used for all
#' the \code{plots}
#'
#' @seealso \code{ggplot2::ggsave}
#'
#' @examples
#'
#' \dontrun{
#'
#' plot1 <- ggplot2::quickplot()
#' plot2 <- plot1
#' ggsave_all(list(plot1, plot2), c("p1.png", "p2.png"), width = 3, height = 3)
#'
#' }
#'
#' @export

# Function to save multiple ggplots
ggsave_all <- function(plots, names, width, height, dpi = 300) {

  # Loop through the plots and save each one of them
  for (i in seq_along(plots)) {

    print(sprintf("Saving %s...", names[i]))
    ggplot2::ggsave(
      names[i], plots[[i]], width = width, height = height, dpi = dpi
    )

  }

}
