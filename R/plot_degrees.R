# Function to plot gene degrees
plot_degrees <- function(arch, xaxis = "location", rm_x = TRUE) {

  if (xaxis == "location") arch <- arch %>% mutate(locus = location)

  # Plot degrees
  plot <- arch %>%
    ggplot() +
    geom_segment(
      aes(x = locus, xend = locus, yend = degree),
      y = 0,
      color = "firebrick"
    ) +
    theme(
      axis.title.x = element_blank(), axis.ticks.x = element_blank(),
      axis.line.x = element_blank(), axis.text.x = element_blank()
    ) +
    ylab("Degree")

  if (rm_x) plot <- plot + rm_axis("x")

  return(plot)

}
