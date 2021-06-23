# Function to plot gene additive effect sizes
plot_effect_sizes <- function(arch, xaxis = "location", rm_x = TRUE) {

  if (xaxis == "location") arch <- arch %>% mutate(locus = location)

  # Plot effect sizes
  plot <- arch %>%
    ggplot() +
    geom_segment(
      aes(x = locus, xend = locus, yend = abs(effect)),
      y = 0,
      color = "steelblue"
    ) +
    ylab(parse(text = "'|'*eta*'|'"))

  if (rm_x) plot <- plot + rm_axis("x")

  return(plot)

}
