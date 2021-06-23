# Function to plot loci through time
plot_loci_tt <- function(data, variable, burnin_bar = TRUE) {

  plot <- data %>%
    ggplot(aes(x = time / 1000)) +
    geom_line(
      mapping = aes(group = locus, y = get(variable), color = trait), alpha = 0.3
    ) +
    facet_grid(. ~ trait) +
    xlab(parse(text = "'Time ('*10^3~'generations)'")) +
    ylab(parse(text = variable)) +
    scale_color_manual(values = trait_colors()) +
    guides(color = FALSE, linetype = FALSE)

  if (burnin_bar) plot <- plot + geom_vline(xintercept = 0, linetype = 4)

  return(plot)

}
