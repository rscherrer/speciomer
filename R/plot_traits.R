# Function to plot individual traits through time
plot_traits <- function(data, burnin_bar = TRUE) {

  plot <- data %>%
    pivot_longer(trait1:trait3, names_to = "trait") %>%
    mutate(trait = str_remove(trait, "trait")) %>%
    mutate(trait = recode_traits(trait)) %>%
    ggplot(aes(x = time / 1000, y = value)) +
    geom_bin2d(bins = 100) +
    facet_grid(. ~ trait) +
    scale_fill_continuous(type = "viridis") +
    xlab(parse(text = "'Time ('*10^3~'generations)'")) +
    ylab("Trait value") +
    labs(fill = "Count")

  if (burnin_bar) plot  <- plot + geom_vline(xintercept = 0, linetype = 4)

  return(plot)

}
