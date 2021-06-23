# Function to plot the density of a genetic variable
plot_density <- function(data, variable, time, bins = 20) {

  curr_time <- time

  data %>%
    filter(time == curr_time) %>%
    ggplot(aes(y = get(variable), fill = trait)) +
    geom_histogram(alpha = 0.5, position = "identity", bins = bins) +
    scale_fill_manual(values = trait_colors()) +
    theme(legend.position = "none") +
    ylab(variable) +
    xlab("Count") +
    facet_grid(. ~ trait)

}
