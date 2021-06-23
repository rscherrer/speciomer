# Function to plot the traits encoded by each locus
plot_barcode <- function(arch, xaxis = "location", size = 0.1) {

  if (xaxis == "location") arch <- arch %>% mutate(locus = location)

  # Barcode plot
  arch %>%
    select(locus, trait) %>%
    ggplot(aes(x = locus, xend = locus, color = trait)) +
    geom_segment(y = 0, yend = 1, size = size) +
    scale_color_manual(values = trait_colors()) +
    theme_void() +
    labs(color = "Trait")

}
