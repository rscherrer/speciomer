# Function to plot a gene network
plot_network <- function(arch, trait, xaxis = "location") {

  if (xaxis == "location") arch <- arch %>% mutate(locus = location)

  curr_trait <- trait
  color <- trait_colors()[trait]

  # Plot the network
  arch %>%
    filter(trait == curr_trait) %>%
    ggraph(layout = 'linear', sort.by = locus, use.numeric = TRUE) +
    geom_edge_arc(aes(alpha = abs(weight)), color = color, fold = TRUE) +
    labs(edge_alpha = parse(text = "'|'*omega*'|'")) +
    theme(legend.position = "left")

}
