# Function to plot a genome scan
plot_genome_scan <- function(
  data, variable, time, xaxis = "location", ylab_parsed = NULL, rm_x = TRUE
) {

  curr_time <- time

  if (is.null(ylab_parsed)) ylab_parsed <- paste0("'", variable, "'")
  if (xaxis == "location") data <- data %>% mutate(locus = location)

  # Genome scan plot
  plot <- data %>%
    filter(time == curr_time) %>%
    ggplot(aes(x = locus)) +
    geom_ribbon(
      mapping = aes(
        xmin = locus, xmax = locus, ymax = get(variable), group = chromosome,
        fill = as.character(chromosome)
      ),
      ymin = 0
    ) +
    scale_fill_manual(values = c("gray40", "gray20", "gray40")) +
    theme(legend.position = "none") +
    xlab(ifelse(xaxis == "locus", "Locus", "Location")) +
    ylab(parse(text = ylab_parsed))

  if (rm_x) plot <- plot + rm_axis("x")

  return(plot)

}
