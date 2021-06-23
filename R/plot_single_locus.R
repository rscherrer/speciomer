# Function to plot a single locus through time against the rest
plot_single_locus <- function(data, locus) {

  curr_locus <- locus

  # What trait does the locus code for?
  curr_trait <- data$trait[first(which(data$locus == locus))]

  # Pick a color
  color <- trait_colors()[curr_trait]

  # Make the plots
  data %>%
    filter(trait == curr_trait) %>%
    pivot_longer(c(Fst, alpha, freq, Qst, Cst), names_to = "statistic") %>%
    group_by(time, statistic) %>%
    summarize(
      low = quantile(value, 0.025),
      high = quantile(value, 0.975),
      value = value[locus == curr_locus]
    ) %>%
    mutate(
      statistic = factor(statistic, levels = c("freq", "Fst", "alpha", "Qst", "Cst")),
      statistic = fct_recode(
        statistic, "p" = "freq", "F[ST]" = "Fst", "alpha" = "alpha",
        "Q[ST]" = "Qst", "C[ST]" = "Cst"
      )
    ) %>%
    ggplot(aes(x = time / 1000, y = value)) +
    geom_ribbon(
      mapping = aes(xmax = time / 1000, ymin = low, ymax = high),
      fill = color, alpha = 0.3
    ) +
    geom_line() +
    geom_vline(xintercept = 0, linetype = 4) +
    facet_wrap(. ~ statistic, scales = "free_y", labeller = label_parsed) +
    xlab(parse(text = "'Time ('*10^3~'generations)'")) +
    ylab(NULL)

}
