plot_outcome <- function(root, which = "full", approx = NULL, ...) {

  if (which == "full") plot <- plot_outcome_full(root, approx = approx)
  if (which == "simple") plot <- plot_outcome_simple(root, approx = approx)
  if (which == "burnin") plot <- plot_outcome_burnin(root, approx = approx)
  if (which == "init") plot <- plot_outcome_init(root, approx = approx)
  if (which == "custom") plot <- plot_outcome_core(root, approx = approx, ...)

  return(plot)

}
