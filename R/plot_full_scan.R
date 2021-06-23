# Function to plot a complete genome scan at a given time
plot_full_scan <- function(data, grn, time, trait) {

  curr_trait <- trait

  # Various layers of genome scans
  genome_fst_scan <- plot_genome_scan(data, variable = "Fst", time = time, ylab_parsed = "F[ST]")
  genome_freq_scan <- plot_genome_scan(data, variable = "rare_freq", time = time, ylab_parsed = "p[min]")
  genome_alpha_scan <- plot_genome_scan(data, variable = "abs_alpha", time = time, ylab_parsed = "'|'*alpha*'|'")
  genome_qst_scan <- plot_genome_scan(data, variable = "Qst", time = time, ylab_parsed = "Q[ST]")
  genome_cst_scan <- plot_genome_scan(data, variable = "Cst", time = time, ylab_parsed = "C[ST]")

  # Fix the scale on some of them
  genome_fst_scan <- genome_fst_scan + ylim(c(0, 1))
  genome_qst_scan <- genome_qst_scan + ylim(c(0, 1))
  genome_cst_scan <- genome_cst_scan + ylim(c(0, 1))
  genome_freq_scan <- genome_freq_scan + ylim(c(0, 0.5))

  # Extract genome architecture from gene regulatory network
  arch <- grn[["nodes"]]

  # Traits encoded by each locus
  barcode <- plot_barcode(arch)

  # Genetic architecture for a given trait (to avoid clutter)
  effect_size_plot <- plot_effect_sizes(filter(arch, trait == curr_trait))
  degree_plot <- plot_degrees(filter(arch, trait == curr_trait))
  network_plot <- plot_network(as_tbl_graph(grn), trait = curr_trait)

  # Combine all layers of the plot
  network_plot /
    degree_plot /
    effect_size_plot /
    barcode /
    genome_fst_scan /
    genome_qst_scan /
    genome_cst_scan /
    genome_freq_scan /
    genome_alpha_scan +
    plot_layout(heights = c(5, 2, 2, 1, 2, 2, 2, 2, 2), guides = "collect")

}
