plot_perf <- function(dat, y = "rank", ylim = c(0, 1), xlim = c(0, 1), b_range = c(-1, 1)) {
  ggplot(dat, aes_string("inaccuracy", y = y, fill = "bias")) +
    geom_point(pch = 21, size = 3.3) +
    scale_fill_gradient2(
      low = muted("red"),
      mid = "white",
      high = muted("blue"), midpoint = 0, limits = b_range) +
    ggsidekick::theme_sleek() +
    coord_cartesian(ylim = ylim, expand = TRUE, xlim = xlim) +
    geom_text_repel(aes(label = method),
      size = 2.9, colour = "grey30",
      point.padding = unit(0.2, "lines"), max.iter = 6e3, segment.size = 0.3) +
    labs(fill = "Bias", x = "Inaccuracy (MAPE)", y = "Rank-order correlation")
}
