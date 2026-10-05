#' Plot the cost-effectiveness plane
#'
#' One point per run: incremental QALYs per patient (x) against incremental cost
#' per patient (y), iABC minus usual care. The mean across runs, which gives the
#' ICER, is marked and labelled with the ICER in words for its quadrant (see
#' [tm_compare()]). The plane is centred on the origin, so all four
#' quadrants are shown. Each willingness-to-pay value in `wtp` (`settings.yaml`)
#' is drawn as a line through the origin with that slope; points below a line are
#' cost-effective at that threshold.
#'
#' @param x Results, from [tm_run()].
#' @param ... Not used.
#' @return A `ggplot` object, invisibly; it is also drawn.
#' @export
plot.tm_results <- function(x, ...) {
  p <- ce_plane(x)
  print(p)
  invisible(p)
}

ce_plane <- function(x) {
  ink <- list(primary = "#0b0b0b", secondary = "#52514e", grid = "#e1e0d9", axis = "#c3c2b7",
              surface = "#fcfcfb", point = "#2a78d6")
  runs <- x$comparison_by_run
  mean_point <- x$comparison
  currency <- x$currency
  money <- function(v) {
    paste0(ifelse(v < 0, "-", ""), currency, format(round(abs(v)), big.mark = ",", trim = TRUE))
  }

  # Equal room on both sides of zero on each axis, so the origin is in the centre and all four quadrants show.
  half_width <- function(v) {
    m <- max(abs(v), na.rm = TRUE) * 1.15
    if (!is.finite(m) || m == 0) 1 else m
  }
  x_lim <- half_width(c(runs$delta_qaly, mean_point$delta_qaly))
  y_lim <- half_width(c(runs$delta_cost, mean_point$delta_cost))
  wtp_lines <- wtp_line_ends(x$wtp, x_lim, y_lim)

  p <- ggplot2::ggplot(runs, ggplot2::aes(x = delta_qaly, y = delta_cost)) +
    ggplot2::geom_hline(yintercept = 0, colour = ink$axis, linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = 0, colour = ink$axis, linewidth = 0.4)
  if (nrow(wtp_lines) > 0) {
    wtp_lines[, label := factor(paste0(money(wtp), " per QALY"), levels = paste0(money(wtp), " per QALY"))]
    dashes <- rep(c("22", "62", "12", "4212", "1343", "8222"), length.out = nrow(wtp_lines))
    p <- p +
      ggplot2::geom_segment(data = wtp_lines,
                            ggplot2::aes(x = -x_end, y = -y_end, xend = x_end, yend = y_end, linetype = label),
                            inherit.aes = FALSE, colour = ink$secondary, linewidth = 0.5) +
      ggplot2::scale_linetype_manual(values = stats::setNames(dashes, levels(wtp_lines$label)),
                                     name = "Willingness to pay",
                                     guide = ggplot2::guide_legend(title.position = "top"))
  }
  label_left <- mean_point$delta_qaly > 0  # put the mean's label on the side with more room
  dominance <- mean_point$quadrant %in% c("dominant", "dominated")
  mean_label <- paste0("Mean: ", if (!dominance) "ICER ", icer_text(mean_point$icer, mean_point$quadrant, currency))
  p <- p +
    ggplot2::geom_point(shape = 21, size = 2.6, stroke = 0.6, fill = ink$point, colour = ink$surface, alpha = 0.85) +
    ggplot2::geom_point(data = mean_point, shape = 23, size = 4, stroke = 0.8, fill = ink$primary, colour = ink$surface) +
    ggplot2::geom_label(
      data = mean_point,
      ggplot2::aes(label = mean_label),
      hjust = if (label_left) 1.08 else -0.08, vjust = 0.5, size = 3.4, colour = ink$primary, fill = ink$surface,
      linewidth = 0, label.padding = ggplot2::unit(0.15, "lines")
    )
  p +
    ggplot2::coord_cartesian(xlim = c(-x_lim, x_lim), ylim = c(-y_lim, y_lim)) +
    ggplot2::scale_y_continuous(labels = money) +
    ggplot2::labs(
      title = "Cost-effectiveness plane: iABC vs usual care",
      x = "Incremental QALYs per patient",
      y = paste0("Incremental cost per patient (", currency, ")")
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = ink$surface, colour = NA),
      panel.grid = ggplot2::element_line(colour = ink$grid, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = ink$secondary),
      axis.title = ggplot2::element_text(colour = ink$secondary),
      plot.title = ggplot2::element_text(colour = ink$primary, face = "bold"),
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_text(colour = ink$secondary),
      legend.text = ggplot2::element_text(colour = ink$secondary),
      legend.key.width = ggplot2::unit(1.4, "cm")
    )
}

# Upper-right end points of the willingness-to-pay lines (through the origin, slope = wtp), stopping at the edge
# of the plotted area (x_lim, y_lim) so every line and label stays inside the plot. Each line runs from
# (-x_end, -y_end) to (x_end, y_end).
wtp_line_ends <- function(wtp, x_lim, y_lim) {
  if (length(wtp) == 0) return(data.table::data.table(wtp = numeric(0), x_end = numeric(0), y_end = numeric(0)))
  ends <- data.table::data.table(wtp = wtp)
  ends[, x_end := pmin(x_lim, y_lim / wtp)]
  ends[, y_end := wtp * x_end]
  ends[]
}
