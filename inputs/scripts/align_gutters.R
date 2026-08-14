# align_gutters.R  (v2 — left edge only)
#
# Pins the panel's LEFT edge to the same x position across figures, so the
# y-axis line lands in the same place whether the labels read "60K", "2.0M",
# "6%", or "150K". The RIGHT side is deliberately left alone: prepare_right_labels()
# already reserves label space inside the x-scale, and touching the gtable's
# right columns (as an earlier version did) clips those labels. Don't do that.
#
# Requires only 'grid' and 'gtable' (base-R shipped).

library(grid)
library(gtable)

# Fixed width for the left axis-text column, in points. Set this just wide
# enough that the widest y-label across the whole chapter clears without
# clipping. "1.2M" / "150K" / "800K" are the long ones; 34pt at DOC_BODY_PT
# Gotham is a safe starting point — tune by eye.
AXIS_LEFT_PT <- 34

fix_left_gutter <- function(plot, left_pt = AXIS_LEFT_PT) {
  g <- ggplotGrob(plot)
  
  # The left axis text lives in the column named "axis-l". Force just that
  # column to a fixed width. The y-title ("ylab-l") and panel keep their own
  # widths, so the panel's left edge becomes constant across figures with
  # different y-label string lengths.
  axis_l <- which(g$layout$name == "axis-l")
  if (length(axis_l)) {
    col <- g$layout$l[axis_l]
    g$widths[col] <- unit(left_pt, "pt")
  }
  g
}

save_gtable <- function(gt, name, size = "standard",
                        out_dir = PATHS$final_charts,
                        dpi = CHART_EXPORT_DPI, bg = "white") {
  dims <- chart_size(size)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_path <- file.path(out_dir, paste0(name, ".png"))
  
  device_fun <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else grDevices::png
  device_fun(out_path, width = dims$w, height = dims$h, units = "in", res = dpi, background = bg)
  grid::grid.newpage()
  grid::grid.draw(gt)
  grDevices::dev.off()
  
  set_png_density(out_path, dpi = dpi)
  invisible(out_path)
}

# Drop-in replacement for save_chart that pins the left edge only.
save_chart_aligned <- function(plot, name, size = "standard",
                               left_pt = AXIS_LEFT_PT, ...) {
  gt <- fix_left_gutter(plot, left_pt = left_pt)
  save_gtable(gt, name, size = size, ...)
  invisible(plot)
}