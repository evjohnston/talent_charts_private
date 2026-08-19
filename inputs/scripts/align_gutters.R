# align_gutters.R  (v3 — left edge only + HTML/PNG/PDF export)
#
# Pins the panel's LEFT edge to the same x position across figures, so the
# y-axis line lands in the same place whether the labels read "60K", "2.0M",
# "6%", or "150K". The RIGHT side is deliberately left alone: prepare_right_labels()
# already reserves label space inside the x-scale, and touching the gtable's
# right columns (as an earlier version did) clips those labels. Don't do that.
#
# Export behavior mirrors save_chart() in helpers.R:
#   final_charts/html/<name>.html
#   final_charts/pngs/<name>.png
#   final_charts/pdfs/<name>.pdf
#
# Source helpers.R BEFORE this file. This file relies on:
#   chart_size()
#   set_png_density()
#   raster_to_html_exact()
#   raster_to_pdf_exact()
#
# Requires grid and gtable. ragg is used when available for the canonical PNG.

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
  required_helpers <- c(
    "chart_size",
    "set_png_density",
    "raster_to_html_exact",
    "raster_to_pdf_exact"
  )
  missing_helpers <- required_helpers[
    !vapply(required_helpers, exists, logical(1), mode = "function")
  ]
  if (length(missing_helpers) > 0) {
    stop(
      "align_gutters.R requires helpers.R to be sourced first. Missing: ",
      paste(missing_helpers, collapse = ", "),
      call. = FALSE
    )
  }
  
  dims <- chart_size(size)
  
  html_dir <- file.path(out_dir, "html")
  png_dir  <- file.path(out_dir, "pngs")
  pdf_dir  <- file.path(out_dir, "pdfs")
  
  for (path in c(html_dir, png_dir, pdf_dir)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }
  
  html_path <- file.path(html_dir, paste0(name, ".html"))
  png_path  <- file.path(png_dir,  paste0(name, ".png"))
  pdf_path  <- file.path(pdf_dir,  paste0(name, ".pdf"))
  
  # 1. Canonical publication PNG.
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(
      filename   = png_path,
      width      = dims$w,
      height     = dims$h,
      units      = "in",
      res        = dpi,
      background = bg
    )
  } else {
    grDevices::png(
      filename = png_path,
      width    = dims$w,
      height   = dims$h,
      units    = "in",
      res      = dpi,
      bg       = bg
    )
  }
  
  device_open <- TRUE
  on.exit({
    if (device_open) grDevices::dev.off()
  }, add = TRUE)
  
  grid::grid.newpage()
  grid::grid.draw(gt)
  grDevices::dev.off()
  device_open <- FALSE
  
  set_png_density(png_path, dpi = dpi)
  
  # 2. PDF page at the same physical dimensions.
  raster_to_pdf_exact(
    image_path = png_path,
    pdf_path   = pdf_path,
    width_in   = dims$w,
    height_in  = dims$h,
    bg         = bg
  )
  
  # 3. Self-contained HTML carrying the exact same finished chart image.
  raster_to_html_exact(
    image_path = png_path,
    html_path  = html_path,
    width_in   = dims$w,
    height_in  = dims$h,
    bg         = bg
  )
  
  invisible(
    list(
      html_path = html_path,
      png_path  = png_path,
      pdf_path  = pdf_path
    )
  )
}

# Drop-in replacement for save_chart() that pins the left edge only.
# Existing Rmd calls do not need to change.
save_chart_aligned <- function(plot, name, size = "standard",
                               left_pt = AXIS_LEFT_PT, ...) {
  gt <- fix_left_gutter(plot, left_pt = left_pt)
  save_gtable(gt, name, size = size, ...)
  invisible(plot)
}
