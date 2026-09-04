# align_gutters.R  (v4 — left edge only + HTML/PNG/vector-PDF export)
#
# Pins the panel's LEFT edge to the same x position across figures.
#
# Export behavior:
#   final_charts/html/<name>.html
#   final_charts/pngs/<name>.png
#   final_charts/pdfs/<name>.pdf
#
# PNG and HTML remain raster-based.
# PDF is drawn directly from the gtable and remains vector.
#
# Source helpers.R BEFORE this file. This file relies on:
#   chart_size()
#   set_png_density()
#   raster_to_html_exact()
#
# Requires grid and gtable. ragg is used when available for the PNG.

library(grid)
library(gtable)

AXIS_LEFT_PT <- 34

fix_left_gutter <- function(plot, left_pt = AXIS_LEFT_PT) {
  g <- ggplotGrob(plot)
  
  axis_l <- which(g$layout$name == "axis-l")
  
  if (length(axis_l)) {
    col <- g$layout$l[axis_l]
    g$widths[col] <- unit(left_pt, "pt")
  }
  
  g
}

save_gtable <- function(
    gt,
    name,
    size = "standard",
    out_dir = PATHS$final_charts,
    dpi = CHART_EXPORT_DPI,
    bg = "white"
) {
  required_helpers <- c(
    "chart_size",
    "set_png_density",
    "raster_to_html_exact"
  )
  
  missing_helpers <- required_helpers[
    !vapply(
      required_helpers,
      exists,
      logical(1),
      mode = "function"
    )
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
    dir.create(
      path,
      showWarnings = FALSE,
      recursive = TRUE
    )
  }
  
  html_path <- file.path(
    html_dir,
    paste0(name, ".html")
  )
  
  png_path <- file.path(
    png_dir,
    paste0(name, ".png")
  )
  
  pdf_path <- file.path(
    pdf_dir,
    paste0(name, ".pdf")
  )
  
  # -----------------------------------------------------------------------
  # 1. Canonical publication PNG
  # -----------------------------------------------------------------------
  
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
  
  png_device_open <- TRUE
  
  tryCatch(
    {
      grid::grid.newpage()
      grid::grid.draw(gt)
    },
    finally = {
      if (png_device_open) {
        grDevices::dev.off()
        png_device_open <- FALSE
      }
    }
  )
  
  set_png_density(
    png_path,
    dpi = dpi
  )
  
  # -----------------------------------------------------------------------
  # 2. Native vector PDF
  # -----------------------------------------------------------------------
  
  if (!requireNamespace("Cairo", quietly = TRUE)) {
    stop(
      "Package 'Cairo' is required. Install it with install.packages('Cairo').",
      call. = FALSE
    )
  }
  
  # Prevent an old raster PDF from remaining after a failed export.
  if (file.exists(pdf_path)) {
    unlink(pdf_path)
  }
  
  grDevices::quartz(
    type   = "pdf",
    file   = pdf_path,
    width  = dims$w,
    height = dims$h,
    family = "Gotham",
    bg     = bg
  )
  
  pdf_device_open <- TRUE
  
  tryCatch(
    {
      grid::grid.newpage()
      grid::grid.draw(gt)
    },
    finally = {
      if (pdf_device_open) {
        grDevices::dev.off()
        pdf_device_open <- FALSE
      }
    }
  )
  
  if (!file.exists(pdf_path)) {
    stop(
      "Vector PDF was not created: ",
      pdf_path,
      call. = FALSE
    )
  }
  
  # -----------------------------------------------------------------------
  # 3. Self-contained raster HTML
  # -----------------------------------------------------------------------
  
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

save_chart_aligned <- function(
    plot,
    name,
    size = "standard",
    left_pt = AXIS_LEFT_PT,
    ...
) {
  gt <- fix_left_gutter(
    plot,
    left_pt = left_pt
  )
  
  save_gtable(
    gt,
    name,
    size = size,
    ...
  )
  
  invisible(plot)
}