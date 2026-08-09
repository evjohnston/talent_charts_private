# publication_spec.R
#
# Physical document geometry, document-facing typography, and renderer density.
# Other styling files derive their sizes and spacing from PUB rather than
# redefining them locally.


# ============================================================================
# Global typography scale
# ============================================================================
# 100 = current/default type sizes
# 95  = all publication text 5% smaller
# 90  = all publication text 10% smaller
# 105 = all publication text 5% larger
#
# This scales text used by figures, tables, and infographics while leaving
# physical page geometry, margins, padding, and chart dimensions unchanged.

FONT_SCALE_PERCENT <- 90

FONT_SCALE <- FONT_SCALE_PERCENT / 100

font_pt <- function(pt) {
  pt * FONT_SCALE
}


# ============================================================================
# Project root + paths
# ============================================================================
# PROJECT_ROOT is normally supplied by setup_publication.R. The fallback below
# walks upward from getwd() so publication_spec.R can still be sourced directly.

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  
  repeat {
    rproj_files <- list.files(
      current,
      pattern = "[.]Rproj$",
      full.names = TRUE
    )
    
    if (length(rproj_files) > 0) {
      return(current)
    }
    
    parent <- dirname(current)
    
    if (identical(parent, current)) {
      stop(
        "Could not locate the project root containing an .Rproj file.",
        call. = FALSE
      )
    }
    
    current <- parent
  }
}

if (!exists("PROJECT_ROOT", inherits = FALSE)) {
  PROJECT_ROOT <- find_project_root()
}

PROJECT_ROOT <- normalizePath(
  PROJECT_ROOT,
  winslash = "/",
  mustWork = TRUE
)

PATHS <- list(
  root               = PROJECT_ROOT,
  
  inputs             = file.path(PROJECT_ROOT, "inputs"),
  scripts            = file.path(PROJECT_ROOT, "inputs", "scripts"),
  data               = file.path(PROJECT_ROOT, "inputs", "data"),
  flags              = file.path(PROJECT_ROOT, "inputs", "flags"),
  fonts              = file.path(PROJECT_ROOT, "inputs", "fonts"),
  titles_and_sources = file.path(
    PROJECT_ROOT,
    "inputs",
    "titles_and_sources.csv"
  ),
  
  outputs            = file.path(PROJECT_ROOT, "outputs"),
  final_charts       = file.path(
    PROJECT_ROOT,
    "outputs",
    "final_charts"
  ),
  final_tables       = file.path(
    PROJECT_ROOT,
    "outputs",
    "final_tables"
  ),
  final_infographics = file.path(
    PROJECT_ROOT,
    "outputs",
    "final_infographics"
  )
)

check_publication_paths <- function() {
  required <- c(
    PATHS$scripts,
    PATHS$data,
    PATHS$flags,
    PATHS$fonts,
    PATHS$titles_and_sources
  )
  
  missing <- required[!file.exists(required)]
  
  if (length(missing)) {
    stop(
      "Project path check failed. Missing: ",
      paste(missing, collapse = ", "),
      ". Project root resolved to: ",
      PROJECT_ROOT,
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}

create_publication_output_dirs <- function() {
  for (path in c(
    PATHS$final_charts,
    PATHS$final_tables,
    PATHS$final_infographics
  )) {
    dir.create(
      path,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }
  
  invisible(TRUE)
}


# ============================================================================
# Publication specification
# ============================================================================

PUB <- list(
  
  page = list(
    width_in  = 8.5,
    height_in = 11,
    margin_in = 1
  ),
  
  # All document-facing font sizes are defined at 100% here, then passed
  # through font_pt(). Change FONT_SCALE_PERCENT above to scale them together.
  type = list(
    title_pt     = font_pt(11),
    subtitle_pt  = font_pt(10),
    caption_pt   = font_pt(10),
    body_pt      = font_pt(10),
    label_pt     = font_pt(10),
    compact_pt   = font_pt(10),
    small_pt     = font_pt(9),
    micro_pt     = font_pt(8),
    milestone_pt = font_pt(7.5),
    source_pt    = font_pt(11)
  ),
  
  # Physical spacing values are in points unless the name says otherwise.
  # These do NOT scale with FONT_SCALE_PERCENT. This keeps chart margins,
  # table padding, and other physical layout dimensions stable as type changes.
  spacing = list(
    # Outer chart edges and named exceptions for label-heavy figures.
    edge_pt             = 10.8,
    edge_wide_pt        = 18,
    edge_xwide_pt       = 24,
    edge_xxwide_pt      = 30,
    
    # Titles, sources, and axes.
    title_bottom_pt       = 5,
    subtitle_bottom_pt    = 14,
    source_top_pt         = 12,
    axis_title_gap_pt     = 10,
    axis_title_compact_pt = 6,
    axis_text_gap_pt      = 4,
    axis_label_right_gap_pt = 8,
    axis_tick_length_pt   = 3,
    
    # Legends and facets.
    legend_top_pt               = 8,
    legend_bottom_pt            = 4,
    legend_key_pt               = 7.5,
    legend_line_key_width_pt    = 40,
    heatmap_legend_key_height_pt = 40,
    heatmap_legend_key_width_pt  = 10,
    facet_label_gap_pt          = 4,
    facet_x_gap_pt              = 20,
    facet_y_gap_pt              = 16,
    bullet_panel_gap_pt         = 28,
    
    # Annotation spacing.
    repel_box_padding_lines          = 0.25,
    repel_point_padding_lines        = 0.25,
    milestone_label_padding_lines    = 0.75,
    label_box_padding_pt             = 5,
    
    # gt table spacing. These are converted from points to the CSS density of
    # each table profile so standard/wide/ultrawide tables keep the same
    # physical padding in Word.
    table_row_pad_pt      = 4,
    table_col_pad_pt      = 4,
    table_heading_pad_pt  = 5,
    table_source_pad_pt   = 5,
    
    # Infographic spacing. These are converted to the infographic CSS density.
    infographic_row_pad_pt     = 2,
    infographic_col_pad_pt     = 2.5,
    infographic_heading_pad_pt = 4,
    infographic_source_pad_pt  = 2.5,
    infographic_side_pad_pt    = 4,
    infographic_group_pad_pt   = 2
  ),
  
  # Unitless type metrics and letter spacing.
  #
  # Line heights remain unitless and do not scale. Letter spacing is expressed
  # in points, so it scales with the typography to preserve the same visual
  # tracking as fonts get smaller or larger.
  type_metrics = list(
    title_lineheight                    = 1.15,
    subtitle_lineheight                 = 1.30,
    dotplot_label_lineheight            = 0.90,
    infographic_kicker_lineheight       = 1.00,
    infographic_title_lineheight        = 1.08,
    infographic_subtitle_lineheight     = 1.15,
    infographic_panel_label_lineheight  = 1.35,
    infographic_panel_title_lineheight  = 1.00,
    infographic_group_lineheight        = 1.10,
    infographic_row_lineheight          = 1.15,
    infographic_source_lineheight       = 1.15,
    infographic_column_lineheight       = 1.10,
    
    infographic_kicker_letterspace_pt      = font_pt(0.75),
    infographic_panel_label_letterspace_pt = font_pt(0.50),
    infographic_panel_title_letterspace_pt = font_pt(0.20)
  ),
  
  render = list(
    figure_dpi    = 300,
    table_css_dpi = 96
  ),
  
  infographic = list(
    width_in  = 6.5,
    height_in = 8.5,
    css_dpi   = 144
  )
)


# ============================================================================
# Derived dimensions
# ============================================================================

# Usable publication dimensions are derived from page size and margins.

PUB$page$body_in <- PUB$page$width_in - 2 * PUB$page$margin_in
PUB$page$body_height_in <- PUB$page$height_in - 2 * PUB$page$margin_in

stopifnot(
  PUB$page$body_in > 0,
  PUB$page$body_height_in > 0
)


# ============================================================================
# Unit conversion helpers
# ============================================================================

pub_pt_to_px <- function(pt, dpi) {
  pt * dpi / 72
}