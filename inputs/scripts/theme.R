# theme.R

# publication_spec.R owns physical dimensions and document-facing type sizes.
if (!exists("PUB")) {
  spec_path <- if (file.exists("inputs/scripts/publication_spec.R")) {
    "inputs/scripts/publication_spec.R"
  } else {
    "publication_spec.R"
  }
  if (!file.exists(spec_path)) stop("publication_spec.R must be sourced before theme.R.")
  source(spec_path)
}

# Gotham is registered in fonts.R before this file during normal report setup.
if (!exists("FONT_FAMILY")) {
  font_path <- file.path(PATHS$scripts, "fonts.R")
  if (!file.exists(font_path)) font_path <- "fonts.R"
  if (file.exists(font_path)) source(font_path)
}
FONT_FAMILY <- if (exists("FONT_FAMILY")) FONT_FAMILY else "gotham"

DATA_DIR <- PATHS$data

# Compatibility aliases. Their values come only from PUB.
DOC_PAGE_WIDTH_IN <- PUB$page$width_in
DOC_MARGIN_IN     <- PUB$page$margin_in
DOC_WIDTH_IN      <- PUB$page$body_in

FIGURE_WIDTH_IN   <- PUB$page$body_in
FIGURE_DPI        <- PUB$render$figure_dpi
TABLE_WIDTH_IN    <- PUB$page$body_in
TABLE_CSS_DPI     <- PUB$render$table_css_dpi
TABLE_EXPORT_DPI  <- PUB$render$table_css_dpi

DOC_TITLE_PT      <- PUB$type$title_pt
DOC_SUBTITLE_PT   <- PUB$type$subtitle_pt
DOC_CAPTION_PT    <- PUB$type$caption_pt
DOC_BODY_PT       <- PUB$type$body_pt
DOC_LABEL_PT      <- PUB$type$label_pt


# ============================================================================
# Document-safe figure export settings
# ============================================================================
# Word target width comes from PUB$page$body_in (page width minus margins).
# All chart size presets keep this same width and vary only height. This prevents
# figures from pasting too wide or shrinking inconsistently in Word.

CHART_WIDTH_IN  <- FIGURE_WIDTH_IN
CHART_EXPORT_DPI <- FIGURE_DPI

chart_height_from <- function(old_w, old_h, new_w = CHART_WIDTH_IN) {
  new_w * old_h / old_w
}

# ---- Chart size presets -------------------------------------
SIZE_STANDARD <- list(w = CHART_WIDTH_IN, h = chart_height_from(8.0,  5.5))
SIZE_MEDIUM   <- list(w = CHART_WIDTH_IN, h = chart_height_from(8.0, 8.25))
SIZE_LONG     <- list(w = CHART_WIDTH_IN, h = chart_height_from(8.0, 11.0))
SIZE_TALL     <- list(w = CHART_WIDTH_IN, h = chart_height_from(6.0,  8.0))
SIZE_WIDE     <- list(w = CHART_WIDTH_IN, h = chart_height_from(12.0, 5.5))
SIZE_XLONG    <- list(w = CHART_WIDTH_IN, h = chart_height_from(8.0, 22.0))
SIZE_SHORT    <- list(w = CHART_WIDTH_IN, h = chart_height_from(8.0,  3.0))

CHART_MARGIN_PT <- PUB$spacing$edge_pt

# ============================================================================
# Shared chart style constants
# ============================================================================

# Line widths
LINE_WIDTH       <- 1.0
LINE_WIDTH_DENSE <- 0.9
LINE_WIDTH_FACET <- 0.7
LINE_WIDTH_LIGHT <- 0.9
LINE_WIDTH_REF   <- 0.4

# Alpha
ALPHA_FILL      <- 0.95
ALPHA_AREA      <- 0.95
ALPHA_COL       <- 0.90
ALPHA_COL_LIGHT <- 0.60
ALPHA_TREND     <- 0.80
ALPHA_BUBBLE    <- 0.75

# Points and bars
POINT_SIZE       <- 2
BAR_WIDTH        <- 0.7
BAR_WIDTH_NARROW <- 0.65
BAR_WIDTH_SINGLE <- 0.4

# Right-side x padding for endpoint labels
PAD_RIGHT_TIGHT    <- 0.03
PAD_RIGHT_NARROW   <- 0.18
PAD_RIGHT_STANDARD <- 0.20
PAD_RIGHT_WIDE     <- 0.30
PAD_RIGHT_XWIDE    <- 0.40

# Scale expansion helpers
EXPAND_BAR         <- expansion(mult = c(0,    0.05))
EXPAND_BAR_TALL    <- expansion(mult = c(0,    0.10))
EXPAND_BAR_XLARGE  <- expansion(mult = c(0,    0.12))
EXPAND_BAR_TIGHT   <- expansion(mult = c(0,    0.02))
EXPAND_LINE        <- expansion(mult = c(0.02, 0.05))
EXPAND_LINE_WIDE   <- expansion(mult = c(0.05, 0.08))
EXPAND_LINE_TALL   <- expansion(mult = c(0.05, 0.10))
EXPAND_NONE        <- expansion(mult = c(0,    0))
EXPAND_TIGHT_TOP   <- expansion(mult = c(0,    0.02))
EXPAND_LABEL_RIGHT <- expansion(mult = c(0,    0.15))

# ---- Colors -------------------------------------------------
tpa_colors <- c(
  "#8c1515", "#2F5D7C", "#F5B427",
  "#92A045", "#5E3762", "#F68F1F",
  "#5D4738", "#918881", "#B9B4AC"
)

arrow_fill <- c(
  "Total"         = tpa_colors[1],  
  "Undergraduate" = tpa_colors[2],
  "Graduate"      = tpa_colors[3],
  "Non-Degree"    = tpa_colors[4],
  "OPT"           = tpa_colors[5]
)

# Named single-use hex values
TPA_NAVY_DARK <- "#1F3D52"   # darkest navy, used in mobility gradient
TPA_RED_MID   <- "#C46C5D"   # mid red, used in mobility gradient
TPA_RED_LIGHT <- "#F4D6D6"   # light red, used as gradient low end in tables

level_colors <- c(
  "Bachelors" = "#F5B427",
  "Bachelor's" = "#F5B427",
  "Masters"   = "#2F5D7C",
  "Master's"   = "#2F5D7C",
  "Doctorate" = "#8C1515",
  "Doctorate" = "#8C1515",
  "Doctorates" = "#8C1515",
  "All Doctorates (IPEDS)" = "#8C1515",
  "Research Doctorates (SED)" = "#92A045"
)

acad_level_colors <- c(
  "Bachelors"              = tpa_colors[3],
  "Masters"                = tpa_colors[2],
  "Doctorate"              = tpa_colors[1],
  "OPT"                    = tpa_colors[4],
  "Associates"             = tpa_colors[5],
  "Non-degree"             = tpa_colors[6],
  "Graduate (unspecified)" = tpa_colors[7],
  "Professional"           = tpa_colors[8]
)

navy_tints <- c(
  "#A6C5DE",
  "#5C8AAE",
  tpa_colors[2]
)

field_colors <- c(
  "Biological and biomedical sciences" = tpa_colors[2],
  "Engineering"                        = tpa_colors[4],
  "Physical Sciences"                  = tpa_colors[3],
  "Social Sciences"                    = tpa_colors[1],
  "Health Sciences"                    = tpa_colors[5],
  "Computer Sciences"                  = tpa_colors[6],
  "Mathematics"                        = tpa_colors[7],
  "Psychology"                         = tpa_colors[8],
  "Non-sciences"                       = tpa_colors[9]
)

stem_gradient_colors <- c(
  "Agriculture"                = TPA_NAVY_DARK,
  "Engineering"                = tpa_colors[2],
  "Health professions"         = "#5C8AAE",
  "Math and computer science"  = "#A6C5DE",
  "Physical and life sciences" = "#CDDEEC",
  "Social sciences"            = "#E8A99E",
  "Non-STEM"                   = tpa_colors[1]
)

# Citizenship status — used by figs 015-017, 090, 095-097.
# Standard mapping: red = US citizens, green = temp visa holders.
citizenship_colors <- c(
  "U.S. citizens and permanent residents" = tpa_colors[1],
  "Temporary visa holders"                = tpa_colors[4]
)

# Inverted citizenship mapping — used by figs 021, 025.
# Navy = citizens, red = TVH; same names, swapped colors.
citizenship_colors_alt <- c(
  "U.S. citizens and permanent residents" = tpa_colors[2],
  "Temporary visa holders"                = tpa_colors[1]
)

# H-1B denial palette — used by figs 073, 075, 087, 088.
hb_denial_colors <- c(
  "New Denials"     = tpa_colors[2],
  "Renewal Denials" = tpa_colors[1]
)

# China / US two-country palette — used by figs 044, 045, 047, 048.
china_us_colors <- c(
  "China"         = tpa_colors[1],
  "United States" = tpa_colors[2]
)

# Degree-level employment palette (four levels).
# Used by figs 027, 028. Extends level_colors with Professional.
degree_employment_colors <- c(
  "Doctorates"   = tpa_colors[1],
  "Professional" = tpa_colors[5],
  "Masters"      = tpa_colors[2],
  "Bachelors"    = tpa_colors[3]
)

# Researcher mobility gradient — used by figs 052, 053.
# The two "Inbound" keys are renamed at the chunk level to include
# the destination country ("US" or "China").
mobility_colors_base <- c(
  "Purely Domestic"                                 = TPA_NAVY_DARK,
  "Started Domestic, Trained Abroad, Returned"      = tpa_colors[2],
  "Inbound, Stayed"                                 = "#A6C5DE",
  "Started Domestic, Trained Abroad, Stayed Abroad" = "#E8A99E",
  "Inbound, Left"                                   = TPA_RED_MID,
  "Purely Abroad"                                   = tpa_colors[1]
)


# Diverging heatmap gradient: tpa_colors[2] → "#F5F2EC" → tpa_colors[1]
# Use this order in scale_fill_gradientn for pct-change heatmaps (fig_004)
HEATMAP_MID <- "#F5F2EC"

# ---- Scale helpers ------------------------------------------
scale_color_fields <- function(...) scale_color_manual(values = field_colors, ...)
scale_fill_fields  <- function(...) scale_fill_manual(values = field_colors, ...)
scale_color_tpa    <- function(...) scale_color_manual(values = tpa_colors, ...)
scale_fill_tpa     <- function(...) scale_fill_manual(values = tpa_colors, ...)

scale_color_levels <- function(...) {
  scale_color_manual(values = level_colors, breaks = names(level_colors), ...)
}

scale_fill_levels <- function(reverse_legend = FALSE, ...) {
  scale_fill_manual(
    values = level_colors,
    breaks = names(level_colors),
    guide  = guide_legend(reverse = reverse_legend),
    ...
  )
}

scale_fill_citizenship <- function(...) {
  scale_fill_manual(values = citizenship_colors, ...)
}

scale_color_china_us <- function(...) {
  scale_color_manual(values = china_us_colors, ...)
}

# ---- Typography ---------------------------------------------
# These point sizes are the document-facing sizes. When figures are saved
# at CHART_WIDTH_IN = 7.5 inches, they should appear at these sizes in Word.
TITLE_SIZE           <- DOC_TITLE_PT
SUBTITLE_SIZE        <- DOC_SUBTITLE_PT
CAPTION_SIZE         <- DOC_CAPTION_PT
AXIS_SIZE            <- DOC_BODY_PT
SWATCH_SIZE          <- PUB$spacing$legend_key_pt
DATA_LABEL_SIZE      <- DOC_LABEL_PT
COMPACT_LABEL_SIZE   <- PUB$type$compact_pt
SMALL_LABEL_SIZE     <- PUB$type$small_pt
MICRO_LABEL_SIZE     <- PUB$type$micro_pt
MILESTONE_LABEL_SIZE <- PUB$type$milestone_pt
BASE_SIZE            <- DOC_BODY_PT

CHART_LINE <- "  "

# ---- Theme helpers ------------------------------------------
base_text <- function(size = AXIS_SIZE, color = "grey25", face = NULL) {
  element_text(family = FONT_FAMILY, size = size, color = color, face = face)
}

publication_margin <- function(
    top = PUB$spacing$edge_pt,
    right = PUB$spacing$edge_pt,
    bottom = PUB$spacing$edge_pt,
    left = PUB$spacing$edge_pt
) {
  margin(top, right, bottom, left, unit = "pt")
}

theme_x_rotated <- function(angle = 45, hjust = 1, vjust = 1) {
  theme(
    axis.text.x = element_text(
      family = FONT_FAMILY,
      color  = "grey25",
      size   = AXIS_SIZE,
      angle  = angle,
      hjust  = hjust,
      vjust  = vjust,
      margin = margin(t = PUB$spacing$axis_text_gap_pt)
    )
  )
}

theme_x_horizontal <- function(hjust = 0.5, vjust = 1) {
  theme_x_rotated(angle = 0, hjust = hjust, vjust = vjust)
}

# ---- Theme --------------------------------------------------
theme_tpa <- function() {
  theme_minimal(base_size = BASE_SIZE, base_family = FONT_FAMILY) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_textbox_simple(
        family     = FONT_FAMILY,
        face       = "bold",
        size       = TITLE_SIZE,
        color      = "black",
        lineheight = PUB$type_metrics$title_lineheight,
        margin     = margin(b = PUB$spacing$title_bottom_pt)
      ),
      plot.subtitle = element_textbox_simple(
        family     = FONT_FAMILY,
        size       = SUBTITLE_SIZE,
        color      = "grey45",
        lineheight = PUB$type_metrics$subtitle_lineheight,
        margin     = margin(b = PUB$spacing$subtitle_bottom_pt)
      ),
      plot.caption = element_textbox_simple(
        family = FONT_FAMILY,
        size   = CAPTION_SIZE,
        color  = "grey45",
        hjust  = 0,
        margin = margin(t = PUB$spacing$source_top_pt)
      ),
      
      axis.title.x = element_blank(),
      axis.title.y = element_text(
        family = FONT_FAMILY,
        margin = margin(r = PUB$spacing$axis_title_gap_pt),
        color  = "grey25",
        size   = AXIS_SIZE
      ),
      axis.text   = base_text(),
      axis.text.x = element_text(
        family = FONT_FAMILY,
        color  = "grey25",
        size   = AXIS_SIZE,
        angle  = 45,
        hjust  = 1,
        vjust  = 1,
        margin = margin(t = PUB$spacing$axis_text_gap_pt)
      ),
      
      axis.line         = element_line(color = "grey50", linewidth = 0.4),
      axis.ticks        = element_line(color = "grey50", linewidth = 0.35),
      axis.ticks.length = unit(PUB$spacing$axis_tick_length_pt, "pt"),
      
      panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      
      legend.position      = "bottom",
      legend.justification = "center",
      legend.margin        = margin(t = PUB$spacing$legend_top_pt, b = PUB$spacing$legend_bottom_pt),
      legend.title         = element_blank(),
      legend.text          = base_text(),
      legend.key.width     = unit(SWATCH_SIZE, "pt"),
      legend.key.height    = unit(SWATCH_SIZE, "pt"),
      
      plot.margin = publication_margin()
    )
}

# ---- Pie Chart variant ----------------------------------------
theme_pie <- function() {
  theme_tpa() +
    theme(
      axis.title      = element_blank(),
      axis.text       = element_blank(),
      axis.ticks      = element_blank(),
      axis.line       = element_blank(),
      panel.grid      = element_blank(),
      panel.border    = element_blank(),
      panel.background = element_blank(),
      legend.position = "bottom"
    )
}

# ---- Heatmap variant ----------------------------------------
theme_heatmap <- function() {
  theme_tpa() +
    theme(
      panel.grid = element_blank(),
      axis.line  = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(
        family = FONT_FAMILY,
        color  = "grey25",
        size   = AXIS_SIZE,
        angle  = 0,
        hjust  = 0.5
      ),
      legend.position   = "right",
      legend.key.height = unit(PUB$spacing$heatmap_legend_key_height_pt, "pt"),
      legend.key.width  = unit(PUB$spacing$heatmap_legend_key_width_pt, "pt"),
      legend.title      = base_text()
    )
}

# ---- Faceted variant ----------------------------------------
theme_facet <- function() {
  theme_tpa() +
    theme(
      strip.text = element_text(
        family = FONT_FAMILY,
        face   = "bold",
        size   = AXIS_SIZE,
        color  = "grey20",
        hjust  = 0,
        margin = margin(b = PUB$spacing$facet_label_gap_pt)
      ),
      strip.background = element_blank(),
      panel.spacing.x  = unit(PUB$spacing$facet_x_gap_pt, "pt"),
      panel.spacing.y  = unit(PUB$spacing$facet_y_gap_pt, "pt"),
      axis.text.x = element_text(
        family = FONT_FAMILY,
        color  = "grey25",
        size   = COMPACT_LABEL_SIZE,
        angle  = 45,
        hjust  = 1,
        margin = margin(t = PUB$spacing$axis_text_gap_pt)
      ),
      axis.text.y = base_text(size = COMPACT_LABEL_SIZE)
    )
}

# ---- Observed/Projected variant -----------------------------
theme_op <- function() {
  theme_tpa() + theme(legend.key.width = unit(PUB$spacing$legend_line_key_width_pt, "pt"))
}

# ---- Dual-axis variant --------------------------------------
theme_dual_axis <- function() {
  theme_tpa() +
    theme(
      axis.title.y.right = element_text(
        family = FONT_FAMILY,
        margin = margin(l = PUB$spacing$axis_title_gap_pt),
        color  = "grey25",
        size   = AXIS_SIZE
      )
    )
}

# ---- Horizontal bar variant ---------------------------------
theme_horizontal_bar <- function() {
  theme_tpa() +
    theme(
      axis.text.x = element_text(
        family = FONT_FAMILY,
        color  = "grey25",
        size   = AXIS_SIZE,
        angle  = 0,
        hjust  = 0.5,
        margin = margin(t = PUB$spacing$axis_text_gap_pt)
      ),
      axis.title.y       = element_blank(),
      panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      axis.line.x        = element_line(color = "grey50", linewidth = 0.4),
      axis.line.y        = element_blank(),
      axis.ticks.x       = element_line(color = "grey50", linewidth = 0.35),
      axis.ticks.y       = element_blank()
    )
}

# ---- Donut/pie variant --------------------------------------
theme_donut <- function() {
  theme_tpa() +
    theme(
      axis.title  = element_blank(),
      axis.text   = element_blank(),
      axis.line   = element_blank(),
      axis.ticks  = element_blank(),
      panel.grid  = element_blank(),
      legend.position = "bottom"
    )
}

# ---- Block-arrow variant ------------------------------------
theme_arrows <- function() {
  theme_tpa() +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.text.x = element_blank(),   # theme_tpa sets this separately; override it
      axis.ticks = element_blank(),
      axis.line  = element_blank(),
      panel.grid = element_blank()
    )
}

# ---- Cleveland dot-plot variant -----------------------------
theme_dotplot <- function() {
  theme_tpa() +
    theme(
      axis.title   = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x  = element_blank(),
      axis.line.y  = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
      axis.text.y = element_text(
        family = FONT_FAMILY, color = "grey25", size = AXIS_SIZE,
        lineheight = PUB$type_metrics$dotplot_label_lineheight,
        margin = margin(r = PUB$spacing$axis_label_right_gap_pt)
      ),
      legend.justification = "right"
    )
}

# ---- Bullet / progress-bar variant --------------------------
theme_bullet <- function() {
  theme_tpa() +
    theme(
      axis.title  = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks  = element_blank(),
      axis.line   = element_blank(),
      panel.grid  = element_blank(),
      axis.text.y = element_text(
        family = FONT_FAMILY, color = "grey25",
        size = AXIS_SIZE, lineheight = PUB$type_metrics$dotplot_label_lineheight
      ),
      strip.text = element_text(
        family = FONT_FAMILY, face = "bold",
        size = TITLE_SIZE, color = "grey15",
        hjust = 0.5, margin = margin(b = 0)
      ),
      strip.background = element_blank(),
      panel.spacing.x  = unit(PUB$spacing$bullet_panel_gap_pt, "pt"),
      legend.position  = "none"
    )
}