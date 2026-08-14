# tables.R

# ============================================================================
# Shared gt table settings
# ============================================================================
# publication_spec.R owns the physical width and typography. Table profiles only
# change the CSS canvas/density used to fit more columns; printed type sizes stay
# constant across standard, wide, and ultrawide outputs.

if (!exists("PUB")) {
  spec_path <- if (file.exists("inputs/scripts/publication_spec.R")) {
    "inputs/scripts/publication_spec.R"
  } else {
    "publication_spec.R"
  }
  if (!file.exists(spec_path)) stop("publication_spec.R must be sourced before tables.R.")
  source(spec_path)
}

# ============================================================================
# Table typography scale
# ============================================================================
# Only the table BODY system is scaled here.
#
# Titles and captions/sources deliberately do NOT use this scale:
#   - table title = PUB$type$title_pt, matching figure/infographic titles
#   - table source/caption = PUB$type$caption_pt, matching figure captions
#
# 100 = table body/header text uses PUB$type$body_pt as-is
# 90  = table body/header text 10% smaller
# 85  = table body/header text 15% smaller
# 80  = table body/header text 20% smaller
#
# This scale multiplies the global FONT_SCALE_PERCENT already applied in
# publication_spec.R. Physical table width and padding are not changed.

TABLE_FONT_SCALE_PERCENT <- 90

TABLE_FONT_SCALE <- TABLE_FONT_SCALE_PERCENT / 100

table_font_pt <- function(pt) {
  pt * TABLE_FONT_SCALE
}

WORD_TABLE_WIDTH_IN <- PUB$page$body_in

# HTML/CSS rendering density. Individual profiles use different CSS widths
# internally so dense tables can fit more columns without changing their
# final physical publication width.
TABLE_CSS_DPI <- PUB$render$table_css_dpi

# Final PNGs use the SAME physical width and raster density as figures.
# The pixel width is always derived from the active publication body width, so
# changing page margins in publication_spec.R updates every table automatically.
TABLE_EXPORT_DPI <- PUB$render$figure_dpi
TABLE_EXPORT_PX  <- round(WORD_TABLE_WIDTH_IN * TABLE_EXPORT_DPI)

# Retained for existing hand-tuned column widths.
TABLE_CSS_WIDTH_PX <- round(WORD_TABLE_WIDTH_IN * TABLE_CSS_DPI)
TABLE_LEGACY_CSS_WIDTH_PX <- round(WORD_TABLE_WIDTH_IN * 200)
TABLE_WIDTH_SCALE <- 1
table_scaled_px <- function(px_value) round(px_value * TABLE_WIDTH_SCALE)

TABLE_EXPORT_HEIGHTS_PX <- c(
  short    = 650,
  standard = 1100,
  long     = 2200,
  xlong    = 4200
)

table_pt_to_px <- function(pt, dpi = TABLE_CSS_DPI) pub_pt_to_px(pt, dpi)

# ---------------------------------------------------------------------------
# Publication-facing table typography
# ---------------------------------------------------------------------------
# These are the ONLY table type-size controls. Builders should never introduce
# local font-size overrides. Standard/wide/ultrawide profiles change only the
# internal CSS canvas used to lay out dense columns; final physical type size is
# held constant by apply_table_profile() + save_table().
TABLE_DATA_FONT_PT          <- table_font_pt(PUB$type$body_pt)
TABLE_COLUMN_LABEL_FONT_PT  <- table_font_pt(PUB$type$body_pt)
TABLE_SPANNER_FONT_PT       <- table_font_pt(PUB$type$body_pt)
TABLE_HEADER_FONT_PT        <- TABLE_COLUMN_LABEL_FONT_PT  # compatibility alias
TABLE_FOOTNOTE_FONT_PT      <- table_font_pt(PUB$type$small_pt)

# Publication-facing elements remain tied directly to the global publication
# spec so table titles and sources match the figure/infographic caption system.
TABLE_TITLE_FONT_PT      <- PUB$type$title_pt
TABLE_SUBTITLE_FONT_PT   <- PUB$type$subtitle_pt
TABLE_SOURCE_FONT_PT     <- PUB$type$caption_pt

# One vertical-rhythm system for every table.
TABLE_BODY_LINEHEIGHT   <- 1.15
TABLE_HEADER_LINEHEIGHT <- 1.10
TABLE_TITLE_LINEHEIGHT <- if (!is.null(PUB$type_metrics$title_lineheight)) {
  PUB$type_metrics$title_lineheight
} else {
  1.15
}
TABLE_SUBTITLE_LINEHEIGHT <- if (!is.null(PUB$type_metrics$subtitle_lineheight)) {
  PUB$type_metrics$subtitle_lineheight
} else {
  1.30
}
TABLE_SOURCE_LINEHEIGHT <- if (!is.null(PUB$type_metrics$infographic_source_lineheight)) {
  PUB$type_metrics$infographic_source_lineheight
} else {
  1.15
}

TABLE_FONT_PX          <- table_pt_to_px(TABLE_DATA_FONT_PT)
TABLE_HEADER_FONT_PX   <- table_pt_to_px(TABLE_COLUMN_LABEL_FONT_PT)
TABLE_SPANNER_FONT_PX  <- table_pt_to_px(TABLE_SPANNER_FONT_PT)
TABLE_FOOTNOTE_FONT_PX <- table_pt_to_px(TABLE_FOOTNOTE_FONT_PT)
TABLE_TITLE_FONT_PX    <- table_pt_to_px(TABLE_TITLE_FONT_PT)
TABLE_SUBTITLE_FONT_PX <- table_pt_to_px(TABLE_SUBTITLE_FONT_PT)
TABLE_NOTE_FONT_PX     <- table_pt_to_px(TABLE_SOURCE_FONT_PT)
TABLE_STUB_WIDTH_PX    <- 210

# Every exported table is publication-width at figure DPI. Height is allowed to
# vary with row count/content; width, DPI, typography, padding, and line-height
# are invariant across the full table set.
TABLE_EXPORT_WIDTH_TOLERANCE_PX <- 2L

# Export audit registry populated by save_table().
TABLE_EXPORT_AUDIT <- tibble::tibble(
  name = character(),
  profile = character(),
  size = character(),
  css_width_px = integer(),
  export_zoom = double(),
  expected_width_px = integer(),
  actual_width_px = integer(),
  actual_height_px = integer(),
  overflow_px = double(),
  body_font_pt = double(),
  header_font_pt = double(),
  column_label_font_pt = double(),
  spanner_font_pt = double(),
  title_font_pt = double(),
  source_font_pt = double(),
  row_padding_pt = double(),
  column_padding_pt = double(),
  heading_padding_pt = double(),
  source_padding_pt = double(),
  body_lineheight = double(),
  header_lineheight = double(),
  title_lineheight = double(),
  source_lineheight = double(),
  status = character()
)

reset_table_export_audit <- function() {
  TABLE_EXPORT_AUDIT <<- TABLE_EXPORT_AUDIT[0, ]
  invisible(TABLE_EXPORT_AUDIT)
}

get_table_export_audit <- function() {
  TABLE_EXPORT_AUDIT
}

# Canvas geometry only. CSS density is derived from pixel width / physical width,
# so every profile inserts at WORD_TABLE_WIDTH_IN with the same printed type size.
make_table_profile <- function(width_px, stub_px) {
  list(
    width_px = width_px,
    stub_px  = stub_px,
    css_dpi  = width_px / WORD_TABLE_WIDTH_IN
  )
}

TABLE_WIDTH_PROFILES <- list(
  standard  = make_table_profile(TABLE_CSS_WIDTH_PX, 210),
  wide      = make_table_profile(900,                260),
  ultrawide = make_table_profile(1080,               285)
)

table_profile <- function(profile = NULL) {
  profile <- profile %||% "standard"
  if (!profile %in% names(TABLE_WIDTH_PROFILES)) {
    stop("Unknown table width profile: ", profile,
         ". Use one of: ", paste(names(TABLE_WIDTH_PROFILES), collapse = ", "))
  }
  TABLE_WIDTH_PROFILES[[profile]]
}

with_table_profile <- function(gt_tbl, profile = "standard") {
  attr(gt_tbl, "tpa_profile") <- profile
  gt_tbl
}

# Hard-code the stub/row-label column for a particular table while keeping
# the total table width fixed. The remaining visible data columns are resized
# evenly inside apply_table_profile().
#
# Example:
#   tab_006d <- build_change_table_wide("tab_006d", meta) %>%
#     with_table_profile("ultrawide") %>%
#     with_stub_width(340)
#
# Or at export time:
#   save_table(tab_006d, "chapter1_tab_006d", profile = "ultrawide", stub_width_px = 340)
with_stub_width <- function(gt_tbl, stub_width_px) {
  attr(gt_tbl, "tpa_stub_width_px") <- stub_width_px
  gt_tbl
}

get_gt_data_cols <- function(gt_tbl) {
  boxhead <- gt_tbl[["_boxhead"]]
  
  if (is.data.frame(boxhead) && all(c("var", "type") %in% names(boxhead))) {
    cols <- boxhead$var[boxhead$type == "default"]
    cols <- cols[!is.na(cols)]
    if (length(cols) > 0) return(cols)
  }
  
  data <- gt_tbl[["_data"]]
  if (is.data.frame(data)) return(names(data))
  
  character(0)
}

get_gt_data_col_count <- function(gt_tbl) {
  max(1, length(get_gt_data_cols(gt_tbl)))
}

# ---------------------------------------------------------------------------
# Content-aware column sizing
# ---------------------------------------------------------------------------
# Layout wrapping and dead whitespace both come from splitting the data area
# EVENLY across columns: a "2015" column and a "+27.0 pp" column then get the
# same width, so the wide one wraps while the narrow one wastes space. These
# helpers measure each column's natural single-line width from its own header,
# spanner share, and formatted cell text, then hand that to
# scale_columns_to_width(), which gives every column at least what it needs and
# distributes the remaining slack proportionally. Total width still lands on
# p$width_px exactly, so the export-width contract and the layout audit are
# unaffected; row height and every point size are untouched.

# Average glyph advance as a fraction of font size, by character class. Gotham
# is a fairly wide humanist sans; these are deliberate slight over-estimates so
# text gets a hair more room than the strict minimum rather than a hair less.
.tpa_char_em <- function(ch) {
  if (grepl("[MW@]", ch))                 return(0.92)
  if (grepl("[mw]", ch))                  return(0.82)
  if (grepl("[A-Z0-9]", ch))              return(0.62)
  if (grepl("[%+\u2212\u2013\u2014]", ch)) return(0.60)  # % + minus en/em dash
  if (grepl("[ ]", ch))                    return(0.30)
  if (grepl("[.,'\u2019:;|!]", ch))        return(0.28)
  if (grepl("[ijltfIr()\\[\\]]", ch))      return(0.34)
  0.55                                                   # ordinary lowercase
}

# Width in px of a single line of text at a given point size and CSS density.
tpa_text_width_px <- function(text, pt, css_dpi, bold = FALSE) {
  text <- as.character(text)
  text[is.na(text)] <- ""
  if (length(text) == 0) return(0)
  px_per_pt <- css_dpi / 72
  bold_factor <- if (bold) 1.06 else 1.0
  vapply(text, function(s) {
    if (!nzchar(s)) return(0)
    # Strip any HTML/markdown the stub may carry (flag <img>, rank <span>);
    # image width is handled separately by the caller.
    s <- gsub("<[^>]*>", "", s)
    chars <- strsplit(s, "", fixed = TRUE)[[1]]
    ems <- sum(vapply(chars, .tpa_char_em, numeric(1)))
    ems * pt * px_per_pt * bold_factor
  }, numeric(1), USE.NAMES = FALSE)
}

# gt stores the display label per column in _boxhead$column_label (a list).
.tpa_col_label <- function(gt_tbl, var) {
  bh <- gt_tbl[["_boxhead"]]
  if (!is.data.frame(bh) || !"var" %in% names(bh)) return(var)
  i <- match(var, bh$var)
  if (is.na(i)) return(var)
  lbl <- bh$column_label[[i]]
  if (is.null(lbl) || length(lbl) == 0) return(var)
  as.character(lbl)[1]
}

# Natural single-line width (px) that a data column needs: the wider of its
# header label (headers render uppercase) and its widest formatted cell.
# Spanner labels are intentionally NOT forced onto one column, because a spanner
# sits above a GROUP of columns and its width is shared; forcing it per-column
# would blow narrow year columns up. A modest share is added so a long spanner
# over a single column (rare) still gets some help.
.tpa_col_natural_px <- function(gt_tbl, var, header_pt, body_pt, css_dpi,
                                pad_px, body_bold = FALSE) {
  data <- gt_tbl[["_data"]]
  vals <- if (is.data.frame(data) && var %in% names(data)) data[[var]] else character(0)
  
  # The real formatted strings live in gt's render layer, but the columns here
  # are short and predictable (shares, pp, signed integers, $B). Estimate the
  # DISPLAYED width, not the raw value: a numeric gains a sign, thousands
  # separators, a decimal, and a suffix. Measuring the bare number under-sizes
  # change columns and re-introduces the "+27.0 pp" wrap; assuming " pp" for
  # every column over-sizes plain share columns and starves everyone. So infer
  # the suffix and sign from the column's own label and magnitude.
  label_raw <- .tpa_col_label(gt_tbl, var)
  is_change <- grepl("change|net|\u0394", label_raw, ignore.case = TRUE)
  if (is.numeric(vals) && length(vals)) {
    mx <- suppressWarnings(max(abs(vals), na.rm = TRUE))
    if (!is.finite(mx)) mx <- 0
    signed   <- is_change || any(vals < 0, na.rm = TRUE)
    frac     <- any(abs(vals - round(vals)) > 1e-9, na.rm = TRUE)  # has decimals
    digits   <- max(1, floor(log10(max(mx, 1))) + 1)
    seps     <- (digits - 1) %/% 3
    suffix   <- if (is_change && frac) " pp" else if (frac) "%" else ""
    # a signed integer count column ($B, patents) keeps its sign but no suffix
    token <- paste0(
      if (signed) "+" else "",
      strrep("0", digits), strrep(",", seps),
      if (frac) ".0" else "",
      suffix
    )
    body_w <- tpa_text_width_px(token, body_pt, css_dpi, body_bold)
  } else {
    vals_chr <- format(vals, trim = TRUE, justify = "none")
    body_w <- if (length(vals_chr)) max(tpa_text_width_px(vals_chr, body_pt, css_dpi, body_bold)) else 0
  }
  
  label <- toupper(.tpa_col_label(gt_tbl, var))   # gt uppercases column labels
  head_w <- tpa_text_width_px(label, header_pt, css_dpi, bold = TRUE)
  
  max(body_w, head_w) + pad_px
}

# Stub width has to be decided ONCE and then handed to BOTH the CSS written by
# fixed_table_width() and the cols_width() calls made by
# scale_columns_to_width(). Each used to compute its own: the CSS pinned the
# width the caller REQUESTED while the column distribution worked from the
# width it had CLAMPED. Whenever those disagreed the column widths summed past
# the table width and the right-hand columns rendered outside the captured
# viewport -- the clipping visible in tables 1.01, 3.01, 3.02 and 5.02.
#
# The function is idempotent: feeding its own result back in returns the same
# number, so apply_table_profile() can resolve once and pass the result down.
resolve_stub_width <- function(gt_tbl,
                               profile = "standard",
                               stub_width_px = NULL,
                               min_data_col_px = 34) {
  p <- table_profile(profile)
  requested <- stub_width_px %||% attr(gt_tbl, "tpa_stub_width_px") %||% p$stub_px
  explicit  <- !is.null(stub_width_px) || !is.null(attr(gt_tbl, "tpa_stub_width_px"))
  
  data_cols   <- get_gt_data_cols(gt_tbl)
  n_data_cols <- max(1, length(data_cols))
  total_px    <- p$width_px
  
  col_pad_pt <- tryCatch(PUB$spacing$table_col_pad_pt, error = function(e) 4)
  pad_px     <- 2 * pub_pt_to_px(col_pad_pt, p$css_dpi) + 6
  
  # Stub natural width from its own longest label. gt does NOT uppercase stub
  # cells, so measure them as-is; ignore embedded images/markup (measured as
  # text-only) and add a flat allowance when the stub clearly carries a flag.
  data <- gt_tbl[["_data"]]
  stub_var <- {
    bh <- gt_tbl[["_boxhead"]]
    sv <- if (is.data.frame(bh) && "type" %in% names(bh)) bh$var[bh$type == "stub"] else character(0)
    sv <- sv[!is.na(sv)]
    if (length(sv)) sv[1] else NA_character_
  }
  stub_natural <- NA_real_
  if (!is.na(stub_var) && is.data.frame(data) && stub_var %in% names(data)) {
    stub_vals <- as.character(data[[stub_var]])
    has_img   <- any(grepl("<img", stub_vals, fixed = TRUE))
    sw <- max(tpa_text_width_px(stub_vals, TABLE_DATA_FONT_PT, p$css_dpi), 0)
    if (has_img) sw <- sw + pub_pt_to_px(16, p$css_dpi)  # flag/icon allowance
    stub_natural <- sw + pad_px
  }
  
  # A pinned stub width is an UPPER bound: it may shrink toward its natural size
  # when the data columns want the room, but never grows past what the labels
  # need. Always clamp so data columns keep their floor.
  max_stub_width_px <- max(60, total_px - (n_data_cols * min_data_col_px))
  stub_target <- if (explicit) {
    if (is.finite(stub_natural)) min(requested, max(stub_natural, min_data_col_px))
    else requested
  } else if (is.finite(stub_natural)) {
    stub_natural
  } else {
    requested
  }
  
  max(60, min(stub_target, max_stub_width_px))
}

scale_columns_to_width <- function(gt_tbl,
                                   profile = "standard",
                                   stub_width_px = NULL,
                                   min_data_col_px = 34) {
  p <- table_profile(profile)
  
  data_cols <- get_gt_data_cols(gt_tbl)
  n_data_cols <- max(1, length(data_cols))
  total_px <- p$width_px
  
  # Physical padding (both sides) converted to this profile's CSS density, so
  # the natural-width estimate includes the same cell padding gt will add.
  col_pad_pt <- tryCatch(PUB$spacing$table_col_pad_pt, error = function(e) 4)
  pad_px <- 2 * pub_pt_to_px(col_pad_pt, p$css_dpi) + 6  # +6px safety for borders
  
  # ---- Natural widths --------------------------------------------------------
  header_pt <- TABLE_COLUMN_LABEL_FONT_PT
  body_pt   <- TABLE_DATA_FONT_PT
  
  natural <- vapply(
    data_cols,
    function(v) .tpa_col_natural_px(gt_tbl, v, header_pt, body_pt, p$css_dpi, pad_px),
    numeric(1)
  )
  natural <- pmax(natural, min_data_col_px)
  
  # ---- Choose the stub width -------------------------------------------------
  stub_w <- resolve_stub_width(
    gt_tbl,
    profile         = profile,
    stub_width_px   = stub_width_px,
    min_data_col_px = min_data_col_px
  )
  
  # ---- Distribute the data area proportionally -------------------------------
  avail <- total_px - stub_w
  # Guarantee every column its natural single-line width if they fit; if the
  # naturals overflow the available area (very dense tables), scale them down
  # proportionally but never below the floor.
  nat_sum <- sum(natural)
  if (nat_sum <= avail) {
    slack <- avail - nat_sum
    # give slack in proportion to natural width so wide columns breathe a bit
    # more than narrow ones, which reads more evenly than a flat top-up
    widths <- natural + slack * (natural / nat_sum)
  } else {
    scaled <- natural / nat_sum * avail
    widths <- pmax(scaled, min_data_col_px)
    # Flooring can push the sum back over `avail`. Reclaim the excess from the
    # columns that are still above the floor, proportional to their headroom, in
    # a single vectorized pass so this can never loop. If the floors alone
    # already exceed avail (table genuinely too dense to fit even at the
    # minimum), leave every column at the floor and let the table be as wide as
    # its floors require -- a fixable data/height problem, never a hang.
    over <- sum(widths) - avail
    if (over > 0) {
      headroom <- pmax(widths - min_data_col_px, 0)
      hr_sum <- sum(headroom)
      if (hr_sum > 0) {
        widths <- widths - headroom / hr_sum * min(over, hr_sum)
      }
    }
  }
  
  # Integer widths reconciled so the row sums EXACTLY to total_px (stub + data),
  # keeping the fixed-layout table on its publication width with no drift.
  # Distribute the rounding remainder one pixel at a time across a FIXED number
  # of columns (|deficit| <= n by construction after floor(), so this is a
  # single bounded pass -- no while loop, no chance of spinning).
  widths <- floor(widths)
  deficit <- (total_px - stub_w) - sum(widths)
  if (deficit != 0 && length(widths) > 0) {
    ord <- order(widths, decreasing = (deficit > 0))
    step <- sign(deficit)
    n <- length(widths)
    for (j in seq_len(abs(deficit))) {
      idx <- ord[((j - 1) %% n) + 1]
      widths[idx] <- widths[idx] + step
    }
  }
  names(widths) <- data_cols
  
  # If the floors alone still overflow, the table genuinely cannot fit at this
  # profile. Say so at knit time: silently overflowing is what produced tables
  # whose right-hand columns fell outside the captured PNG.
  overflow_px <- max(0, (stub_w + sum(widths)) - total_px)
  if (overflow_px > 0) {
    warning(
      "Table columns overflow the ", profile, " profile by ", round(overflow_px),
      " px; the rightmost column will be clipped in the exported PNG. ",
      "Widen the profile, shorten the stub labels, or drop a column.",
      call. = FALSE
    )
  }
  
  # ---- Apply -----------------------------------------------------------------
  out <- tryCatch(
    gt_tbl %>% cols_width(stub() ~ px(stub_w)),
    error = function(e) {
      warning("Could not set the stub width via cols_width(); falling back to ",
              "the profile CSS width. Stub and data columns may disagree.",
              call. = FALSE)
      gt_tbl
    }
  )
  for (col in data_cols) {
    w <- widths[[col]]
    out <- tryCatch(
      out %>% cols_width(rlang::new_formula(rlang::sym(col), rlang::expr(px(!!w)))),
      error = function(e) {
        warning("Could not set the width of column '", col, "'.", call. = FALSE)
        out
      }
    )
  }
  
  attr(out, "tpa_overflow_px") <- overflow_px
  attr(out, "tpa_actual_stub_width_px") <- stub_w
  attr(out, "tpa_actual_data_width_px") <- if (length(widths)) stats::median(widths) else NA_real_
  attr(out, "tpa_actual_col_widths_px") <- widths
  out
}

infer_table_profile <- function(name = NULL, gt_tbl = NULL) {
  explicit <- attr(gt_tbl, "tpa_profile")
  if (!is.null(explicit)) return(explicit)
  "standard"
}

TABLE_BG_STRIPE         <- "#F5F2EC"
TABLE_TEXT_COLOR        <- "grey25"
TABLE_MUTED_TEXT_COLOR  <- "grey45"
TABLE_CHANGE_PALETTE    <- c(tpa_colors[2], "white", tpa_colors[1])
TABLE_INTENSITY_PALETTE <- c(TPA_RED_LIGHT, tpa_colors[1])

# ---- Small reusable styling helpers ---------------------------------------

with_size <- function(gt_tbl, size = "standard") {
  attr(gt_tbl, "tpa_size") <- size
  gt_tbl
}

change_domain <- function(x) {
  lim <- suppressWarnings(max(abs(x), na.rm = TRUE))
  if (!is.finite(lim) || lim == 0) lim <- 1
  c(-lim, lim)
}

# All change-bearing tables use the same input:
#   "relative" = (last - first) / first * 100
#   "ppt"      = last - first, expressed in the displayed value's native units
#                (percentage points for shares; absolute units for counts/dollars).
# "absolute" remains accepted as a backward-compatible alias for "ppt".
resolve_change_type <- function(change_type = c("relative", "ppt")) {
  change_type <- change_type[1]
  if (identical(change_type, "absolute")) change_type <- "ppt"
  match.arg(change_type, c("relative", "ppt"))
}

table_change <- function(first, last, change_type = c("relative", "ppt")) {
  change_type <- resolve_change_type(change_type)
  out <- if (change_type == "relative") {
    (last - first) / first * 100
  } else {
    last - first
  }
  ifelse(is.finite(out), out, NA_real_)
}

# ---------------------------------------------------------------------------
# Shared change vocabulary
# ---------------------------------------------------------------------------
# One source of truth for how a change is LABELED, FORMATTED, and DESCRIBED.
# Each builder used to spell these out locally, which is how the set ended up
# with "Change", "Net Change", "Net change", and "Net" all naming the same
# operation, and how table 2.02 came to print percentage-point values with a
# "%" suffix.
#
# The label rule is semantic rather than cosmetic:
#   interior_deltas = FALSE -> "Change"     (the only change column; there is
#                                            nothing to distinguish it from)
#   interior_deltas = TRUE  -> "Net Change" (per-period delta columns are also
#                                            present, so the full-window figure
#                                            needs a name of its own)
CHANGE_LABEL_SIMPLE <- "Change"
CHANGE_LABEL_NET    <- "Net Change"
TABLE_DELTA_MARK    <- "\u0394"

change_col_label <- function(interior_deltas = FALSE) {
  if (isTRUE(interior_deltas)) CHANGE_LABEL_NET else CHANGE_LABEL_SIMPLE
}

# Interior per-period delta header: the period followed by the delta mark.
delta_col_label <- function(period) {
  paste0(period, " ", TABLE_DELTA_MARK)
}

# Cell suffix. A "ppt" change is in percentage points and must never print "%".
change_pattern <- function(change_type) {
  if (resolve_change_type(change_type) == "relative") "{x}%" else "{x} pp"
}

# Footnote prose for the change operation. `units` distinguishes a share (where
# a native-unit change is measured in percentage points) from a count or dollar
# figure (where it is an absolute change).
change_phrase <- function(change_type, units = c("share", "count")) {
  units <- match.arg(units)
  if (resolve_change_type(change_type) == "relative") {
    "relative change"
  } else if (units == "count") {
    "absolute change"
  } else {
    "percentage-point change"
  }
}

# Join sentence fragments and guarantee a single terminal period. paste() with a
# bare "." as its own argument produced the "... 2000 and 2025 ." artifact in
# table 2.01; this collapses that whitespace instead.
end_sentence <- function(...) {
  txt <- paste(..., sep = " ")
  txt <- gsub("[[:space:]]+", " ", trimws(txt))
  txt <- gsub("[[:space:]]+([.,;:])", "\\1", txt)
  if (!grepl("[.!?]$", txt)) txt <- paste0(txt, ".")
  txt
}

# The "Data from ... / Chart by ..." block comes from the figure metadata, so
# terminal punctuation drifted between entries. Normalize per line so every
# table's source block reads the same however the metadata row was typed. A
# trailing markdown emphasis marker is preserved so the period lands inside it.
normalize_caption <- function(caption) {
  if (is.null(caption)) return(caption)
  joined <- paste(caption, collapse = "\n")
  if (!nzchar(trimws(joined))) return(caption)
  lines <- strsplit(joined, "\n", fixed = TRUE)[[1]]
  out <- vapply(lines, function(ln) {
    raw <- trimws(ln)
    if (!nzchar(raw)) return(ln)
    m     <- regmatches(raw, regexpr("[*_]+$", raw))
    close <- if (length(m)) m[[1]] else ""
    core  <- sub("[*_]+$", "", raw)
    if (grepl("[.!?]$", core)) return(raw)
    paste0(core, ".", close)
  }, character(1), USE.NAMES = FALSE)
  paste(out, collapse = "\n")
}

style_stub_left <- function(gt_tbl) {
  gt_tbl %>%
    tab_style(
      style     = cell_text(align = "left"),
      locations = cells_stub(rows = TRUE)
    )
}

style_bold_columns <- function(gt_tbl, columns) {
  gt_tbl %>%
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body(columns = {{ columns }})
    )
}

style_spanner_rule <- function(gt_tbl, id, color = tpa_colors[1]) {
  gt_tbl %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      id, color
    ))
}

style_change_color <- function(gt_tbl, columns, domain, na_color = "white") {
  gt_tbl %>%
    data_color(
      columns = {{ columns }},
      fn = scales::col_numeric(
        palette  = TABLE_CHANGE_PALETTE,
        domain   = domain,
        na.color = na_color
      )
    )
}

table_publication_css <- function(profile = "standard") {
  p <- table_profile(profile)
  pt_to_px_profile <- function(pt) pub_pt_to_px(pt, p$css_dpi)
  
  data_px    <- pt_to_px_profile(TABLE_DATA_FONT_PT)
  column_px  <- pt_to_px_profile(TABLE_COLUMN_LABEL_FONT_PT)
  spanner_px <- pt_to_px_profile(TABLE_SPANNER_FONT_PT)
  
  sprintf(
    paste0(
      ".gt_table { line-height: %.3f !important; }\n",
      
      # BODY / DATA VALUES / STUB LABELS
      # Explicit CSS here is intentional: it prevents gt themes or individual
      # builders from making the visible data values smaller than the shared
      # publication setting.
      ".gt_table tbody td, .gt_table .gt_stub, .gt_table .gt_rowname { ",
      "font-size: %spx !important; line-height: %.3f !important; ",
      "vertical-align: middle !important; }\n",
      
      # COLUMN NAMES
      # All ordinary column headings get exactly one physical size regardless
      # of standard/wide/ultrawide profile.
      ".gt_table .gt_col_heading { font-size: %spx !important; ",
      "line-height: %.3f !important; }\n",
      
      # SPANNERS
      # Spanners use the same point size as column names. Weight may differ,
      # but size does not.
      ".gt_table .gt_column_spanner, .gt_table .gt_column_spanner_outer { ",
      "font-size: %spx !important; line-height: %.3f !important; ",
      "font-weight: 700 !important; }\n",
      
      # Make ordinary wrapper spans inherit the forced heading size while
      # preserving semantic <sup>/<sub> sizing for footnote markers.
      ".gt_table .gt_col_heading span:not(.gt_footnote_marks), ",
      ".gt_table .gt_column_spanner span:not(.gt_footnote_marks) { ",
      "font-size: inherit !important; }\n",
      
      ".gt_table .gt_heading .gt_title { line-height: %.3f !important; margin: 0 !important; }\n",
      ".gt_table .gt_heading .gt_subtitle { line-height: %.3f !important; margin: 0 !important; }\n",
      ".gt_table .gt_source_notes, .gt_table .gt_footnote { line-height: %.3f !important; }\n",
      ".gt_table .gt_stub, .gt_table .gt_rowname, .gt_table .gt_col_heading, ",
      ".gt_table .gt_column_spanner, .gt_table .gt_source_notes, .gt_table .gt_footnote { ",
      "letter-spacing: normal !important; }"
    ),
    TABLE_BODY_LINEHEIGHT,
    data_px,
    TABLE_BODY_LINEHEIGHT,
    column_px,
    TABLE_HEADER_LINEHEIGHT,
    spanner_px,
    TABLE_HEADER_LINEHEIGHT,
    TABLE_TITLE_LINEHEIGHT,
    TABLE_SUBTITLE_LINEHEIGHT,
    TABLE_SOURCE_LINEHEIGHT
  )
}

fixed_table_width <- function(gt_tbl, profile = NULL, width_px = NULL, stub_width_px = NULL) {
  p <- table_profile(profile)
  width_px <- width_px %||% p$width_px
  stub_width_px <- stub_width_px %||% p$stub_px
  
  gt_tbl %>%
    tab_options(
      table.width     = px(width_px),
      container.width = px(width_px)
    ) %>%
    opt_css(css = sprintf(
      paste0(
        "html, body { margin: 0 !important; padding: 0 !important; ",
        "width: %spx !important; overflow: visible !important; }\n",
        ".gt_table { width: %spx !important; max-width: %spx !important; ",
        "table-layout: fixed !important; border-collapse: collapse !important; }\n",
        ".gt_table * { box-sizing: border-box !important; }\n",
        ".gt_table td, .gt_table th { box-sizing: border-box !important; overflow: visible !important; }\n",
        ".gt_table .gt_stub, .gt_table .gt_rowname { ",
        "width: %spx !important; min-width: %spx !important; max-width: %spx !important; ",
        "white-space: normal !important; overflow-wrap: normal !important; word-break: normal !important; hyphens: none !important; }\n",
        ".gt_table .gt_stub *, .gt_table .gt_rowname * { word-break: normal !important; overflow-wrap: normal !important; hyphens: none !important; }\n",
        ".gt_table .gt_row { white-space: normal !important; overflow-wrap: normal !important; word-break: normal !important; hyphens: none !important; }\n",
        # Headings wrap on break-word, unlike body cells. An unbreakable label
        # such as "BUSINESS/MGMT" has no natural break opportunity, so with
        # overflow-wrap:normal it spilled out of its cell and printed on top of
        # the neighbouring header (table 2.02). Breaking mid-token is the lesser
        # evil: a wrapped header is legible, an overprinted one is not.
        ".gt_table .gt_col_heading { white-space: normal !important; overflow-wrap: break-word !important; word-break: normal !important; hyphens: none !important; }\n",
        ".gt_table .gt_source_notes { white-space: normal !important; overflow-wrap: break-word !important; word-break: normal !important; }"
      ),
      width_px, width_px, width_px,
      stub_width_px, stub_width_px, stub_width_px
    ))
}

apply_table_profile <- function(gt_tbl, profile = "standard", stub_width_px = NULL) {
  p <- table_profile(profile)
  
  # Convert publication point sizes to CSS pixels at this profile's internal
  # density. save_table() applies the reciprocal export zoom, so the same point
  # size becomes the same physical size in every final PNG.
  pt_to_px_profile <- function(pt) pub_pt_to_px(pt, p$css_dpi)
  
  requested_stub_width_px <-
    stub_width_px %||%
    attr(gt_tbl, "tpa_stub_width_px") %||%
    p$stub_px
  
  # Resolve once so the CSS and the column widths are derived from the SAME
  # number (see resolve_stub_width()).
  resolved_stub_width_px <- resolve_stub_width(
    gt_tbl,
    profile       = profile,
    stub_width_px = requested_stub_width_px
  )
  
  gt_tbl %>%
    tab_options(
      table.font.size =
        px(pt_to_px_profile(TABLE_DATA_FONT_PT)),
      
      column_labels.font.size =
        px(pt_to_px_profile(TABLE_COLUMN_LABEL_FONT_PT)),
      
      heading.title.font.size =
        px(pt_to_px_profile(TABLE_TITLE_FONT_PT)),
      
      heading.subtitle.font.size =
        px(pt_to_px_profile(TABLE_SUBTITLE_FONT_PT)),
      
      source_notes.font.size =
        px(pt_to_px_profile(TABLE_SOURCE_FONT_PT)),
      
      footnotes.font.size =
        px(pt_to_px_profile(TABLE_FOOTNOTE_FONT_PT)),
      
      table.width     = px(p$width_px),
      container.width = px(p$width_px),
      
      column_labels.padding =
        px(pt_to_px_profile(PUB$spacing$table_col_pad_pt)),
      
      data_row.padding =
        px(pt_to_px_profile(PUB$spacing$table_row_pad_pt)),
      
      heading.padding =
        px(pt_to_px_profile(PUB$spacing$table_heading_pad_pt)),
      
      source_notes.padding =
        px(pt_to_px_profile(PUB$spacing$table_source_pad_pt)),
      
      footnotes.padding =
        px(pt_to_px_profile(PUB$spacing$table_source_pad_pt))
    ) %>%
    opt_css(css = table_publication_css(profile)) %>%
    fixed_table_width(
      profile = profile,
      stub_width_px = resolved_stub_width_px
    ) %>%
    scale_columns_to_width(
      profile = profile,
      stub_width_px = resolved_stub_width_px
    ) %>%
    with_table_profile(profile)
}

set_default_stub_width <- function(gt_tbl, width_px = TABLE_STUB_WIDTH_PX) {
  # Most project tables use rowname_col=..., which renders as a gt stub.
  # Give that first label column enough room by default; if a future table has
  # no stub, safely leave it unchanged.
  tryCatch(
    gt_tbl %>% cols_width(stub() ~ px(width_px)),
    error = function(e) gt_tbl
  )
}
split_table_width <- function(stub_width_px = TABLE_STUB_WIDTH_PX,
                              n_numeric_cols,
                              table_width_px = TABLE_CSS_WIDTH_PX) {
  list(
    stub = stub_width_px,
    num  = (table_width_px - stub_width_px) / n_numeric_cols
  )
}

# ---- Theme ----------------------------------------------------------------

theme_gt_tpa <- function(gt_tbl, meta = NULL) {
  tbl <- gt_tbl %>%
    gt_theme_538() %>%
    opt_table_font(font = list("Gotham", default_fonts())) %>%
    # Base64-embed Gotham and hard-override the family on every element so the
    # font survives webshot/headless-Chrome PNG export and beats gt_theme_538's
    # own font. gotham_gt_css() comes from fonts.R.
    opt_css(css = gotham_gt_css()) %>%
    tab_style(
      style     = cell_borders(sides = "top", color = tpa_colors[1], weight = px(2)),
      locations = cells_column_labels(everything())
    ) %>%
    tab_options(
      table.font.size        = px(TABLE_FONT_PX),
      table.font.color       = TABLE_TEXT_COLOR,
      table.background.color = "white",
      
      column_labels.font.weight    = "bold",
      column_labels.font.size      = px(TABLE_HEADER_FONT_PX),
      column_labels.text_transform = "uppercase",
      column_labels.padding        = px(table_pt_to_px(PUB$spacing$table_col_pad_pt)),
      
      row.striping.background_color = TABLE_BG_STRIPE,
      data_row.padding              = px(table_pt_to_px(PUB$spacing$table_row_pad_pt)),
      
      heading.title.font.size    = px(TABLE_TITLE_FONT_PX),
      heading.title.font.weight  = "bold",
      heading.subtitle.font.size = px(TABLE_SUBTITLE_FONT_PX),
      heading.padding            = px(table_pt_to_px(PUB$spacing$table_heading_pad_pt)),
      heading.align              = "left",
      
      source_notes.font.size = px(TABLE_NOTE_FONT_PX),
      source_notes.padding   = px(table_pt_to_px(PUB$spacing$table_source_pad_pt)),
      
      footnotes.font.size = px(TABLE_FOOTNOTE_FONT_PX),
      footnotes.padding   = px(table_pt_to_px(PUB$spacing$table_source_pad_pt))
    ) %>%
    opt_align_table_header(align = "left") %>%
    # One footnote-mark system for the whole set. build_ranking_delta_table()
    # used to switch table 5.02 to *, dagger, double-dagger whenever it carried
    # a cell-level note, so that one table numbered its footnotes differently
    # from every other table in the report.
    opt_footnote_marks(marks = "numbers")
  
  if (!is.null(meta)) {
    tbl <- tbl %>%
      tab_header(title = gt::html(meta$title), subtitle = meta$subtitle) %>%
      tab_source_note(source_note = md(normalize_caption(meta$caption))) %>%
      tab_style(
        style     = cell_text(color = TABLE_MUTED_TEXT_COLOR),
        locations = cells_title(groups = "subtitle")
      ) %>%
      tab_style(
        style     = cell_text(color = TABLE_MUTED_TEXT_COLOR),
        locations = cells_source_notes()
      )
  }
  
  tbl
}


# ---- Country flag helpers -----------------------------------
# Circular flag PNGs live in inputs/flags/<slug>.png, paths relative to the knit
# working directory (same convention as build_patent_company_delta_table).
# local_image() base64-embeds each file, so output HTML/PNG stays portable.
FLAG_SLUG <- c(
  "Australia"     = "australia",
  "Brazil"         = "brazil-",       # file is literally brazil-.png
  "Canada"         = "canada",
  "China"          = "china",
  "Colombia"       = "colombia",
  "France"         = "france",
  "Germany"        = "germany",
  "Hong Kong"      = "hong-kong",
  "India"          = "india",
  "Indonesia"      = "indonesia",
  "Japan"          = "japan",
  "Mexico"         = "mexico",
  "Nepal"          = "nepal",
  "Nigeria"        = "nigeria",
  "Pakistan"       = "pakistan",
  "Saudi Arabia"   = "saudi-arabia",
  "South Korea"    = "south-korea",
  "Taiwan"         = "taiwan",
  "Turkey"         = "turkey",
  "Turkey/Türkiye" = "turkey",
  "United Kingdom" = "united-kingdom",
  "United States"  = "united-states",
  "Vietnam"        = "vietnam"
)

flag_path_for <- function(country) {
  slug <- unname(FLAG_SLUG[as.character(country)])
  ifelse(is.na(slug), NA_character_, file.path(PATHS$flags, paste0(slug, ".png")))
}

# Prepend the flag image to the stub label. Any country with no slug or no
# file on disk (e.g. Australia) just renders as its name, no broken image.
# Pair with fmt_markdown(columns = <stub_col>) right after gt().
add_country_flag_stub <- function(df, stub_col, height = 14) {
  paths <- flag_path_for(df[[stub_col]])
  imgs  <- vapply(paths, function(p) {
    if (is.na(p) || !file.exists(p)) return("")
    paste0(gt::local_image(p, height = height), "&nbsp;&nbsp;")
  }, character(1))
  df[[stub_col]] <- paste0(imgs, as.character(df[[stub_col]]))
  df
}

# ---- Regional enrollment change table -----------------------
# Expects a CSV with:
# Year, Asia, Africa, Sub-saharan, Europe,
# Latin America and Carribian, North America,
# Oceania, Middle East and North Africa, Stateless

build_region_table <- function(name, meta) {
  
  df <- read_fig(name)
  
  first_year <- min(df$Year, na.rm = TRUE)
  last_year <- max(df$Year, na.rm = TRUE)
  
  first <- df %>%
    filter(Year == first_year)
  
  last <- df %>%
    filter(Year == last_year)
  
  region_cols <- c(
    "Asia",
    "Africa, Sub-Saharan",
    "Europe",
    "Latin America and Caribbean",
    "North America",
    "Oceania",
    "Middle East and North Africa"
  )
  
  first_total <- sum(first[region_cols], na.rm = TRUE)
  last_total <- sum(last[region_cols], na.rm = TRUE)
  
  tbl_df <- tibble(
    Region = region_cols,
    
    FirstYearCount = as.numeric(first[1, region_cols]),
    
    FirstYearShare =
      100 * as.numeric(first[1, region_cols]) / first_total,
    
    LastYearCount = as.numeric(last[1, region_cols]),
    
    LastYearShare =
      100 * as.numeric(last[1, region_cols]) / last_total
    
  ) %>%
    mutate(
      PctChangeCount = table_change(FirstYearCount, LastYearCount, "relative"),
      
      PctChangeShare = table_change(FirstYearShare, LastYearShare, "ppt")
    ) %>%
    arrange(desc(PctChangeShare))
  
  count_lim <- max(abs(tbl_df$PctChangeCount), na.rm = TRUE)
  share_lim <- max(abs(tbl_df$PctChangeShare), na.rm = TRUE)
  
  tbl_df %>%
    gt(rowname_col = "Region", id = name) %>%
    
    fmt_integer(
      columns = c(FirstYearCount, LastYearCount)
    ) %>%
    
    fmt_number(
      columns = c(FirstYearShare, LastYearShare),
      decimals = 1,
      pattern = "{x}%"
    ) %>%
    
    fmt_number(
      columns = PctChangeCount,
      decimals = 1,
      force_sign = TRUE,
      pattern = "{x}%"
    ) %>%
    
    fmt_number(
      columns = PctChangeShare,
      decimals = 1,
      force_sign = TRUE,
      pattern = "{x} pp"
    ) %>%
    
    tab_spanner(
      label = "Enrollment",
      columns = c(
        FirstYearCount,
        LastYearCount,
        PctChangeCount
      )
    ) %>%
    
    tab_spanner(
      label = "Share of Total",
      columns = c(
        FirstYearShare,
        LastYearShare,
        PctChangeShare
      )
    ) %>%
    
    cols_label(
      FirstYearCount = as.character(first_year),
      LastYearCount  = as.character(last_year),
      PctChangeCount = change_col_label(FALSE),
      
      FirstYearShare = as.character(first_year),
      LastYearShare  = as.character(last_year),
      PctChangeShare = change_col_label(FALSE)
    ) %>%
    
    data_color(
      columns = PctChangeCount,
      fn = scales::col_numeric(
        palette = c(tpa_colors[2], "white", tpa_colors[1]),
        domain = c(-count_lim, count_lim)
      )
    ) %>%
    
    data_color(
      columns = PctChangeShare,
      fn = scales::col_numeric(
        palette = c(tpa_colors[2], "white", tpa_colors[1]),
        domain = c(-share_lim, share_lim)
      )
    ) %>%
    
    cols_align(
      align = "right",
      columns = c(
        FirstYearCount,
        LastYearCount,
        PctChangeCount,
        FirstYearShare,
        LastYearShare,
        PctChangeShare
      )
    ) %>%
    
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = c(PctChangeCount, PctChangeShare)
      )
    ) %>%
    
    tab_footnote(
      footnote = end_sentence(
        "Counts represent total international students across all degree levels",
        "by region of origin. Shares are calculated as a percentage of all",
        "international students. Change is the relative change for enrollment",
        "and the percentage-point change for share, between",
        first_year, "and", last_year
      ),
      locations = cells_column_spanners(
        spanners = c("Enrollment", "Share of Total")
      )
    ) %>%
    
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      name, tpa_colors[1]
    )) %>%
    with_size("long")
  
}

# ---- STEM labor-force citizenship table ----------------------
# Used by tab_013. Source is fig_025. One row per labor-force
# category; columns are the U.S.-citizen/permanent-resident share
# and the temporary-visa-holder share (they sum to 100%, so only
# the temp-visa column carries information and gets the color scale).
# Row ordering and the Total-last placement happen upstream.
build_labor_citizenship_table <- function(df, name, meta) {
  
  # single continuous scale on the temp-visa share -- the meaningful
  # number -- so the S&E-core vs. periphery contrast reads as color
  df %>%
    gt(rowname_col = "Category", id = name) %>%
    fmt_number(columns = c(Citizen, TVH), decimals = 1, pattern = "{x}%") %>%
    cols_label(Citizen = "U.S. citizen or\npermanent resident",
               TVH     = "Temporary\nvisa holder") %>%
    gt_color_rows(columns  = TVH,
                  palette  = c(TPA_RED_LIGHT, tpa_colors[1]),
                  pal_type = "continuous") %>%
    cols_align(align = "right", columns = c(Citizen, TVH)) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = TVH)) %>%
    tab_style(style = cell_text(align = "left"),
              locations = cells_stub(rows = TRUE)) %>%
    tab_footnote(
      footnote = paste(
        "Each row is the citizenship split within that labor-force category;",
        "the two shares sum to 100%. Rows are ordered from occupations most",
        "directly tied to science and engineering to those least tied, with",
        "the overall total shown last."
      ),
      locations = cells_column_labels(columns = TVH)
    ) %>%
    theme_gt_tpa(meta = meta) %>%
    with_size("long")
}

# ---- H-1B top-10 employer table (union of both years) --------
# Used by tab_012. One flat table of every firm that ranked top-10
# in either the first or last year. Each row shows that firm's share
# in both years plus either relative or percentage-point change. Built from a df with
# Company, Y1, Y2, Change.
build_h1b_top10_table <- function(df, name, meta, first_year, last_year,
                                  change_type = c("relative", "ppt")) {
  
  change_type <- resolve_change_type(change_type)
  df <- df %>% mutate(Change = table_change(Y1, Y2, change_type))
  chg_lim <- max(abs(df$Change), na.rm = TRUE)
  
  df %>%
    gt(rowname_col = "Company", id = name) %>%
    fmt_number(columns = c(Y1, Y2), decimals = 1, pattern = "{x}%") %>%
    fmt_number(columns = Change, decimals = 1, force_sign = TRUE,
               pattern = if (change_type == "relative") "{x}%" else "{x} pp") %>%
    sub_missing(columns = Change, missing_text = "new") %>%
    cols_label(Y1 = as.character(first_year),
               Y2 = as.character(last_year),
               Change = change_col_label(FALSE)) %>%
    data_color(
      columns = Change,
      fn = scales::col_numeric(
        palette = c(tpa_colors[2], "white", tpa_colors[1]),
        domain  = c(-chg_lim, chg_lim),
        na.color = "white"
      )
    ) %>%
    cols_align(align = "right", columns = c(Y1, Y2, Change)) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = Change)) %>%
    tab_style(style = cell_text(align = "left"),
              locations = cells_stub(rows = TRUE)) %>%
    tab_footnote(
      footnote = end_sentence(
        "Share of all H-1B approvals in NAICS 54 (professional, scientific, and",
        "technical services). Firms shown ranked among the ten largest employers",
        "in", first_year, "or", paste0(last_year, "."), change_col_label(FALSE),
        "is the", change_phrase(change_type), "in share from", first_year, "to",
        last_year
      ),
      locations = cells_column_labels(columns = Change)
    ) %>%
    theme_gt_tpa(meta = meta) %>%
    with_size("standard")
}

# ---- Occupation share table (bookends + year-over-year change) ----
# Used by tab_011 and degree-level variants. Takes a df already
# pivoted wide with a stub column named Occupation, raw shares for
# the first and last year (level_cols, e.g. "Y2017", "Y2023"),
# year-over-year change columns for the interior years
# (chg_cols, e.g. "Chg2019", "Chg2021"), and a Change column.
# change_type determines relative versus percentage-point change. Year selection, recoding,
# and row ordering happen upstream in the calculation chunk -- this
# function only builds and styles. spanner_label and footnote are
# parametrized; defaults reproduce tab_011.
build_occupation_share_table <- function(df, name, meta, level_cols,
                                         spanner_label = "Temporary visa holder share",
                                         change_type = c("ppt", "relative"),
                                         footnote = NULL) {
  
  change_type <- resolve_change_type(change_type)
  if (is.null(footnote)) {
    footnote = end_sentence(
      "Each year column is the temporary visa holder share of employment within",
      "each STEM occupation category.",
      change_col_label(TRUE), "is the", change_phrase(change_type),
      "from", gsub("^Y", "", level_cols[1]), "to",
      gsub("^Y", "", level_cols[length(level_cols)]), ".",
      "Rows are ordered from occupations most directly tied to science and",
      "engineering to those least tied")
  }
  
  span_order  <- level_cols
  level_labels <- setNames(gsub("^Y", "", level_cols), level_cols)
  
  tbl <- df %>%
    gt(rowname_col = "Occupation", id = name) %>%
    fmt_number(columns = all_of(level_cols), decimals = 1, pattern = "{x}%") %>%
    fmt_number(columns = "Change", decimals = 1,
               force_sign = TRUE,
               pattern = if (change_type == "relative") "{x}%" else "{x} pp") %>%
    tab_spanner(label = spanner_label,
                columns = all_of(span_order)) %>%
    cols_label(!!!level_labels,
               Change = change_col_label(TRUE))
  
  lim <- max(abs(df[["Change"]]), na.rm = TRUE)
  tbl <- tbl %>%
    data_color(
      columns = "Change",
      fn = scales::col_numeric(
        palette = c(tpa_colors[2], "white", tpa_colors[1]),
        domain  = c(-lim, lim)
      )
    )
  
  tbl <- tbl %>%
    tab_style(
      style     = cell_fill(color = "white"),
      locations = cells_body(columns = all_of(level_cols))
    ) %>%
    tab_style(
      style     = cell_text(align = "left"),
      locations = cells_stub(rows = TRUE)
    ) %>%
    cols_align(align = "right", columns = c(all_of(level_cols), "Change"))
  
  tbl %>%
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body(columns = Change)
    ) %>%
    tab_footnote(
      footnote  = footnote,
      locations = cells_column_spanners(spanners = spanner_label)
    ) %>%
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      name, tpa_colors[1]
    )) %>%
    with_size("standard")
}

# ---- Build Employment Based Visa Processing Times ----
build_eb_combined_table <- function(meta,
                                    sources = c(`Rest of World` = "TAB605a",
                                                China            = "TAB605b",
                                                India            = "TAB605c"),
                                    years   = c(2005, 2010, 2015, 2020, 2025),
                                    keys    = c("all", "chn", "ind")) {
  
  to_months <- function(txt) {
    y <- as.numeric(str_match(txt, "(\\d+)\\s*years?")[, 2])
    m <- as.numeric(str_match(txt, "(\\d+)\\s*months?")[, 2])
    y * 12 + m
  }
  
  pull_country <- function(name, key) {
    read_fig(name) %>%
      mutate(Year = suppressWarnings(as.numeric(Year))) %>%
      filter(Year %in% years) %>%
      transmute(
        Year,
        !!paste0(key, "_EB1") := to_months(`EB1.1`),
        !!paste0(key, "_EB2") := to_months(`EB2.1`),
        !!paste0(key, "_EB3") := to_months(`EB3.1`)
      )
  }
  
  df <- tibble(Year = years)
  for (i in seq_along(sources))
    df <- df %>% left_join(pull_country(sources[[i]], keys[i]), by = "Year")
  
  num_cols <- paste0(rep(keys, each = 3), "_EB", 1:3)
  dom_max  <- max(as.matrix(df[num_cols]), na.rm = TRUE)
  df       <- df %>% mutate(Year = as.character(Year))
  
  tbl <- df %>%
    select(Year, all_of(num_cols)) %>%
    gt(rowname_col = "Year") %>%
    cols_label(!!!setNames(as.list(rep(c("EB1", "EB2", "EB3"), length(keys))),
                           num_cols)) %>%
    tab_spanner(label = names(sources)[1], columns = all_of(paste0(keys[1], "_EB", 1:3))) %>%
    tab_spanner(label = names(sources)[2], columns = all_of(paste0(keys[2], "_EB", 1:3))) %>%
    tab_spanner(label = names(sources)[3], columns = all_of(paste0(keys[3], "_EB", 1:3))) %>%
    fmt_number(columns = all_of(num_cols), decimals = 0) %>%
    gt_color_rows(columns  = all_of(num_cols),
                  palette  = c(TPA_RED_LIGHT, tpa_colors[1]),
                  pal_type = "continuous",
                  domain   = c(0.5, dom_max))
  
  # White background + black text for exact-zero cells, per column
  # (so a row's zero doesn't blank its non-zero cells)
  for (col in num_cols)
    tbl <- tab_style(tbl,
                     style = list(cell_fill(color = "white"),
                                  cell_text(color = "black")),
                     locations = cells_body(columns = all_of(col),
                                            rows = .data[[col]] == 0))
  
  tbl %>%
    cols_align(align = "center", columns = all_of(num_cols)) %>%
    tab_style(style = cell_text(align = "left"),
              locations = cells_stub(rows = TRUE)) %>%
    tab_footnote(
      footnote  = "Cell values are in months.",
      locations = cells_column_spanners(spanners = names(sources))
    ) %>%
    theme_gt_tpa(meta = meta) %>%
    with_size("standard")
}

# First-year / peak-year / last-year counts with the decline from peak.
# For count series that rise then fall (endpoints alone would hide the arc).
build_peak_table <- function(name, meta, first, last,
                             id_col = "Year", drop_below = NULL,
                             change_type = c("relative", "ppt")) {
  
  change_type <- resolve_change_type(change_type)
  raw <- read_fig(name) %>%
    rename(!!id_col := 1) %>%
    mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))
  
  cats <- setdiff(names(raw), id_col)
  
  rows <- lapply(cats, function(c) {
    v  <- raw[[c]]; yr <- raw[[id_col]]
    pk <- which.max(v)
    tibble(country = c,
           first_val = v[yr == first][1],
           peak_year = yr[pk],
           peak_val  = v[pk],
           last_val  = v[yr == last][1]) %>%
      mutate(from_peak = table_change(peak_val, last_val, change_type))
  }) %>% bind_rows()
  
  if (!is.null(drop_below))
    rows <- filter(rows, peak_val >= drop_below)
  
  rows <- arrange(rows, desc(peak_val))
  
  rows %>%
    gt(rowname_col = "country") %>%
    fmt_number(columns = c(first_val, peak_val, last_val), decimals = 0) %>%
    fmt_number(columns = from_peak, decimals = 0, force_sign = TRUE,
               pattern = if (change_type == "relative") "{x}%" else "{x}") %>%
    cols_label(first_val = as.character(first),
               peak_val  = "Peak",
               peak_year = "(year)",
               last_val  = as.character(last),
               from_peak = "From peak") %>%
    cols_move(columns = peak_year, after = peak_val) %>%
    fmt_number(columns = peak_year, decimals = 0, use_seps = FALSE) %>%
    data_color(columns = from_peak,
               fn = scales::col_numeric(
                 palette = c(tpa_colors[1], "white"),
                 domain  = c(min(rows$from_peak, 0, na.rm = TRUE), 0))) %>%
    cols_align(align = "center", columns = -country) %>%
    tab_style(cell_text(align = "left"), cells_stub(rows = TRUE)) %>%
    tab_footnote(
      footnote = end_sentence(
        "Peak is each country's highest single-year certification total;",
        "\u201cfrom peak\u201d is the", change_phrase(change_type, "count"), "to 2024"
      ),
      locations = cells_column_labels(columns = from_peak)
    ) %>%
    theme_gt_tpa(meta = meta) %>%
    with_size("standard")
}

# Certified-used vs certified-expired PERM certifications, two bookend years,
# the share that expired unused, and the point change in that share.
# ---- Certified-used vs certified-expired PERM certifications ----
build_perm_expiry_table <- function(meta, path = NULL,
                                    first = 2008, last = 2024,
                                    countries = c("India", "China",
                                                  "South Korea", "Mexico"),
                                    change_type = c("ppt", "relative")) {
  
  change_type <- resolve_change_type(change_type)
  if (is.null(path)) path <- file.path(DATA_DIR, "TAB604.csv")
  
  raw <- readr::read_csv(path, show_col_types = FALSE) %>%
    mutate(Count  = suppressWarnings(as.numeric(Count)),
           Status = str_trim(Status)) %>%
    filter(Status %in% c("Certified", "Certified-Expired"),
           Country %in% countries,
           Year %in% c(first, last))
  
  wide <- raw %>%
    select(Country, Year, Status, Count) %>%
    pivot_wider(names_from = c(Status, Year), values_from = Count,
                names_sep = "_") %>%
    mutate(
      Country = factor(Country, levels = countries),
      exp_first = .data[[paste0("Certified-Expired_", first)]],
      exp_last  = .data[[paste0("Certified-Expired_", last)]],
      use_first = .data[[paste0("Certified_", first)]],
      use_last  = .data[[paste0("Certified_", last)]],
      share_first = exp_first / (exp_first + use_first) * 100,
      share_last  = exp_last  / (exp_last  + use_last)  * 100,
      share_net   = table_change(share_first, share_last, change_type)
    ) %>%
    arrange(Country) %>%
    select(Country, use_first, exp_first, share_first,
           use_last,  exp_last,  share_last, share_net)
  
  lim_net <- max(abs(wide$share_net), na.rm = TRUE)
  
  # factor -> character so the flag markup isn't dropped by factor coercion
  wide <- wide %>% mutate(Country = as.character(Country))
  wide <- add_country_flag_stub(wide, "Country")
  
  wide %>%
    gt(rowname_col = "Country", id = "TAB604") %>%
    fmt_markdown(columns = "Country") %>%
    fmt_number(columns = starts_with(c("use_", "exp_")), decimals = 0) %>%
    fmt_number(columns = c(share_first, share_last), decimals = 0, pattern = "{x}%") %>%
    fmt_number(columns = share_net, decimals = 1, force_sign = TRUE,
               pattern = if (change_type == "relative") "{x}%" else "{x} pp") %>%
    cols_label(use_first = "Used", exp_first = "Expired", share_first = "Share Expired",
               use_last  = "Used", exp_last  = "Expired", share_last  = "Share Expired",
               share_net = change_col_label(FALSE)) %>%
    tab_spanner(label = as.character(first),
                columns = c(use_first, exp_first, share_first)) %>%
    tab_spanner(label = as.character(last),
                columns = c(use_last, exp_last, share_last)) %>%
    data_color(columns = c(share_first, share_last),
               fn = scales::col_numeric(
                 palette = c("white", tpa_colors[1]),
                 domain  = c(0, 100))) %>%
    data_color(columns = share_net,
               fn = scales::col_numeric(
                 palette = c(tpa_colors[2], "white", tpa_colors[1]),
                 domain  = c(-lim_net, lim_net))) %>%
    tab_style(style = cell_fill(color = "white"),
              locations = cells_body(columns = starts_with(c("use_", "exp_")))) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = share_net)) %>%
    cols_align(align = "center", columns = -Country) %>%
    tab_style(style = cell_text(align = "left"), locations = cells_stub(rows = TRUE)) %>%
    tab_footnote(
      footnote = end_sentence(
        "\u201cExpired\u201d columns show certifications that lapsed before use,",
        "in count and as a share of that year's certifications.",
        change_col_label(FALSE), "is the", change_phrase(change_type),
        "in that share"
      ),
      locations = cells_column_labels(columns = share_net)
    ) %>%
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom: 2px solid %s !important; }",
      "TAB604", tpa_colors[1])) %>%
    with_size("standard")
}

# ---- Build Employment Section Intentions Bookend Table ----
build_sector_bookend_table <- function(meta,
                                       sources = c(`U.S. citizens`          = "TAB603a",
                                                   `Temporary visa holders` = "TAB603b"),
                                       first = 1994, last = 2024,
                                       keys  = c("us", "tvh"),
                                       sector_order = c("Academe", "Industry or business",
                                                        "Government", "Nonprofit organization",
                                                        "Other or unknown"),
                                       change_type = c("ppt", "relative")) {
  
  change_type <- resolve_change_type(change_type)
  pull_group <- function(name, key) {
    raw <- read_fig(name) %>%
      mutate(Year = suppressWarnings(as.numeric(Year))) %>%
      distinct(Year, .keep_all = TRUE)
    lv <- function(yr) as.numeric(raw[raw$Year == yr, sector_order][1, ]) * 100
    tibble(
      sector = sector_order,
      !!paste0(key, "_first") := lv(first),
      !!paste0(key, "_last")  := lv(last)
    ) %>%
      mutate(!!paste0(key, "_net") := table_change(
        .data[[paste0(key, "_first")]],
        .data[[paste0(key, "_last")]],
        change_type
      ))
  }
  
  df <- tibble(sector = factor(sector_order, levels = sector_order))
  for (i in seq_along(sources))
    df <- df %>% left_join(pull_group(sources[[i]], keys[i]), by = "sector")
  
  net_cols <- paste0(keys, "_net")
  lim_net  <- max(abs(as.matrix(df[net_cols])), na.rm = TRUE)
  
  tbl <- df %>%
    gt(rowname_col = "sector", id = "tab_069") %>%
    fmt_number(columns = ends_with(c("_first", "_last")), decimals = 1, pattern = "{x}%") %>%
    fmt_number(columns = all_of(net_cols), decimals = 1, force_sign = TRUE,
               pattern = if (change_type == "relative") "{x}%" else "{x} pp")
  
  for (i in seq_along(sources)) {
    k <- keys[i]
    tbl <- tbl %>%
      cols_label(!!paste0(k, "_first") := as.character(first),
                 !!paste0(k, "_last")  := as.character(last),
                 !!paste0(k, "_net")   := change_col_label(FALSE)) %>%
      tab_spanner(label = names(sources)[i],
                  columns = all_of(paste0(k, c("_first", "_last", "_net"))))
  }
  
  tbl %>%
    data_color(columns = all_of(net_cols),
               fn = scales::col_numeric(
                 palette = c(tpa_colors[2], "white", tpa_colors[1]),
                 domain  = c(-lim_net, lim_net))) %>%
    tab_style(style = cell_fill(color = "white"),
              locations = cells_body(columns = ends_with(c("_first", "_last")))) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = all_of(net_cols))) %>%
    cols_align(align = "center", columns = -sector) %>%
    tab_style(style = cell_text(align = "left"), locations = cells_stub(rows = TRUE)) %>%
    tab_footnote(
      footnote = end_sentence(
        first, "and", last, "are shares of PhDs with definite commitments (%).",
        change_col_label(FALSE), "is the", change_phrase(change_type),
        "between the two years"
      ),
      locations = cells_column_labels(columns = all_of(net_cols))
    ) %>%
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom: 2px solid %s !important; }",
      "tab_069", tpa_colors[1])) %>%
    with_size("standard")
}

# ---- Bookend table (two time points + change) --------------------
# Used by tab_015, tab_016, tab_017. Reads a raw wide file where column 1
# is the time/cohort dimension and the rest are categories. Pulls two
# bookend rows, transposes so categories become table rows, computes the
# change between them, and colors the change column.
#   id_col       : name assigned to column 1 (default "Year")
#   first, last  : the two column-1 values to use as bookends
#   first_label / last_label : column headers (default = the values)
#   change_type  : "relative" (percent) or "ppt" (percentage points)
#   drop_cols    : category columns to exclude (e.g. all-zero "Antarctica")
#   recode_cats  : named vector to rename categories for display
#   cat_order    : explicit display order (uses recoded names); overrides sort_by
#   sort_by      : "last" (2024 share desc) or "change" (change desc)
#   flagged_rows : stub rows that get the flag_note footnote
build_bookend_table <- function(name, meta,
                                first, last,
                                id_col      = "Year",
                                first_label = as.character(first),
                                last_label  = as.character(last),
                                spanner_label = "Share of students",
                                change_type = c("relative", "ppt"),
                                drop_cols = NULL, recode_cats = NULL,
                                cat_order = NULL,
                                sort_by   = c("last", "change"),
                                flagged_rows = NULL, flag_note = NULL,
                                footnote = NULL,
                                scale = 1) {
  
  change_type <- resolve_change_type(change_type)
  sort_by     <- match.arg(sort_by)
  
  raw <- read_fig(name) %>%
    rename(!!id_col := 1) %>%
    mutate(across(-all_of(id_col), ~ suppressWarnings(as.numeric(.))))
  
  cats <- setdiff(names(raw), c(id_col, drop_cols))
  
  fr <- raw %>% filter(.data[[id_col]] == first) %>% slice(1) %>% select(all_of(cats))
  lr <- raw %>% filter(.data[[id_col]] == last)  %>% slice(1) %>% select(all_of(cats))
  
  df <- tibble(
    category  = cats,
    pct_first = as.numeric(unlist(fr)) * scale,
    pct_last  = as.numeric(unlist(lr)) * scale
  ) %>%
    mutate(
      category = if (!is.null(recode_cats)) recode(category, !!!recode_cats) else category,
      Change   = table_change(pct_first, pct_last, change_type)
    )
  
  df <- if (!is.null(cat_order)) {
    df %>% arrange(match(category, cat_order))
  } else if (sort_by == "change") {
    df %>% arrange(desc(Change))
  } else {
    df %>% arrange(desc(pct_last))
  }
  
  lim         <- max(abs(df$Change), na.rm = TRUE)
  chg_pattern <- if (change_type == "relative") "{x}%" else "{x} pp"
  
  tbl <- df %>%
    gt(rowname_col = "category", id = name) %>%
    fmt_number(columns = c(pct_first, pct_last), decimals = 1, pattern = "{x}%") %>%
    fmt_number(columns = Change, decimals = 1, force_sign = TRUE, pattern = chg_pattern) %>%
    cols_label(pct_first = first_label, pct_last = last_label,
               Change = change_col_label(FALSE)) %>%
    tab_spanner(label = spanner_label, columns = c(pct_first, pct_last)) %>%
    tab_spanner(label = "\u00A0", columns = Change) %>%   # blank spanner: aligns header heights
    data_color(
      columns = Change,
      fn = scales::col_numeric(
        palette = c(tpa_colors[2], "white", tpa_colors[1]),
        domain  = c(-lim, lim)
      )
    ) %>%
    tab_style(style = cell_fill(color = "white"),
              locations = cells_body(columns = c(pct_first, pct_last))) %>%
    cols_align(align = "right", columns = c(pct_first, pct_last, Change)) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = Change)) %>%
    tab_style(style = cell_text(align = "left"),
              locations = cells_stub(rows = TRUE))
  
  if (!is.null(flagged_rows) && !is.null(flag_note)) {
    tbl <- tbl %>%
      tab_footnote(footnote = flag_note,
                   locations = cells_stub(rows = df$category %in% flagged_rows))
  }
  if (!is.null(footnote)) {
    tbl <- tbl %>%
      tab_footnote(footnote = footnote,
                   locations = cells_column_spanners(spanners = spanner_label))
  }
  
  tbl %>%
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      name, tpa_colors[1])) %>%
    with_size("standard")
}

# ---- Prepare Table 1.01 source data -----------------------------------------
#
# One source of truth for Table 1.01 data preparation.
#
# - standardizes degree labels
# - standardizes field labels
# - combines the two engineering categories using underlying counts
# - recomputes NonresidentShare
#
# No endpoint selection or display formatting happens here.

prepare_tab101_data <- function(name = "TAB101") {
  
  read_fig(name) %>%
    
    mutate(
      Year = suppressWarnings(
        as.integer(.data$Year)
      ),
      
      Degree = recode(
        .data$Degree,
        "Bachelor's" = "Bachelors",
        "Master's"   = "Masters",
        "Doctorate"  = "Doctorate"
      ),
      
      Field = recode(
        .data$Field,
        
        "Computer and Information Sciences and Support Services" =
          "Computer and Information Sciences"
      ),
      
      Nonresident_Total = suppressWarnings(
        as.numeric(.data$Nonresident_Total)
      ),
      
      Grand_Total = suppressWarnings(
        as.numeric(.data$Grand_Total)
      )
    ) %>%
    
    filter(
      is.finite(.data$Year),
      .data$Degree %in% c(
        "Bachelors",
        "Masters",
        "Doctorate"
      )
    ) %>%
    
    group_by(
      .data$Field,
      .data$Degree,
      .data$Year
    ) %>%
    
    summarise(
      Nonresident_Total = if (
        all(is.na(.data$Nonresident_Total))
      ) {
        NA_real_
      } else {
        sum(
          .data$Nonresident_Total,
          na.rm = TRUE
        )
      },
      
      Grand_Total = if (
        all(is.na(.data$Grand_Total))
      ) {
        NA_real_
      } else {
        sum(
          .data$Grand_Total,
          na.rm = TRUE
        )
      },
      
      .groups = "drop"
    ) %>%
    
    mutate(
      NonresidentShare = if_else(
        is.finite(.data$Nonresident_Total) &
          is.finite(.data$Grand_Total) &
          .data$Grand_Total > 0,
        
        100 *
          .data$Nonresident_Total /
          .data$Grand_Total,
        
        NA_real_
      )
    )
}

# ---- Combined share-change table (all degrees) ------------------------------
#
# Reads the long-format source:
#   Year
#   Field
#   Degree            (Bachelor's / Master's / Doctorate)
#   Nonresident_Total
#   Grand_Total
#   NonresidentShare  (percent, e.g. 7.24)
#
# The two engineering source rows are collapsed into a single "Engineering"
# row by summing the underlying counts, then recomputing the share. Endpoints
# are taken from the data (min and max Year), not hardcoded. Column order is
# forced to match `degrees`. The *_PctChangeInShare columns are recalculated
# from the two displayed endpoint columns so the table cannot drift. Fields are
# sorted by average movement across the three degrees.

build_change_table_wide <- function(
    name,
    meta,
    change_type = c("relative", "ppt")
) {
  
  change_type <- resolve_change_type(change_type)
  
  degrees <- c(
    "Bachelors",
    "Masters",
    "Doctorate"
  )
  
  keep_fields <- c(
    "Mathematics and Statistics",
    "Engineering",
    "Computer and Information Sciences",
    "Architecture and Related Services",
    "Physical Sciences",
    "Biological and Biomedical Sciences",
    "Natural Resources and Conservation"
  )
  
  # --------------------------------------------------------------------------
  # Read long-format source; merge the two engineering rows by summing counts
  # --------------------------------------------------------------------------
  
  df_long <- prepare_tab101_data(name) %>%
    filter(
      .data$Field %in% keep_fields
    )
  
  # Endpoints come from the data, not from constants.
  earlier_year <- min(df_long$Year, na.rm = TRUE)
  latest_year  <- max(df_long$Year, na.rm = TRUE)
  
  earlier_cols <- paste0(degrees, "_", earlier_year)
  latest_cols  <- paste0(degrees, "_", latest_year)
  change_cols  <- paste0(degrees, "_PctChangeInShare")
  year_cols    <- c(earlier_cols, latest_cols)
  
  required_cols <- c(
    "Field",
    earlier_cols,
    latest_cols,
    change_cols
  )
  
  # --------------------------------------------------------------------------
  # Pivot the two endpoint years to wide
  # --------------------------------------------------------------------------
  
  df <- df_long %>%
    filter(Year %in% c(earlier_year, latest_year)) %>%
    select(Field, Degree, Year, NonresidentShare) %>%
    pivot_wider(
      names_from  = c(Degree, Year),
      names_sep   = "_",
      values_from = NonresidentShare
    )
  
  # Change columns are recalculated below; seed them so the check passes.
  for (deg in degrees) {
    df[[paste0(deg, "_PctChangeInShare")]] <- NA_real_
  }
  
  # Force physical column order to match `degrees`.
  df <- df %>%
    select(Field, all_of(c(rbind(earlier_cols, latest_cols, change_cols))))
  
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in ",
      name,
      ": ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  df <- df %>%
    mutate(
      across(
        -Field,
        ~ suppressWarnings(
          as.numeric(.x)
        )
      )
    )
  
  # --------------------------------------------------------------------------
  # Recalculate change from the displayed endpoint values
  # --------------------------------------------------------------------------
  
  for (deg in degrees) {
    
    col_earlier <- paste0(deg, "_", earlier_year)
    col_latest  <- paste0(deg, "_", latest_year)
    change_col  <- paste0(deg, "_PctChangeInShare")
    
    df[[change_col]] <- table_change(
      df[[col_earlier]],
      df[[col_latest]],
      change_type
    )
  }
  
  # --------------------------------------------------------------------------
  # Rank fields by average movement across the three degree levels
  # --------------------------------------------------------------------------
  
  # Sort by each field's average latest-year share across the three degrees.
  df <- df %>%
    mutate(
      .avg = rowMeans(
        across(
          all_of(latest_cols)
        ),
        na.rm = TRUE
      )
    ) %>%
    arrange(
      desc(.avg)
    ) %>%
    select(
      -.avg
    )
  
  # Symmetric domain keeps zero at the visual midpoint.
  lim <- max(
    abs(
      as.matrix(
        df[change_cols]
      )
    ),
    na.rm = TRUE
  )
  
  if (
    !is.finite(lim) ||
    lim == 0
  ) {
    lim <- 1
  }
  
  # --------------------------------------------------------------------------
  # Build table
  # --------------------------------------------------------------------------
  
  tbl <- df %>%
    gt(
      rowname_col = "Field",
      id = name
    ) %>%
    
    fmt_number(
      columns = all_of(earlier_cols),
      decimals = 1,
      pattern = "{x}%"
    ) %>%
    
    fmt_number(
      columns = all_of(latest_cols),
      decimals = 1,
      pattern = "{x}%"
    ) %>%
    
    fmt_number(
      columns = all_of(change_cols),
      decimals = 1,
      force_sign = TRUE,
      pattern = if (
        change_type == "relative"
      ) {
        "{x}%"
      } else {
        "{x} pp"
      }
    )
  
  # Degree spanners.
  for (deg in degrees) {
    
    tbl <- tbl %>%
      tab_spanner(
        label = deg,
        columns = all_of(
          c(
            paste0(deg, "_", earlier_year),
            paste0(deg, "_", latest_year),
            paste0(deg, "_PctChangeInShare")
          )
        )
      )
  }
  
  # --------------------------------------------------------------------------
  # Column labels
  # --------------------------------------------------------------------------
  
  tbl <- tbl %>%
    cols_label(
      .list = setNames(
        c(
          rep(as.character(earlier_year), length(earlier_cols)),
          rep(as.character(latest_year),  length(latest_cols)),
          rep("Change",                   length(change_cols))
        ),
        c(earlier_cols, latest_cols, change_cols)
      )
    ) %>%
    
    # Diverging gradient on change columns.
    data_color(
      columns = all_of(change_cols),
      fn = scales::col_numeric(
        palette = c(
          tpa_colors[2],
          "white",
          tpa_colors[1]
        ),
        domain = c(
          -lim,
          lim
        )
      )
    ) %>%
    
    # Endpoint-year columns remain white.
    tab_style(
      style = cell_fill(
        color = "white"
      ),
      locations = cells_body(
        columns = all_of(year_cols)
      )
    ) %>%
    
    cols_align(
      align = "right",
      columns = -Field
    )
  
  # --------------------------------------------------------------------------
  # Bold endpoint shares above 50%
  # --------------------------------------------------------------------------
  
  for (col in year_cols) {
    
    tbl <- tbl %>%
      tab_style(
        style = cell_text(
          weight = "bold"
        ),
        locations = cells_body(
          columns = all_of(col),
          rows = round(.data[[col]], 1) >= 50
        )
      )
  }
  
  # --------------------------------------------------------------------------
  # Footnote + shared publication styling
  # --------------------------------------------------------------------------
  
  tbl %>%
    tab_footnote(
      footnote = end_sentence(
        "Values are the percent of degree completions awarded to international",
        "(nonresident) students.", change_col_label(FALSE), "is the",
        change_phrase(change_type), "in that share from",
        earlier_year, "to", latest_year,
        ", with fields ordered by their average", latest_year,
        "share across the three degree levels"
      ),
      locations = cells_column_spanners(
        spanners = degrees
      )
    ) %>%
    
    theme_gt_tpa(
      meta = meta
    ) %>%
    
    opt_css(
      css = sprintf(
        "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
        name,
        tpa_colors[1]
      )
    ) %>%
    
    with_table_profile(
      "ultrawide"
    ) %>%
    
    with_size(
      "long"
    )
}

# ---- Field share-change table -------------------------------
# Used by tab 006.
# Expects a CSV with Field, FirstYear, LastYear, PctChangeInShare.
# FirstYear/LastYear are nonresident shares (%); PctChangeInShare is
# the relative change between them.

build_change_table <- function(name, meta,
                               change_type = c("relative", "ppt")) {
  change_type <- resolve_change_type(change_type)
  df <- read_fig(name) %>%
    mutate(
      FirstYear        = as.numeric(FirstYear),
      LastYear         = as.numeric(LastYear),
      PctChangeInShare = as.numeric(PctChangeInShare)
    ) %>%
    mutate(PctChangeInShare = table_change(FirstYear, LastYear, change_type)) %>%
    arrange(desc(PctChangeInShare))
  
  df %>%
    gt(rowname_col = "Field") %>%
    fmt_number(columns = c(FirstYear, LastYear), decimals = 1) %>%
    fmt_number(columns = PctChangeInShare, decimals = 1,
               force_sign = TRUE,
               pattern = if (change_type == "relative") "{x}%" else "{x} pp") %>%
    cols_label(
      FirstYear        = "1995",
      LastYear         = "2024",
      PctChangeInShare = "Change"
    ) %>%
    gt_color_rows(columns  = PctChangeInShare,
                  palette  = c(TPA_RED_LIGHT, tpa_colors[1]),
                  pal_type = "continuous") %>%
    cols_align(align = "right",
               columns = c(FirstYear, LastYear, PctChangeInShare)) %>%
    tab_style(style     = cell_text(weight = "bold"),
              locations = cells_body(columns = PctChangeInShare)) %>%
    theme_gt_tpa(meta = meta) %>%
    with_size("standard")
}

# ---- University ranking tables ------------------------------
# Used by tabs 001, 002.
# integer_cols = TRUE uses fmt_integer on the four field columns (tab 001);
# FALSE uses fmt_number(decimals = 2) instead (tab 002).

build_ranking_table <- function(name, meta, integer_cols = TRUE) {
  df <- read_fig(name) %>%
    mutate(Year  = as.character(Year),
           across(-Year, ~ suppressWarnings(as.numeric(.))))
  
  field_cols <- c("Physical Sciences", "Life Sciences",
                  "Engineering", "Computer Science")
  
  tbl <- df %>% gt(rowname_col = "Year")
  
  if (integer_cols) {
    tbl <- fmt_integer(tbl, columns = all_of(field_cols))
  } else {
    tbl <- fmt_number(tbl, columns = all_of(field_cols), decimals = 2)
  }
  
  tbl %>%
    fmt_number(columns = Average, decimals = 2) %>%
    sub_missing(missing_text = "—") %>%
    gt_color_rows(columns  = all_of(field_cols),
                  palette  = c(TPA_RED_LIGHT, tpa_colors[1]),
                  pal_type = "continuous") %>%
    cols_align(align   = "right",
               columns = all_of(c(field_cols, "Average"))) %>%
    tab_style(style     = cell_text(weight = "bold"),
              locations = cells_body(columns = Average)) %>%
    theme_gt_tpa(meta = meta) %>%
    with_size("standard")
}

# ---- University ranking bookend + interim-delta table -------
# Sibling to build_ranking_table (which stays for tab_002). Transposes the
# field-by-year ranking counts into a bookend-with-changes layout: one row
# per field, two bookend-year counts as levels with interior year-over-year
# changes between them, plus a net change. Counts are integers; change
# columns get a per-column symmetric diverging scale. df arrives pivoted
# wide with columns in display order: Field, <level>, <chg>, <chg>, <chg>,
# <level>, Change. change_type controls relative versus native-unit change. NA cells
# render as an em dash. cell_footnote/cell_footnote_field/cell_footnote_col
# attach an optional single-cell footnote (default: the first level column
# of the named field) with standard symbol marks so it reads as a dagger.
build_ranking_delta_table <- function(df, name, meta, level_cols, chg_cols,
                                      spanner_label       = "Universities in the global top 100",
                                      change_type         = c("ppt", "relative"),
                                      footnote            = NULL,
                                      cell_footnote       = NULL,
                                      cell_footnote_field = NULL,
                                      cell_footnote_col   = NULL) {
  change_type  <- resolve_change_type(change_type)
  all_chg_cols <- c(chg_cols, "Change")
  span_order   <- c(level_cols[1], chg_cols, level_cols[2])
  numeric_cols <- c(level_cols, all_chg_cols)
  
  if (is.null(footnote)) {
    footnote <- end_sentence(
      "The first and last columns are counts of universities in each field;",
      "interior columns show the change from the prior shown year, and",
      change_col_label(TRUE), "is the change across the full window. Changes are",
      if (change_type == "relative") "relative (percent)" else "absolute counts"
    )
  }
  
  level_labels <- setNames(gsub("^Y", "", level_cols), level_cols)
  chg_labels   <- setNames(delta_col_label(gsub("^Chg", "", chg_cols)), chg_cols)
  
  # Column widths are deliberately not set here; save_table() owns geometry.
  tbl <- df %>%
    gt(rowname_col = "Field", id = name) %>%
    fmt_integer(columns = all_of(level_cols))
  
  if (change_type == "relative") {
    tbl <- tbl %>% fmt_number(columns = all_of(all_chg_cols), decimals = 1,
                              force_sign = TRUE, pattern = "{x}%")
  } else {
    tbl <- tbl %>% fmt_integer(columns = all_of(all_chg_cols), force_sign = TRUE)
  }
  
  tbl <- tbl %>%
    sub_missing(missing_text = "\u2014") %>%
    tab_spanner(label = spanner_label, columns = all_of(span_order)) %>%
    cols_label(!!!level_labels, !!!chg_labels,
               Change = change_col_label(length(chg_cols) > 0))
  
  for (col in all_chg_cols) {
    lim <- max(abs(df[[col]]), na.rm = TRUE)
    tbl <- tbl %>%
      data_color(columns = all_of(col),
                 fn = scales::col_numeric(
                   palette = c(tpa_colors[2], "white", tpa_colors[1]),
                   domain  = c(-lim, lim), na.color = "white"))
  }
  
  tbl <- tbl %>%
    tab_style(style = cell_fill(color = "white"),
              locations = cells_body(columns = all_of(level_cols))) %>%
    tab_style(style = cell_text(align = "left"),
              locations = cells_stub(rows = TRUE)) %>%
    cols_align(align = "right", columns = all_of(numeric_cols))
  
  tbl <- tbl %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = Change)) %>%
    tab_footnote(footnote = footnote,
                 locations = cells_column_spanners(spanners = spanner_label))
  
  # optional single-cell footnote (e.g. a level cell carrying a different year)
  has_cell_note <- !is.null(cell_footnote) && !is.null(cell_footnote_field)
  if (has_cell_note) {
    r  <- which(as.character(df$Field) == cell_footnote_field)
    cc <- if (is.null(cell_footnote_col)) level_cols[1] else cell_footnote_col
    tbl <- tbl %>%
      tab_footnote(footnote  = cell_footnote,
                   locations = cells_body(columns = all_of(cc), rows = r))
  }
  
  tbl <- tbl %>%
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      name, tpa_colors[1]))
  
  # No opt_footnote_marks() call here: theme_gt_tpa() sets numeric marks for
  # every table in the set, so a cell-level note is numbered like any other.
  tbl %>% with_size("standard")
}

# ---- EB visa wait time tables -------------------------------
# Used by tabs 003, 004, 005.
# Expects a CSV with Year, EB1, EB1.1, EB2, EB2.1, EB3, EB3.1 columns.
# Merges each numeric/label pair, colors rows on a shared domain
# anchored to the max of EB3 (the longest waits).

build_eb_table <- function(name, meta) {
  df <- read_fig(name) %>%
    mutate(
      Year = as.character(Year),
      EB1  = as.numeric(EB1),
      EB2  = as.numeric(EB2),
      EB3  = as.numeric(EB3)
    )
  
  df %>%
    gt(rowname_col = "Year") %>%
    cols_merge(columns = c(EB1, `EB1.1`), pattern = "{2}") %>%
    cols_merge(columns = c(EB2, `EB2.1`), pattern = "{2}") %>%
    cols_merge(columns = c(EB3, `EB3.1`), pattern = "{2}") %>%
    cols_label(EB1 = "EB1", EB2 = "EB2", EB3 = "EB3") %>%
    gt_color_rows(columns  = c(EB1, EB2, EB3),
                  palette  = c(TPA_RED_LIGHT, tpa_colors[1]),
                  pal_type = "continuous",
                  domain   = c(0, max(df$EB3, na.rm = TRUE))) %>%
    cols_align(align = "right", columns = c(EB1, EB2, EB3)) %>%
    theme_gt_tpa(meta = meta) %>%
    with_size("standard")
}

# ---- Field-share change table -------------------------------
# Used by tab 008.
# Takes an already-assembled wide df with Country plus, for each field
# label, {label}_First / {label}_Last / {label}_Change columns
# (First = 2009 share, Last = 2024 share, Change = relative change).
# field_labels is the character vector of spanner labels (also the
# column-name prefixes). Each Change column gets its own symmetric
# color domain because per-field change magnitudes differ a lot.

# ---- Field-share change table -------------------------------
# Used by tab 008.
# Takes an already-assembled wide df with Country plus, for each field
# label, {label}_First / {label}_Last / {label}_Change columns
# (First = 2009 share, Last = 2024 share, Change = relative change OR
# percentage-point change, depending on change_type). field_labels is the
# character vector of spanner labels (also the column-name prefixes). Each
# Change column gets its own symmetric color domain because per-field
# change magnitudes differ a lot.
#   change_type : "relative" (percent, default) or "ppt" (percentage points).
#                 Controls change-column *formatting* and the footnote only;
#                 compute the matching values upstream in the chunk.

build_field_share_table <- function(df, name, meta, field_labels,
                                    change_type = c("relative", "ppt")) {
  change_type <- resolve_change_type(change_type)
  change_cols <- paste0(field_labels, "_Change")
  value_cols  <- c(paste0(field_labels, "_First"),
                   paste0(field_labels, "_Last"))
  
  for (lab in field_labels) {
    df[[paste0(lab, "_Change")]] <- table_change(
      df[[paste0(lab, "_First")]],
      df[[paste0(lab, "_Last")]],
      change_type
    )
  }
  
  # merge the flag image into the Country stub (no separate column, no header)
  if ("flag" %in% names(df)) {
    df <- df %>%
      mutate(
        Country = paste0(
          vapply(flag, function(f) gt::local_image(f, height = 14), character(1)),
          "&nbsp;&nbsp;", Country
        )
      ) %>%
      select(-flag)
  }
  
  tbl <- df %>%
    gt(rowname_col = "Country", id = name) %>%
    fmt_markdown(columns = "Country") %>%     # renders the embedded <img> in the stub
    fmt_number(columns = ends_with("_First"),  decimals = 1, pattern = "{x}%") %>%
    fmt_number(columns = ends_with("_Last"),   decimals = 1, pattern = "{x}%") %>%
    fmt_number(columns = ends_with("_Change"),
               decimals = 1, force_sign = TRUE,
               pattern = if (change_type == "ppt") "{x} pp" else "{x}%") %>%
    sub_missing(missing_text = "—")
  
  for (lab in field_labels) {
    tbl <- tbl %>%
      tab_spanner(label = lab, columns = starts_with(paste0(lab, "_")))
  }
  
  tbl <- tbl %>%
    cols_label(
      ends_with("_First")  ~ "2009",
      ends_with("_Last")   ~ "2024",
      ends_with("_Change") ~ "Change"
    )
  
  # per-field diverging gradient: blue (decline) -> white (0) -> red (rise)
  for (col in change_cols) {
    lim <- max(abs(df[[col]]), na.rm = TRUE)
    tbl <- tbl %>%
      data_color(
        columns = all_of(col),
        fn = scales::col_numeric(
          palette = c(tpa_colors[2], "white", tpa_colors[1]),
          domain  = c(-lim, lim)
        )
      )
  }
  
  tbl %>%
    tab_style(
      style     = cell_fill(color = "white"),
      locations = cells_body(columns = all_of(value_cols))
    ) %>%
    cols_align(align = "right", columns = -Country) %>%
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body(columns = all_of(change_cols))
    ) %>%
    tab_footnote(
      footnote = end_sentence(
        "Values are the share of each origin country's international students",
        "enrolled in the given field in 2010 and 2025, and the",
        change_phrase(change_type), "between the two years. Countries are sorted",
        "by total international student enrollment, with the largest first"
      ),
      locations = cells_column_spanners(spanners = field_labels)
    ) %>%
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      name, tpa_colors[1]
    )) %>%
    with_table_profile("ultrawide") %>%
    with_stub_width(table_scaled_px(170)) %>%
    with_size("long")
}

build_citizenship_table <- function(df, name, meta,
                                    first_year, last_year,
                                    footnote = NULL,
                                    value_is_share = FALSE,
                                    share_group = FALSE,
                                    change_type = c("relative", "ppt")) {
  
  change_type <- resolve_change_type(change_type)
  df <- df %>%
    mutate(
      USC_Change = table_change(USC_First, USC_Last, change_type),
      TVH_Change = table_change(TVH_First, TVH_Last, change_type)
    )
  if (share_group) {
    df <- df %>% mutate(SHR_Change = table_change(SHR_First, SHR_Last, change_type))
  }
  
  change_cols <- c("USC_Change", "TVH_Change")
  value_cols  <- c("USC_First", "USC_Last", "TVH_First", "TVH_Last")
  if (share_group) {
    change_cols <- c(change_cols, "SHR_Change")
    share_vals  <- c("SHR_First", "SHR_Last")
  }
  
  tbl <- df %>%
    gt(rowname_col = "Field", id = name)
  
  if (value_is_share) {
    tbl <- tbl %>% fmt_number(columns = all_of(value_cols), decimals = 1, pattern = "{x}%")
  } else {
    tbl <- tbl %>% fmt_integer(columns = all_of(value_cols))
  }
  if (share_group) {
    tbl <- tbl %>% fmt_number(columns = all_of(share_vals), decimals = 1, pattern = "{x}%")
  }
  
  base_change_cols <- c("USC_Change", "TVH_Change")
  if (change_type == "relative" || value_is_share) {
    tbl <- tbl %>%
      fmt_number(columns = all_of(base_change_cols),
                 decimals = 1, force_sign = TRUE,
                 pattern = if (change_type == "relative") "{x}%" else "{x} pp")
  } else {
    tbl <- tbl %>% fmt_integer(columns = all_of(base_change_cols), force_sign = TRUE)
  }
  if (share_group) {
    tbl <- tbl %>%
      fmt_number(columns = SHR_Change, decimals = 1, force_sign = TRUE,
                 pattern = if (change_type == "relative") "{x}%" else "{x} pp")
  }
  
  tbl <- tbl %>%
    tab_spanner(label = "U.S. citizens & permanent residents",
                columns = c(USC_First, USC_Last, USC_Change)) %>%
    tab_spanner(label = "Temporary visa holders",
                columns = c(TVH_First, TVH_Last, TVH_Change))
  
  if (share_group) {
    tbl <- tbl %>%
      tab_spanner(label = "International share",
                  columns = c(SHR_First, SHR_Last, SHR_Change))
  }
  
  tbl <- tbl %>%
    cols_label(
      USC_First = as.character(first_year), USC_Last = as.character(last_year),
      USC_Change = "Change",
      TVH_First = as.character(first_year), TVH_Last = as.character(last_year),
      TVH_Change = "Change"
    )
  if (share_group) {
    tbl <- tbl %>%
      cols_label(
        SHR_First = as.character(first_year), SHR_Last = as.character(last_year),
        SHR_Change = "Change"
      )
  }
  
  # per-group symmetric diverging gradient on each change column
  for (col in change_cols) {
    lim <- max(abs(df[[col]]), na.rm = TRUE)
    tbl <- tbl %>%
      data_color(
        columns = all_of(col),
        fn = scales::col_numeric(
          palette = c(tpa_colors[2], "white", tpa_colors[1]),
          domain  = c(-lim, lim)
        )
      )
  }
  
  white_cols <- value_cols
  if (share_group) white_cols <- c(white_cols, share_vals)
  
  tbl <- tbl %>%
    tab_style(style = cell_fill(color = "white"),
              locations = cells_body(columns = all_of(white_cols))) %>%
    cols_align(align = "right", columns = -Field) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = all_of(change_cols)))
  
  if (!is.null(footnote)) {
    spanners <- c("U.S. citizens & permanent residents", "Temporary visa holders")
    if (share_group) spanners <- c(spanners, "International share")
    tbl <- tbl %>%
      tab_footnote(footnote = footnote,
                   locations = cells_column_spanners(spanners = spanners))
  }
  
  tbl %>%
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      name, tpa_colors[1])) %>%
    with_table_profile("wide") %>%
    with_stub_width(table_scaled_px(210)) %>%
    with_size("long")
}

# ---- Conference author-share summary table -------------------
# Companion to fig_048. One row per conference: U.S. and Chinese shares at
# the start and end of the compared window, the percentage-point change,
# and the inflection year (first year China's share exceeded the U.S.
# share). "Start"/"latest" anchor on the first and last year BOTH countries
# are observed, so comparisons are like-for-like; that window is shown per
# row under Coverage. Columns are grouped by country.
build_conference_summary_table <- function(name, meta, conf_names,
                                           data_name = name,
                                           us_label = "United States",
                                           cn_label = "China",
                                           change_type = c("ppt", "relative")) {
  
  change_type <- resolve_change_type(change_type)
  raw <- read_fig(data_name) %>%
    transmute(
      conf    = as.character(conf_norm),
      year    = suppressWarnings(as.integer(year)),
      country = as.character(country),
      share   = suppressWarnings(as.numeric(share)) * 100
    ) %>%
    filter(country %in% c(us_label, cn_label), !is.na(year), !is.na(share)) %>%
    mutate(country = if_else(country == us_label, "US", "CN"))
  
  wide <- raw %>%
    tidyr::pivot_wider(names_from = country, values_from = share,
                       values_fn = ~ mean(.x, na.rm = TRUE))
  
  summ <- wide %>%
    group_by(conf) %>%
    group_modify(~ {
      d   <- .x %>% arrange(year)
      bp  <- !is.na(d$US) & !is.na(d$CN)
      ayf <- if (any(bp)) min(d$year[bp]) else min(d$year)
      ayl <- if (any(bp)) max(d$year[bp]) else max(d$year)
      hit <- bp & (d$CN > d$US)
      tibble(
        first_yr = ayf,
        last_yr  = ayl,
        US_First = d$US[d$year == ayf][1],
        CN_First = d$CN[d$year == ayf][1],
        US_Last  = d$US[d$year == ayl][1],
        CN_Last  = d$CN[d$year == ayl][1],
        cross_yr = if (any(hit)) min(d$year[hit]) else NA_integer_
      )
    }) %>%
    ungroup() %>%
    mutate(
      US_Change  = table_change(US_First, US_Last, change_type),
      CN_Change  = table_change(CN_First, CN_Last, change_type),
      # A venue observed in only one year would otherwise read "2022-2022".
      Coverage   = if_else(first_yr == last_yr,
                           as.character(first_yr),
                           paste0(first_yr, "\u2013", last_yr)),
      Conference = recode(conf, !!!conf_names),
      cross_yr   = as.character(cross_yr)
    ) %>%
    arrange(desc(CN_Change)) %>%
    select(Conference, Coverage,
           US_First, US_Last, US_Change,
           CN_First, CN_Last, CN_Change, cross_yr)
  
  lim <- max(abs(c(summ$US_Change, summ$CN_Change)), na.rm = TRUE)
  
  summ %>%
    gt(rowname_col = "Conference", id = name) %>%
    fmt_number(columns = c(US_First, US_Last, CN_First, CN_Last),
               decimals = 1, pattern = "{x}%") %>%
    fmt_number(columns = c(US_Change, CN_Change),
               decimals = 1, force_sign = TRUE,
               pattern = if (change_type == "relative") "{x}%" else "{x} pp") %>%
    sub_missing(columns = c(US_First, US_Last, CN_First, CN_Last,
                            US_Change, CN_Change), missing_text = "\u2014") %>%
    sub_missing(columns = cross_yr, missing_text = "Not yet") %>%
    cols_label(
      Coverage  = "Coverage",
      US_First  = "First", US_Last = "Latest", US_Change = "Change",
      CN_First  = "First", CN_Last = "Latest", CN_Change = "Change",
      cross_yr  = "China overtook"
    ) %>%
    tab_spanner(label = "\u00A0", columns = Coverage, id = "sp_cov") %>%
    tab_spanner(label = us_label, columns = c(US_First, US_Last, US_Change)) %>%
    tab_spanner(label = cn_label, columns = c(CN_First, CN_Last, CN_Change)) %>%
    tab_spanner(label = "\u00A0", columns = cross_yr, id = "sp_cross") %>%
    data_color(
      columns = c(US_Change, CN_Change),
      fn = scales::col_numeric(
        palette  = c(tpa_colors[2], "white", tpa_colors[1]),
        domain   = c(-lim, lim),
        na.color = "white"
      )
    ) %>%
    tab_style(style = cell_fill(color = "white"),
              locations = cells_body(columns = c(US_First, US_Last,
                                                 CN_First, CN_Last))) %>%
    cols_align(align = "right",
               columns = c(US_First, US_Last, US_Change,
                           CN_First, CN_Last, CN_Change, cross_yr)) %>%
    cols_align(align = "left", columns = Coverage) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = c(US_Change, CN_Change))) %>%
    tab_style(style = cell_text(align = "left"),
              locations = cells_stub(rows = TRUE)) %>%
    tab_footnote(
      footnote = end_sentence(
        "Each row gives the U.S. and Chinese share of accepted author",
        "affiliations at the first and last year both countries are observed;",
        "that window is shown under Coverage and differs by venue.",
        change_col_label(FALSE), "is the", paste0(change_phrase(change_type), "."),
        "\u201CChina overtook\u201D is the first such year China's share",
        "exceeded the U.S. share; \u201CNot yet\u201D means the U.S. share still",
        "led in the latest observed year"
      ),
      locations = cells_column_labels(columns = cross_yr)
    ) %>%
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      name, tpa_colors[1])) %>%
    with_size("long")
}

# ---- R&D-by-type bookend + interim-delta table --------------
# Level-only variant of the R&D-type table: shows dollar levels at each
# shown year plus a single net-change column, with a total-spending row per
# country. No interior delta columns; only the Net Change column is shaded.
# Data are current-PPP dollars (not inflation-adjusted), so the footnote says so.
build_rd_type_level_table <- function(df, name, meta, level_cols,
                                      spanner_label = "GERD (USD billions, current PPP)",
                                      footnote = NULL) {
  
  if (is.null(footnote)) {
    footnote <- end_sentence(
      "Spending levels in billions of US dollars, PPP converted, at current",
      "prices (not adjusted for inflation).", change_col_label(FALSE), "is the",
      "relative change from the first to the last shown year. Basic research is",
      "investment in future capability; applied research and experimental",
      "development are nearer-term. Totals may not sum exactly because of",
      "rounding"
    )
  }
  
  level_labels <- setNames(gsub("^Y", "", level_cols), level_cols)
  chg_lim      <- max(abs(df$Change), na.rm = TRUE)
  
  df %>%
    gt(rowname_col = "Type", groupname_col = "Country", id = name) %>%
    fmt_number(columns = all_of(level_cols), decimals = 1, pattern = "${x}B") %>%
    fmt_number(columns = "Change", decimals = 1, force_sign = TRUE,
               pattern = "{x}%") %>%
    tab_spanner(label = spanner_label, id = "gerd_span",
                columns = all_of(level_cols)) %>%
    cols_label(!!!level_labels, Change = change_col_label(FALSE)) %>%
    data_color(
      columns = Change,
      fn = scales::col_numeric(
        palette = TABLE_CHANGE_PALETTE,
        domain  = c(-chg_lim, chg_lim)
      )
    ) %>%
    tab_style(style = cell_text(align = "left"),
              locations = cells_stub(rows = TRUE)) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_row_groups()) %>%
    cols_align(align = "right", columns = c(all_of(level_cols), "Change")) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = Change)) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(rows = Type == "Total")) %>%
    tab_footnote(footnote = footnote,
                 locations = cells_column_spanners(spanners = "gerd_span")) %>%
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      name, tpa_colors[1])) %>%
    with_size("standard")
}

build_patents_table <- function(name, meta, sources,
                                first_year = 2014, last_year = 2024,
                                countries = c("China", "United States of America",
                                              "Japan", "Republic of Korea",
                                              "Germany", "United Kingdom", "Australia"),
                                recode_countries = c(
                                  "United States of America" = "United States",
                                  "Republic of Korea"        = "South Korea"),
                                change_type = c("relative", "ppt")) {
  
  change_type  <- resolve_change_type(change_type)
  field_labels <- names(sources)
  
  per_field <- lapply(seq_along(sources), function(i) {
    fld <- field_labels[i]
    df  <- read_fig(sources[[i]]) %>%
      rename(Year = 1) %>%
      mutate(Year = suppressWarnings(as.integer(round(as.numeric(as.character(Year))))))
    
    ctry_cols <- intersect(countries, names(df))
    fr <- df %>% filter(Year == first_year) %>% slice(1)
    lr <- df %>% filter(Year == last_year)  %>% slice(1)
    
    tibble(
      Country = ctry_cols,
      First   = as.numeric(unlist(fr[, ctry_cols])),
      Last    = as.numeric(unlist(lr[, ctry_cols]))
    ) %>%
      mutate(Change = table_change(First, Last, change_type)) %>%
      rename_with(~ paste0(fld, "_", .x), c(First, Last, Change))
  })
  
  value_cols  <- c(paste0(field_labels, "_First"), paste0(field_labels, "_Last"))
  change_cols <- paste0(field_labels, "_Change")
  
  merged <- Reduce(function(a, b) full_join(a, b, by = "Country"), per_field) %>%
    mutate(Country = recode(Country, !!!recode_countries)) %>%
    mutate(.tot = rowSums(across(all_of(paste0(field_labels, "_Last"))), na.rm = TRUE)) %>%
    arrange(desc(.tot)) %>%
    select(-.tot)
  
  # capture the recoded plain-text country names before the stub gets flag markup
  country_names <- merged$Country
  merged <- add_country_flag_stub(merged, "Country")
  
  tbl <- merged %>%
    gt(rowname_col = "Country", id = name) %>%
    fmt_markdown(columns = "Country") %>%
    fmt_integer(columns = all_of(value_cols))
  
  if (change_type == "relative") {
    tbl <- tbl %>% fmt_number(columns = all_of(change_cols),
                              decimals = 1, force_sign = TRUE, pattern = "{x}%")
  } else {
    tbl <- tbl %>% fmt_integer(columns = all_of(change_cols), force_sign = TRUE)
  }
  
  tbl <- tbl %>% sub_missing(missing_text = "\u2014")
  
  for (fld in field_labels) {
    tbl <- tbl %>% tab_spanner(label = fld, columns = starts_with(paste0(fld, "_")))
  }
  
  # --- FIX: resolve column labels to a named vector before the gt call, so
  # gt never has to evaluate `first_year`/`last_year` inside cols_label ---
  first_cols  <- grep("_First$",  names(merged), value = TRUE)
  last_cols   <- grep("_Last$",   names(merged), value = TRUE)
  chg_lbl_cols <- grep("_Change$", names(merged), value = TRUE)
  
  label_vec <- c(
    setNames(rep(as.character(first_year), length(first_cols)),   first_cols),
    setNames(rep(as.character(last_year),  length(last_cols)),    last_cols),
    setNames(rep("Change",                 length(chg_lbl_cols)), chg_lbl_cols)
  )
  
  tbl <- tbl %>% cols_label(.list = label_vec)
  
  # per-field symmetric diverging gradient (fields differ by orders of
  # magnitude, so each change column gets its own domain)
  for (col in change_cols) {
    lim <- max(abs(merged[[col]]), na.rm = TRUE)
    tbl <- tbl %>%
      data_color(columns = all_of(col),
                 fn = scales::col_numeric(
                   palette  = c(tpa_colors[2], "white", tpa_colors[1]),
                   domain   = c(-lim, lim), na.color = "white"))
  }
  
  # pre-resolve footnote year strings too (same scoping class of issue)
  fy_lab <- as.character(first_year)
  ly_lab <- as.character(last_year)
  
  tbl %>%
    tab_style(style = cell_fill(color = "white"),
              locations = cells_body(columns = all_of(value_cols))) %>%
    cols_align(align = "right", columns = -Country) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = all_of(change_cols))) %>%
    tab_style(style = cell_text(align = "left"),
              locations = cells_stub(rows = TRUE)) %>%
    tab_footnote(
      footnote = end_sentence(
        "Patents granted by each country's patent office. Values are grants in",
        fy_lab, "and", ly_lab, "with the",
        paste0(change_phrase(change_type, "count"), " between them."),
        "Countries are ordered by total", ly_lab,
        "grants across the three fields; \u2014 marks a year with no reported",
        "grants"),
      locations = cells_column_spanners(spanners = field_labels)
    ) %>%
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      name, tpa_colors[1])) %>%
    with_table_profile("ultrawide") %>%
    with_stub_width(table_scaled_px(170)) %>%
    with_size("standard")
}

# ---- Count + share bookend table (two entities) -------------
# Companion to fig_046. Flat table, one row per entity (default: US, China),
# reading a wide CSV with a Year column plus <prefix>_Count and <prefix>_Share
# columns per entity. Pulls two bookend years and reports, per entity, the
# count and share in each year, plus change under the selected change_type.
# Two spanners group the count block and the
# share block. Only the share-change column carries the diverging color scale
# by default -- the share move is the analytic point, the count change is context;
# set color_count = TRUE to color both (as build_region_table does).
#   entities : named list mapping display label -> column prefix, in row order,
#              e.g. list("United States" = "US", "China" = "CN")
#   share_is_fraction : TRUE if *_Share columns are 0-1 (multiplied by 100)
# ---- Count + share bookend table (two entities) -------------
build_count_share_bookend_table <- function(name, meta,
                                            first, last,
                                            entities = list("United States" = "US",
                                                            "China"         = "CN"),
                                            count_spanner = "Count",
                                            share_spanner = "Share",
                                            share_is_fraction = TRUE,
                                            color_count = FALSE,
                                            change_type = c("ppt", "relative"),
                                            footnote = NULL) {
  
  change_type <- resolve_change_type(change_type)
  raw <- read_fig(name) %>%
    rename(Year = 1) %>%
    mutate(across(everything(), ~ suppressWarnings(as.numeric(.)))) %>%
    filter(Year %in% c(first, last))
  
  fr <- raw %>% filter(Year == first) %>% slice(1)
  lr <- raw %>% filter(Year == last)  %>% slice(1)
  
  sh_mult <- if (share_is_fraction) 100 else 1
  
  df <- tibble(
    Entity    = names(entities),
    cnt_first = vapply(entities, function(p) fr[[paste0(p, "_Count")]], numeric(1)),
    cnt_last  = vapply(entities, function(p) lr[[paste0(p, "_Count")]], numeric(1)),
    sh_first  = vapply(entities, function(p) fr[[paste0(p, "_Share")]], numeric(1)) * sh_mult,
    sh_last   = vapply(entities, function(p) lr[[paste0(p, "_Share")]], numeric(1)) * sh_mult
  ) %>%
    mutate(
      cnt_chg = table_change(cnt_first, cnt_last, change_type),
      sh_chg  = table_change(sh_first, sh_last, change_type)
    )
  
  sh_lim  <- max(abs(df$sh_chg),  na.rm = TRUE)
  cnt_lim <- max(abs(df$cnt_chg), na.rm = TRUE)
  
  df <- add_country_flag_stub(df, "Entity")
  
  tbl <- df %>%
    gt(rowname_col = "Entity", id = name) %>%
    fmt_markdown(columns = "Entity") %>%
    fmt_integer(columns = c(cnt_first, cnt_last)) %>%
    fmt_number(columns = c(sh_first, sh_last), decimals = 1, pattern = "{x}%")
  
  if (change_type == "relative") {
    tbl <- tbl %>%
      fmt_number(columns = c(cnt_chg, sh_chg), decimals = 1,
                 force_sign = TRUE, pattern = "{x}%")
  } else {
    tbl <- tbl %>%
      fmt_integer(columns = cnt_chg, force_sign = TRUE) %>%
      fmt_number(columns = sh_chg, decimals = 1,
                 force_sign = TRUE, pattern = "{x} pp")
  }
  
  tbl <- tbl %>%
    tab_spanner(label = count_spanner, columns = c(cnt_first, cnt_last, cnt_chg)) %>%
    tab_spanner(label = share_spanner, columns = c(sh_first, sh_last, sh_chg)) %>%
    cols_label(
      cnt_first = as.character(first), cnt_last = as.character(last), cnt_chg = "Change",
      sh_first  = as.character(first), sh_last  = as.character(last), sh_chg  = "Change"
    ) %>%
    data_color(
      columns = sh_chg,
      fn = scales::col_numeric(
        palette = c(tpa_colors[2], "white", tpa_colors[1]),
        domain  = c(-sh_lim, sh_lim))
    )
  
  if (color_count) {
    tbl <- tbl %>%
      data_color(
        columns = cnt_chg,
        fn = scales::col_numeric(
          palette = c(tpa_colors[2], "white", tpa_colors[1]),
          domain  = c(-cnt_lim, cnt_lim)))
  } else {
    tbl <- tbl %>%
      tab_style(style = cell_fill(color = "white"),
                locations = cells_body(columns = cnt_chg))
  }
  
  tbl <- tbl %>%
    tab_style(style = cell_fill(color = "white"),
              locations = cells_body(columns = c(cnt_first, cnt_last,
                                                 sh_first, sh_last))) %>%
    cols_align(align = "right",
               columns = c(cnt_first, cnt_last, cnt_chg,
                           sh_first, sh_last, sh_chg)) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = c(cnt_chg, sh_chg))) %>%
    tab_style(style = cell_text(align = "left"),
              locations = cells_stub(rows = TRUE))
  
  if (!is.null(footnote)) {
    tbl <- tbl %>%
      tab_footnote(footnote = footnote,
                   locations = cells_column_spanners(
                     spanners = c(count_spanner, share_spanner)))
  }
  
  tbl %>%
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      name, tpa_colors[1])) %>%
    with_size("standard")
}

# ---- STEM outcomes-by-cohort table --------------------------
# Companion to fig_038a. Reads a wide file: a `cohort` column plus one
# column per outcome, where values are the share of that cohort's STEM
# entrants landing in each outcome (rows sum to 100%). One row per cohort,
# columns in the same order as the figure's stack. Each outcome column
# gets its own continuous scale so trends read within a column across
# cohorts; stem_bachelors (the headline outcome) is bolded.
build_stem_outcomes_table <- function(name, meta) {
  outcome_levels <- c(
    stem_bachelors    = "Bachelor's in STEM",
    nonstem_bachelors = "Bachelor's in another field",
    subbac_credential = "Associate's or certificate",
    still_enrolled    = "Still enrolled, no degree",
    left_no_degree    = "Left without a degree"
  )
  outcome_cols <- names(outcome_levels)
  
  df <- read_fig(name) %>%
    mutate(cohort = as.character(cohort),
           across(-cohort, ~ suppressWarnings(as.numeric(.)))) %>%
    select(cohort, all_of(outcome_cols))
  
  tbl <- df %>%
    gt(rowname_col = "cohort", id = name) %>%
    fmt_number(columns = all_of(outcome_cols), decimals = 1, pattern = "{x}%") %>%
    cols_label(!!!outcome_levels) %>%
    tab_spanner(label = "Share of STEM entrants", columns = all_of(outcome_cols))
  
  for (col in outcome_cols) {
    tbl <- tbl %>%
      gt_color_rows(columns  = all_of(col),
                    palette  = c(TPA_RED_LIGHT, tpa_colors[1]),
                    pal_type = "continuous")
  }
  
  tbl %>%
    cols_align(align = "right", columns = all_of(outcome_cols)) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = stem_bachelors)) %>%
    tab_style(style = cell_text(align = "left"),
              locations = cells_stub(rows = TRUE)) %>%
    tab_footnote(
      footnote = paste("Values are the share of each entering STEM cohort ending in",
                       "each outcome; rows sum to 100%. Cohorts are ordered by entry year."),
      locations = cells_column_spanners(spanners = "Share of STEM entrants")
    ) %>%
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      name, tpa_colors[1])) %>%
    with_size("standard")
}


# ---- Company patent bookend + interleaved-delta table -------
# Flat company list for the patents-by-company section, each row tagged
# with a circular country flag merged into the company stub (flag first,
# then name). Grant counts shown as levels with the change between each
# successive pair interleaved (if any), plus a net change. Levels are
# integer counts; change columns get a per-column symmetric diverging scale.
# df arrives pivoted wide with columns in display order:
# Company, flag, <level>, [<chg>, <level>, ...], Change, where flag holds
# relative paths to inputs/flags/<slug>.png. Merging the flag into the stub keeps
# it off the spanner row, so no red rule appears above it. chg_cols may be
# empty (character(0)) to show levels plus net change only.
# change_type controls relative versus native-unit change formatting;
# compute matching values upstream.
build_patent_company_delta_table <- function(df, name, meta,
                                             level_cols, chg_cols,
                                             spanner_label = "Patents granted",
                                             change_type   = c("ppt", "relative"),
                                             show_flag     = TRUE,
                                             flag_height   = 18,
                                             footnote      = NULL) {
  change_type  <- resolve_change_type(change_type)
  all_chg_cols <- c(chg_cols, "Change")
  numeric_cols <- c(level_cols, all_chg_cols)
  
  # interleaved display block: level, chg, level, chg, level (contiguous)
  span_order <- character(0)
  for (i in seq_along(level_cols)) {
    span_order <- c(span_order, level_cols[i])
    if (i <= length(chg_cols)) span_order <- c(span_order, chg_cols[i])
  }
  
  if (is.null(footnote)) {
    footnote <- end_sentence(
      "The first, middle, and last columns are patent grants in each year. The",
      "interior columns show the change from the prior shown year rather than",
      "the level;", change_col_label(length(chg_cols) > 0), "is the change",
      "across the full window. Changes are",
      if (change_type == "relative") "relative (percent)" else "absolute counts of grants"
    )
  }
  
  level_labels <- setNames(gsub("^Y", "", level_cols), level_cols)
  chg_labels   <- setNames(rep("Change", length(chg_cols)), chg_cols)
  
  # Column widths are deliberately not set here; save_table() owns geometry.
  tbl <- df %>%
    gt(rowname_col = "Company", id = name) %>%
    fmt_markdown(columns = "Company") %>%
    fmt_integer(columns = all_of(level_cols))
  
  if (change_type == "relative") {
    tbl <- tbl %>% fmt_number(columns = all_of(all_chg_cols), decimals = 1,
                              force_sign = TRUE, pattern = "{x}%")
  } else {
    tbl <- tbl %>% fmt_integer(columns = all_of(all_chg_cols), force_sign = TRUE)
  }
  
  # flag icon merged into the Company stub, ahead of the name
  if (show_flag) {
    tbl <- tbl %>%
      fmt_image(columns = flag, height = flag_height) %>%
      cols_merge(columns = c(Company, flag), pattern = "{2}&nbsp;&nbsp;{1}")
  }
  
  label_args <- c(level_labels, chg_labels,
                  list(Change = change_col_label(length(chg_cols) > 0)))
  tbl <- tbl %>%
    tab_spanner(label = spanner_label, columns = all_of(span_order)) %>%
    cols_label(.list = label_args)
  
  for (col in all_chg_cols) {
    lim <- max(abs(df[[col]]), na.rm = TRUE)
    tbl <- tbl %>%
      data_color(columns = all_of(col),
                 fn = scales::col_numeric(
                   palette = c(tpa_colors[2], "white", tpa_colors[1]),
                   domain  = c(-lim, lim), na.color = "white"))
  }
  
  tbl <- tbl %>%
    tab_style(style = cell_fill(color = "white"),
              locations = cells_body(columns = all_of(level_cols))) %>%
    tab_style(style = cell_text(align = "left"),
              locations = cells_stub(rows = TRUE)) %>%
    cols_align(align = "right", columns = all_of(numeric_cols))
  
  tbl %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = Change)) %>%
    tab_footnote(footnote = footnote,
                 locations = cells_column_spanners(spanners = spanner_label)) %>%
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      name, tpa_colors[1])) %>%
    with_size("standard")
}

build_conference_summary_table_split <- function(name, meta, conf_names,
                                                 data_name = name,
                                                 us_label = "United States",
                                                 cn_label = "China",
                                                 row_slice = NULL,
                                                 change_type = c("ppt", "relative")) {
  
  change_type <- resolve_change_type(change_type)
  raw <- read_fig(data_name) %>%
    transmute(
      conf    = as.character(conf_norm),
      year    = suppressWarnings(as.integer(year)),
      country = as.character(country),
      share   = suppressWarnings(as.numeric(share)) * 100
    ) %>%
    filter(country %in% c(us_label, cn_label), !is.na(year), !is.na(share)) %>%
    mutate(country = if_else(country == us_label, "US", "CN"))
  
  wide <- raw %>%
    pivot_wider(
      names_from = country,
      values_from = share,
      values_fn = ~ mean(.x, na.rm = TRUE)
    )
  
  summ <- wide %>%
    group_by(conf) %>%
    group_modify(~ {
      d   <- .x %>% arrange(year)
      bp  <- !is.na(d$US) & !is.na(d$CN)
      ayf <- if (any(bp)) min(d$year[bp]) else min(d$year)
      ayl <- if (any(bp)) max(d$year[bp]) else max(d$year)
      hit <- bp & (d$CN > d$US)
      
      tibble(
        first_yr = ayf,
        last_yr  = ayl,
        US_First = d$US[d$year == ayf][1],
        CN_First = d$CN[d$year == ayf][1],
        US_Last  = d$US[d$year == ayl][1],
        CN_Last  = d$CN[d$year == ayl][1],
        cross_yr = if (any(hit)) min(d$year[hit]) else NA_integer_
      )
    }) %>%
    ungroup() %>%
    mutate(
      US_Change  = table_change(US_First, US_Last, change_type),
      CN_Change  = table_change(CN_First, CN_Last, change_type),
      # A venue observed in only one year would otherwise read "2022-2022".
      Coverage   = if_else(first_yr == last_yr,
                           as.character(first_yr),
                           paste0(first_yr, "\u2013", last_yr)),
      Conference = recode(conf, !!!conf_names),
      cross_yr   = as.character(cross_yr)
    ) %>%
    arrange(desc(CN_Change))
  
  if (!is.null(row_slice)) {
    summ <- summ %>% slice(row_slice)
  }
  
  summ <- summ %>%
    select(
      Conference, Coverage,
      US_First, US_Last, US_Change,
      CN_First, CN_Last, CN_Change,
      cross_yr
    )
  
  lim <- max(abs(c(summ$US_Change, summ$CN_Change)), na.rm = TRUE)
  
  summ %>%
    gt(rowname_col = "Conference", id = name) %>%
    fmt_number(columns = c(US_First, US_Last, CN_First, CN_Last),
               decimals = 1, pattern = "{x}%") %>%
    fmt_number(columns = c(US_Change, CN_Change),
               decimals = 1, force_sign = TRUE,
               pattern = if (change_type == "relative") "{x}%" else "{x} pp") %>%
    sub_missing(columns = c(US_First, US_Last, CN_First, CN_Last,
                            US_Change, CN_Change), missing_text = "\u2014") %>%
    sub_missing(columns = cross_yr, missing_text = "Not yet") %>%
    cols_label(
      Coverage  = "Coverage",
      US_First  = "First",
      US_Last   = "Latest",
      US_Change = "Change",
      CN_First  = "First",
      CN_Last   = "Latest",
      CN_Change = "Change",
      cross_yr  = "China overtook"
    ) %>%
    tab_spanner(label = "\u00A0", columns = Coverage, id = "sp_cov") %>%
    tab_spanner(label = us_label, columns = c(US_First, US_Last, US_Change)) %>%
    tab_spanner(label = cn_label, columns = c(CN_First, CN_Last, CN_Change)) %>%
    tab_spanner(label = "\u00A0", columns = cross_yr, id = "sp_cross") %>%
    data_color(
      columns = c(US_Change, CN_Change),
      fn = scales::col_numeric(
        palette  = c(tpa_colors[2], "white", tpa_colors[1]),
        domain   = c(-lim, lim),
        na.color = "white"
      )
    ) %>%
    tab_style(style = cell_fill(color = "white"),
              locations = cells_body(columns = c(US_First, US_Last, CN_First, CN_Last))) %>%
    cols_align(align = "right",
               columns = c(US_First, US_Last, US_Change, CN_First, CN_Last, CN_Change, cross_yr)) %>%
    cols_align(align = "left", columns = Coverage) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = c(US_Change, CN_Change))) %>%
    tab_style(style = cell_text(align = "left"),
              locations = cells_stub(rows = TRUE)) %>%
    tab_footnote(
      footnote = end_sentence(
        "Each row gives the U.S. and Chinese share of accepted author",
        "affiliations at the first and last year both countries are observed;",
        "that window is shown under Coverage and differs by venue.",
        change_col_label(FALSE), "is the", paste0(change_phrase(change_type), "."),
        "\u201CChina overtook\u201D is the first such year China's share",
        "exceeded the U.S. share; \u201CNot yet\u201D means the U.S. share still",
        "led in the latest observed year"
      ),
      locations = cells_column_labels(columns = cross_yr)
    ) %>%
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      name, tpa_colors[1]
    )) %>%
    with_size("standard")
}

# ---- Export -------------------------------------------------

save_table <- function(gt_tbl, name, size = NULL, profile = NULL, stub_width_px = NULL) {
  if (is.null(size)) {
    size <- attr(gt_tbl, "tpa_size")
    if (is.null(size)) size <- "standard"
  }
  
  profile <- profile %||% infer_table_profile(name, gt_tbl)
  p <- table_profile(profile)
  
  if (!size %in% names(TABLE_EXPORT_HEIGHTS_PX)) {
    stop(
      "Unknown size: ", size,
      ". Use one of: ", paste(names(TABLE_EXPORT_HEIGHTS_PX), collapse = ", "),
      "."
    )
  }
  
  html_dir <- file.path(PATHS$final_tables, "html")
  png_dir  <- file.path(PATHS$final_tables, "pngs")
  dir.create(html_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(png_dir,  showWarnings = FALSE, recursive = TRUE)
  
  png_path  <- file.path(png_dir,  paste0(name, ".png"))
  html_path <- file.path(html_dir, paste0(name, ".html"))
  
  # Builders own content; save_table() owns final geometry.
  gt_tbl <- apply_table_profile(
    gt_tbl,
    profile = profile,
    stub_width_px = stub_width_px
  )
  
  overflow_px <- attr(gt_tbl, "tpa_overflow_px") %||% 0
  
  # Browser headroom only. The PNG is cropped to the rendered table.
  height_scale <- p$width_px / TABLE_CSS_WIDTH_PX
  vheight <- ceiling(
    unname(TABLE_EXPORT_HEIGHTS_PX[[size]]) * height_scale
  )
  
  # All profiles land at exactly the same publication raster width.
  export_zoom <- TABLE_EXPORT_PX / p$width_px
  
  gtsave(gt_tbl, html_path)
  
  gtsave(
    gt_tbl,
    png_path,
    vwidth  = p$width_px,
    vheight = vheight,
    zoom    = export_zoom,
    expand  = 0
  )
  
  actual_width_px  <- NA_integer_
  actual_height_px <- NA_integer_
  
  if (requireNamespace("magick", quietly = TRUE)) {
    img  <- magick::image_read(png_path)
    info <- magick::image_info(img)
    
    actual_width_px  <- as.integer(info$width[1])
    actual_height_px <- as.integer(info$height[1])
    width_delta <- abs(actual_width_px - TABLE_EXPORT_PX)
    
    if (width_delta > TABLE_EXPORT_WIDTH_TOLERANCE_PX) {
      stop(
        "Table ", name, " exported at ", actual_width_px, " px wide; expected ",
        TABLE_EXPORT_PX, " px (", WORD_TABLE_WIDTH_IN, " in at ",
        TABLE_EXPORT_DPI, " dpi). Profile: ", profile, ".",
        call. = FALSE
      )
    }
    
    # Normalize a rare browser rounding difference of one or two pixels.
    if (actual_width_px != TABLE_EXPORT_PX) {
      img <- magick::image_resize(
        img,
        geometry = paste0(TABLE_EXPORT_PX, "x")
      )
      info <- magick::image_info(img)
      actual_width_px  <- as.integer(info$width[1])
      actual_height_px <- as.integer(info$height[1])
    }
    
    magick::image_write(
      img,
      path    = png_path,
      density = paste0(TABLE_EXPORT_DPI, "x", TABLE_EXPORT_DPI)
    )
  }
  
  status <- if (is.na(actual_width_px)) {
    "not checked (magick unavailable)"
  } else if (actual_width_px == TABLE_EXPORT_PX) {
    "PASS"
  } else {
    "FAIL"
  }
  
  TABLE_EXPORT_AUDIT <<- dplyr::bind_rows(
    TABLE_EXPORT_AUDIT,
    tibble::tibble(
      name = name,
      profile = profile,
      size = size,
      css_width_px = as.integer(p$width_px),
      export_zoom = export_zoom,
      expected_width_px = as.integer(TABLE_EXPORT_PX),
      actual_width_px = actual_width_px,
      actual_height_px = actual_height_px,
      overflow_px = as.numeric(overflow_px),
      body_font_pt = TABLE_DATA_FONT_PT,
      header_font_pt = TABLE_COLUMN_LABEL_FONT_PT,
      column_label_font_pt = TABLE_COLUMN_LABEL_FONT_PT,
      spanner_font_pt = TABLE_SPANNER_FONT_PT,
      title_font_pt = TABLE_TITLE_FONT_PT,
      source_font_pt = TABLE_SOURCE_FONT_PT,
      row_padding_pt = PUB$spacing$table_row_pad_pt,
      column_padding_pt = PUB$spacing$table_col_pad_pt,
      heading_padding_pt = PUB$spacing$table_heading_pad_pt,
      source_padding_pt = PUB$spacing$table_source_pad_pt,
      body_lineheight = TABLE_BODY_LINEHEIGHT,
      header_lineheight = TABLE_HEADER_LINEHEIGHT,
      title_lineheight = TABLE_TITLE_LINEHEIGHT,
      source_lineheight = TABLE_SOURCE_LINEHEIGHT,
      status = status
    )
  )
  
  gt_tbl
}

write_table_export_audit <- function(
    path = file.path(PATHS$final_tables, "table_layout_audit.csv"),
    expected_names = NULL
) {
  audit <- get_table_export_audit()
  
  if (!is.null(expected_names)) {
    missing <- setdiff(expected_names, audit$name)
    extra   <- setdiff(audit$name, expected_names)
    
    if (length(missing) > 0) {
      stop(
        "Table audit is missing expected exports: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    
    if (length(extra) > 0) {
      warning(
        "Table audit contains additional exports: ",
        paste(extra, collapse = ", "),
        call. = FALSE
      )
    }
  }
  
  # A PNG can be exactly the right raster width and still have lost its
  # rightmost column, because the crop happens at the viewport edge. The
  # per-table overflow figure is the only thing that catches that.
  clipped <- audit %>% dplyr::filter(!is.na(overflow_px), overflow_px > 0)
  if (nrow(clipped) > 0) {
    stop(
      "Table columns overflow their profile (content will be clipped): ",
      paste0(clipped$name, " (+", round(clipped$overflow_px), "px)",
             collapse = ", "),
      call. = FALSE
    )
  }
  
  checked <- audit %>% dplyr::filter(!is.na(actual_width_px))
  if (nrow(checked) > 0 && any(checked$actual_width_px != checked$expected_width_px)) {
    bad <- checked %>%
      dplyr::filter(actual_width_px != expected_width_px) %>%
      dplyr::pull(name)
    stop(
      "Not all table PNGs have the common publication width: ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }
  
  invariant_cols <- c(
    "expected_width_px",
    "body_font_pt",
    "header_font_pt",
    "column_label_font_pt",
    "spanner_font_pt",
    "title_font_pt",
    "source_font_pt",
    "row_padding_pt",
    "column_padding_pt",
    "heading_padding_pt",
    "source_padding_pt",
    "body_lineheight",
    "header_lineheight",
    "title_lineheight",
    "source_lineheight"
  )
  
  for (col in invariant_cols) {
    vals <- unique(audit[[col]][!is.na(audit[[col]])])
    if (length(vals) > 1) {
      stop(
        "Table style audit failed: ", col,
        " is not constant across exports.",
        call. = FALSE
      )
    }
  }
  
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(audit, path)
  invisible(audit)
}