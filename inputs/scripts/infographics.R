# Shared helpers for the six "By the Numbers" chapter infographics.
#
# The report architecture remains authoritative:
#   - read_fig() and DATA_DIR come from helpers.R
#   - titles_and_sources and get_meta() come from helpers.R
#   - theme constants come from theme.R
#   - fixed_table_width() comes from tables.R
#
# This file only adds infographic-specific endpoint selection, formatting,
# layout, and exact-size export. No observations or endpoint years live here.

INFOGRAPHIC_SPEC <- list(
  # Keep infographics aligned to the same usable publication width as figures
  # and tables. If page margins change in publication_spec.R, this updates too.
  width_in = PUB$page$body_in,
  height_in = PUB$infographic$height_in,
  dpi = PUB$render$figure_dpi,
  css_dpi = PUB$infographic$css_dpi
)

INFOGRAPHIC_SPEC$css_width_px <- round(
  INFOGRAPHIC_SPEC$width_in * INFOGRAPHIC_SPEC$css_dpi
)
INFOGRAPHIC_SPEC$css_height_px <- round(
  INFOGRAPHIC_SPEC$height_in * INFOGRAPHIC_SPEC$css_dpi
)
INFOGRAPHIC_SPEC$output_width_px <- round(
  INFOGRAPHIC_SPEC$width_in * INFOGRAPHIC_SPEC$dpi
)
INFOGRAPHIC_SPEC$output_height_px <- round(
  INFOGRAPHIC_SPEC$height_in * INFOGRAPHIC_SPEC$dpi
)
INFOGRAPHIC_SPEC$zoom <- INFOGRAPHIC_SPEC$dpi / INFOGRAPHIC_SPEC$css_dpi

# All infographic pages use the same five physical column proportions.
# The stub gets half the width so descriptive measures can remain on one line.
# These proportions automatically follow INFOGRAPHIC_SPEC$width_in.
INFOGRAPHIC_COLUMN_SHARES <- c(
  measure = 0.50,
  earlier = 0.16,
  latest  = 0.20,
  change  = 0.115,
  trend   = 0.025
)

INFOGRAPHIC_COLUMNS <- round(
  INFOGRAPHIC_SPEC$css_width_px * INFOGRAPHIC_COLUMN_SHARES
)

# Rounding can leave the total one or two pixels off. Put any residual pixel
# into the measure column so the table width remains exact.
INFOGRAPHIC_COLUMNS[["measure"]] <- INFOGRAPHIC_COLUMNS[["measure"]] +
  (INFOGRAPHIC_SPEC$css_width_px - sum(INFOGRAPHIC_COLUMNS))

# Reuse the report palette wherever a matching theme constant already exists.
INFOGRAPHIC_PALETTE <- list(
  red = tpa_colors[[1]],
  gray = "#887E6F",
  rule = "#D8D3CB",
  tint = "#F7F4EF",
  pink = TPA_RED_LIGHT,
  text = "grey20",
  muted = "grey40"
)

stopifnot(
  sum(INFOGRAPHIC_COLUMNS) == INFOGRAPHIC_SPEC$css_width_px
)

# -----------------------------------------------------------------------------
# Source-aware loading and metadata
# -----------------------------------------------------------------------------

read_infographic_source <- function(id, meta_id = id) {
  out <- read_fig(id)
  attr(out, "tpa_data_id") <- id
  attr(out, "tpa_meta_id") <- meta_id
  out
}

read_first_available_source <- function(..., meta_id = NULL) {
  ids <- c(...)
  
  for (id in ids) {
    path <- file.path(DATA_DIR, paste0(id, ".csv"))
    if (file.exists(path)) {
      return(read_infographic_source(id, meta_id = meta_id %||% id))
    }
  }
  
  stop(
    "Could not find any of the requested files: ",
    paste(ids, collapse = ", "),
    "."
  )
}

infographic_data_id <- function(df) {
  attr(df, "tpa_data_id", exact = TRUE) %||% NA_character_
}

infographic_meta_id <- function(df) {
  attr(df, "tpa_meta_id", exact = TRUE) %||%
    infographic_data_id(df)
}

metadata_record <- function(id) {
  if (!exists("titles_and_sources", inherits = TRUE)) {
    stop("titles_and_sources has not been loaded. Source helpers.R first.")
  }
  
  out <- titles_and_sources %>%
    dplyr::filter(.data$fig_no == .env$id)
  
  if (nrow(out) == 0) {
    stop("No titles_and_sources entry was found for `", id, "`.")
  }
  
  out[1, , drop = FALSE]
}

metadata_text <- function(id) {
  row <- metadata_record(id)
  fields <- intersect(
    c("Actual Figure", "title", "title_long", "source_short", "source_long"),
    names(row)
  )
  
  values <- unlist(row[fields], use.names = FALSE)
  values <- as.character(values[!is.na(values) & nzchar(trimws(values))])
  paste(values, collapse = " ")
}

extract_year <- function(x, use_last = FALSE) {
  matches <- stringr::str_extract_all(
    as.character(x),
    "(?<![0-9])(?:18|19|20|21)[0-9]{2}(?![0-9])"
  )
  
  vapply(
    matches,
    function(found) {
      if (length(found) == 0) return(NA_real_)
      as.numeric(if (use_last) tail(found, 1) else found[[1]])
    },
    numeric(1)
  )
}

extract_short_year_ranges <- function(x) {
  matches <- stringr::str_match_all(
    as.character(x),
    "(?<![0-9])((?:18|19|20|21)[0-9]{2})\\s*[–—-]\\s*([0-9]{2})(?![0-9])"
  )
  
  purrr::map_dfr(matches, function(m) {
    if (nrow(m) == 0) return(tibble::tibble())
    
    start <- as.numeric(m[, 2])
    suffix <- as.numeric(m[, 3])
    end <- floor(start / 100) * 100 + suffix
    end <- ifelse(end < start, end + 100, end)
    
    tibble::tibble(start = start, end = end)
  })
}

extract_academic_year_pairs <- function(x) {
  matches <- stringr::str_match_all(
    as.character(x),
    "(?<![0-9])((?:18|19|20|21)[0-9]{2})\\s*/\\s*([0-9]{2})(?![0-9])"
  )
  
  purrr::map_dfr(matches, function(m) {
    if (nrow(m) == 0) return(tibble::tibble())
    
    start <- as.numeric(m[, 2])
    suffix <- as.numeric(m[, 3])
    end <- floor(start / 100) * 100 + suffix
    end <- ifelse(end < start, end + 100, end)
    
    tibble::tibble(start = start, end = end)
  })
}

metadata_years <- function(id) {
  text <- metadata_text(id)

  # metadata_text() is one combined citation string. The generic extract_year()
  # helper intentionally returns one year per input label, so using it here
  # previously retained only the FIRST year it encountered. A citation such as
  # "1995–2024" was therefore interpreted as 1995–1995.
  #
  # Metadata ranges need every explicit four-digit year in the citation.
  full <- stringr::str_extract_all(
    as.character(text),
    "(?<![0-9])(?:18|19|20|21)[0-9]{2}(?![0-9])"
  )[[1]]
  full <- suppressWarnings(as.numeric(full))
  full <- full[is.finite(full)]

  academic_pairs <- extract_academic_year_pairs(text)
  short_ranges <- extract_short_year_ranges(text)

  sort(unique(c(
    full,
    academic_pairs$start,
    academic_pairs$end,
    short_ranges$start,
    short_ranges$end
  )))
}

metadata_year_range <- function(id) {
  years <- metadata_years(id)
  if (length(years) == 0) {
    stop("No year could be read from titles_and_sources for `", id, "`.")
  }
  
  c(first = min(years), last = max(years))
}

metadata_latest_year <- function(id) {
  unname(metadata_year_range(id)[["last"]])
}

source_latest_year <- function(df) {
  years <- if ("Year" %in% names(df)) valid_years(df) else numeric()
  if (length(years) > 0) return(max(years))
  metadata_latest_year(infographic_meta_id(df))
}

# -----------------------------------------------------------------------------
# Generic data parsing and endpoint selection
# -----------------------------------------------------------------------------

numeric_clean <- function(x) {
  suppressWarnings(
    as.numeric(
      gsub(
        pattern = "[,$%]",
        replacement = "",
        x = as.character(x)
      )
    )
  )
}

year_number <- function(x) {
  period_end_year(x)
}

normalize_label <- function(x) {
  tolower(
    trimws(
      gsub(
        pattern = "\\*",
        replacement = "",
        x = as.character(x)
      )
    )
  )
}

valid_years <- function(df, year_column = "Year") {
  if (!year_column %in% names(df)) return(numeric())
  years <- year_number(df[[year_column]])
  sort(unique(years[is.finite(years)]))
}

series_data <- function(df, column, year_column = "Year") {
  missing <- setdiff(c(year_column, column), names(df))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), ".")
  }
  
  out <- tibble::tibble(
    Year = year_number(df[[year_column]]),
    Value = numeric_clean(df[[column]])
  ) %>%
    dplyr::filter(is.finite(.data$Year), is.finite(.data$Value)) %>%
    dplyr::arrange(.data$Year)
  
  if (nrow(out) == 0) {
    stop("No usable observations were found for `", column, "`.")
  }
  
  duplicate_years <- out %>%
    dplyr::count(.data$Year) %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::pull(.data$Year)
  
  for (year in duplicate_years) {
    values <- unique(out$Value[out$Year == year])
    if (length(values) > 1) {
      stop(
        "Multiple different values were found for `", column,
        "` in ", year, "."
      )
    }
  }
  
  out %>% dplyr::distinct(.data$Year, .keep_all = TRUE)
}

period_end_year <- function(x) {
  labels <- trimws(as.character(x))
  out <- suppressWarnings(as.numeric(labels))
  
  # Academic/fiscal periods may be written as 2024/25, 2024/2025,
  # 2024-25, or 2024-2025. For those labels, use the ending year.
  matches <- stringr::str_match(
    labels,
    "(?<![0-9])((?:18|19|20|21)[0-9]{2})\\s*[/–—-]\\s*((?:18|19|20|21)[0-9]{2}|[0-9]{2})(?![0-9])"
  )
  
  has_period <- !is.na(matches[, 1])
  if (any(has_period)) {
    start_year <- as.numeric(matches[has_period, 2])
    end_token <- matches[has_period, 3]
    end_year <- suppressWarnings(as.numeric(end_token))
    
    short_end <- nchar(end_token) == 2
    end_year[short_end] <-
      floor(start_year[short_end] / 100) * 100 + end_year[short_end]
    end_year[short_end & end_year < start_year] <-
      end_year[short_end & end_year < start_year] + 100
    
    out[has_period] <- end_year
  }
  
  missing <- !is.finite(out)
  if (any(missing)) {
    out[missing] <- extract_year(labels[missing], use_last = FALSE)
  }
  
  out
}

calendar_year_label <- function(year) {
  parsed <- period_end_year(year)
  ifelse(is.finite(parsed), as.character(round(parsed)), as.character(year))
}

# The report data stores academic years by ending year. These aliases remain so
# chapter code can identify academic series without changing how the year prints.
academic_period_label <- function(df, year) {
  calendar_year_label(year)
}

academic_year_label <- function(year) {
  calendar_year_label(year)
}

data_endpoint <- function(
    df,
    column,
    which = c("first", "last"),
    year_column = "Year",
    academic = FALSE
) {
  which <- match.arg(which)
  series <- series_data(df, column, year_column)
  row <- if (which == "first") {
    dplyr::slice_head(series, n = 1)
  } else {
    dplyr::slice_tail(series, n = 1)
  }
  
  label <- if (academic) {
    academic_period_label(df, row$Year[[1]])
  } else {
    calendar_year_label(row$Year[[1]])
  }
  
  list(
    value = row$Value[[1]],
    year = row$Year[[1]],
    label = label
  )
}

projection_endpoints <- function(
    df,
    observed_column,
    projected_column,
    year_column = "Year"
) {
  observed <- data_endpoint(
    df,
    observed_column,
    which = "last",
    year_column = year_column
  )
  projected <- data_endpoint(
    df,
    projected_column,
    which = "last",
    year_column = year_column
  )
  
  list(first = observed, last = projected)
}

row_sum_data <- function(df, columns, year_column = "Year") {
  missing <- setdiff(c(year_column, columns), names(df))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), ".")
  }
  
  values <- as.data.frame(lapply(df[columns], numeric_clean))
  usable <- rowSums(is.finite(as.matrix(values))) > 0
  
  tibble::tibble(
    Year = year_number(df[[year_column]]),
    Value = rowSums(values, na.rm = TRUE),
    Usable = usable
  ) %>%
    dplyr::filter(is.finite(.data$Year), .data$Usable, is.finite(.data$Value)) %>%
    dplyr::select(-Usable) %>%
    dplyr::arrange(.data$Year)
}

common_endpoint_year <- function(
    df,
    columns,
    which = c("first", "last"),
    year_column = "Year"
) {
  which <- match.arg(which)
  common <- Reduce(
    intersect,
    lapply(columns, function(column) series_data(df, column, year_column)$Year)
  )
  
  if (length(common) == 0) {
    stop("No common nonmissing year was found for: ", paste(columns, collapse = ", "), ".")
  }
  
  if (which == "first") min(common) else max(common)
}

row_sum_endpoint <- function(
    df,
    columns,
    which = c("first", "last"),
    year_column = "Year",
    academic = FALSE
) {
  which <- match.arg(which)
  year <- common_endpoint_year(df, columns, which, year_column)
  rows <- row_sum_data(df, columns, year_column) %>%
    dplyr::filter(.data$Year == .env$year)
  
  if (nrow(rows) != 1) {
    stop("Expected one summed observation for ", year, ".")
  }
  
  list(
    value = rows$Value[[1]],
    year = year,
    label = if (academic) academic_period_label(df, year) else calendar_year_label(year)
  )
}

period_endpoint <- function(x, which = c("first", "last")) {
  which <- match.arg(which)
  labels <- as.character(x)
  years <- year_number(labels)
  usable <- is.finite(years) & !is.na(labels) & nzchar(trimws(labels))
  
  if (!any(usable)) stop("No dated periods were found.")
  
  periods <- tibble::tibble(label = labels[usable], year = years[usable]) %>%
    dplyr::distinct(.data$label, .data$year) %>%
    dplyr::arrange(.data$year)
  
  if (which == "first") {
    dplyr::slice_head(periods, n = 1)
  } else {
    dplyr::slice_tail(periods, n = 1)
  }
}

temporal_columns <- function(df, exclude = character()) {
  columns <- setdiff(names(df), exclude)
  years <- year_number(columns)
  keep <- is.finite(years)
  
  out <- tibble::tibble(
    column = columns[keep],
    year = years[keep],
    position = which(keep)
  ) %>%
    dplyr::arrange(.data$year, .data$position)
  
  if (nrow(out) == 0) stop("No dated columns were found.")
  out
}

temporal_column_endpoint <- function(
    df,
    which = c("first", "last"),
    exclude = character()
) {
  which <- match.arg(which)
  periods <- temporal_columns(df, exclude)
  if (which == "first") dplyr::slice_head(periods, n = 1) else dplyr::slice_tail(periods, n = 1)
}

metadata_endpoint <- function(df, which = c("first", "last")) {
  which <- match.arg(which)
  range <- metadata_year_range(infographic_meta_id(df))
  year <- unname(range[[which]])
  list(value = NA_real_, year = year, label = calendar_year_label(year))
}

# -----------------------------------------------------------------------------
# Statistic controls and formatting
# -----------------------------------------------------------------------------

stat_is_enabled <- function(
    stat_id,
    section_id,
    include_all,
    include_sections,
    include_stats
) {
  section_is_on <-
    section_id %in% names(include_sections) &&
    isTRUE(include_sections[[section_id]])
  
  stat_is_on <-
    isTRUE(include_all) ||
    (
      stat_id %in% names(include_stats) &&
        isTRUE(include_stats[[stat_id]])
    )
  
  section_is_on && stat_is_on
}

stat_is_emphasized <- function(stat_id, emphasize_stats = NULL) {
  !is.null(emphasize_stats) &&
    stat_id %in% names(emphasize_stats) &&
    isTRUE(emphasize_stats[[stat_id]])
}

# Add a plus sign to positive values. Negative values retain their sign.
sign_prefix <- function(value) {
  ifelse(value > 0, "+", "")
}

# Standard value types use Chapter 1 precision and notation.
# Currency names state whether the incoming value is already in billions.
format_value <- function(value, value_type) {
  switch(
    value_type,
    
    count = scales::comma(value, accuracy = 1),
    
    signed_count = paste0(
      sign_prefix(value),
      scales::comma(value, accuracy = 1)
    ),
    
    share = scales::percent(value, accuracy = 0.1),
    
    signed_share = paste0(
      sign_prefix(value),
      scales::percent(value, accuracy = 0.1)
    ),
    
    percent_value = paste0(
      scales::number(value, accuracy = 0.1),
      "%"
    ),
    
    rate = scales::number(value, accuracy = 0.1),
    
    signed_score = paste0(
      sign_prefix(value),
      scales::number(value, accuracy = 0.1)
    ),
    
    ratio = paste0(
      scales::number(value, accuracy = 0.1),
      ":1"
    ),
    
    currency_billions = paste0(
      "$",
      scales::number(value, accuracy = 0.1, big.mark = ","),
      "B"
    ),
    
    signed_currency_billions = paste0(
      sign_prefix(value),
      "$",
      scales::number(abs(value), accuracy = 0.1, big.mark = ","),
      "B"
    ),
    
    currency_billions_from_dollars = paste0(
      "$",
      scales::number(value / 1e9, accuracy = 0.1, big.mark = ","),
      "B"
    ),
    
    stop("Unknown value type: ", value_type)
  )
}

format_cell <- function(
    value,
    year_label,
    value_type,
    projected = FALSE
) {
  label <- calendar_year_label(year_label)
  if (projected) label <- paste0(label, "P")
  paste0(format_value(value, value_type), " (", label, ")")
}


format_country_gap_cell <- function(
    value,
    year_label,
    value_type,
    positive_label = "US",
    negative_label = "CN"
) {
  label <- calendar_year_label(year_label)
  
  core <- switch(
    value_type,
    signed_count = scales::comma(abs(value), accuracy = 1),
    signed_share = scales::percent(abs(value), accuracy = 0.1),
    signed_score = scales::number(abs(value), accuracy = 0.1),
    signed_currency_billions = paste0(
      "$",
      scales::number(abs(value), accuracy = 0.1, big.mark = ","),
      "B"
    ),
    stop("Unsupported country-gap value type: ", value_type)
  )
  
  if (!is.finite(value) || value == 0) {
    display <- core
  } else {
    leader <- if (value > 0) positive_label else negative_label
    display <- paste0("+", core, " ", leader)
  }
  
  paste0(display, " (", label, ")")
}

format_change <- function(first, last, change_type) {
  difference <- last - first
  relative_change <- if (isTRUE(all.equal(first, 0))) NA_real_ else (last / first) - 1
  multiple_value <- if (isTRUE(all.equal(first, 0))) NA_real_ else last / first
  
  unavailable <- function(value) {
    if (!is.finite(value)) "—" else NULL
  }
  
  if (change_type == "multiple" && !is.null(unavailable(multiple_value))) return("—")
  if (change_type == "percent" && !is.null(unavailable(relative_change))) return("—")
  
  switch(
    change_type,
    
    multiple = paste0(
      scales::number(multiple_value, accuracy = 0.1),
      "×"
    ),
    
    percent = paste0(
      sign_prefix(relative_change),
      scales::percent(relative_change, accuracy = 1)
    ),
    
    pp_share = paste0(
      sign_prefix(difference),
      scales::number(difference * 100, accuracy = 0.1),
      " pp"
    ),
    
    pp_percent = paste0(
      sign_prefix(difference),
      scales::number(difference, accuracy = 0.1),
      " pp"
    ),
    
    ratio_points = paste0(
      sign_prefix(difference),
      scales::number(difference, accuracy = 0.1),
      " ratio points"
    ),
    
    points = paste0(
      sign_prefix(difference),
      scales::number(difference, accuracy = 0.1),
      " points"
    ),
    
    count_delta = paste0(
      sign_prefix(difference),
      scales::comma(difference, accuracy = 1)
    ),
    
    currency_delta_billions = paste0(
      sign_prefix(difference),
      "$",
      scales::number(abs(difference), accuracy = 0.1, big.mark = ","),
      "B"
    ),
    
    currency_delta_from_dollars = paste0(
      sign_prefix(difference),
      "$",
      scales::number(abs(difference) / 1e9, accuracy = 0.1, big.mark = ","),
      "B"
    ),
    
    stop("Unknown change type: ", change_type)
  )
}


# ============================================================================
# Fixed print typography and pagination
# ============================================================================
#
# The infographic canvas uses a fixed CSS pixel density. Point sizes are
# converted from the shared publication typography in publication_spec.R:
#
#   pt / 72 pt per inch * css_dpi = CSS px
#
# These values never change with row count. Overflow is handled only by adding
# another 6.5 x 8.5 inch page.

infographic_pt_to_px <- function(pt) {
  pt * INFOGRAPHIC_SPEC$css_dpi / 72
}

# Data cells, column headers, and section headers share one reduced body size.
# This scale is global: it does NOT change by chapter, page, or row count.
# This fixed scale is shared by every chapter and does not change with row count.
INFOGRAPHIC_BODY_SCALE_PERCENT <- 94
INFOGRAPHIC_BODY_SCALE <- INFOGRAPHIC_BODY_SCALE_PERCENT / 100

INFOGRAPHIC_BODY_PT <- PUB$type$body_pt * INFOGRAPHIC_BODY_SCALE

INFOGRAPHIC_FIXED_LAYOUT <- list(
  data_font_px = infographic_pt_to_px(INFOGRAPHIC_BODY_PT),
  header_font_px = infographic_pt_to_px(INFOGRAPHIC_BODY_PT),
  chapter_font_px = infographic_pt_to_px(PUB$type$small_pt),
  title_font_px = infographic_pt_to_px(PUB$type$title_pt),
  subtitle_font_px = infographic_pt_to_px(PUB$type$subtitle_pt),
  panel_font_px = infographic_pt_to_px(INFOGRAPHIC_BODY_PT),
  group_font_px = infographic_pt_to_px(INFOGRAPHIC_BODY_PT),

  # Match the source/caption size used by figures and revised tables.
  source_font_px = infographic_pt_to_px(PUB$type$caption_pt),

  row_padding_px = infographic_pt_to_px(2.5)
)

# One uniform data-row height for every chapter/page.
INFOGRAPHIC_FIXED_LAYOUT$row_height_px <- ceiling(
  INFOGRAPHIC_FIXED_LAYOUT$data_font_px *
    PUB$type_metrics$infographic_row_lineheight +
    2 * INFOGRAPHIC_FIXED_LAYOUT$row_padding_px
)


# Fixed editorial pagination.
#
# The publication target is 30 BODY rows per page, with room reserved for up
# to five visible section headers. A section header counts as one rendered-row
# equivalent when pages are split.
#
# Therefore:
#   body-row cap             = 30
#   section-header allowance = 5
#   rendered-row budget      = 35
#
# A page with five section headers can contain all 30 body rows.
# A page with more than five section headers is broken earlier so the combined
# body-row + section-header count never exceeds 35. Pages are never resized and
# typography is never reduced to force additional content onto the canvas.
INFOGRAPHIC_ROWS_PER_PAGE <- 30L
INFOGRAPHIC_SECTION_HEADERS_PER_PAGE <- 5L
INFOGRAPHIC_RENDERED_ROWS_PER_PAGE <-
  INFOGRAPHIC_ROWS_PER_PAGE +
  INFOGRAPHIC_SECTION_HEADERS_PER_PAGE

# Retained as a compatibility alias. It is deliberately independent of n_rows.
layout_for_rows <- function(n_rows = NULL) {
  INFOGRAPHIC_FIXED_LAYOUT
}

font_sizes <- function(layout = INFOGRAPHIC_FIXED_LAYOUT, ...) {
  list(
    data = layout$data_font_px,
    header = layout$header_font_px,
    chapter = layout$chapter_font_px,
    title = layout$title_font_px,
    subtitle = layout$subtitle_font_px,
    panel = layout$panel_font_px,
    group = layout$group_font_px,
    source = layout$source_font_px
  )
}

# Convert punctuation only in text that is displayed in the infographic.
# Source-file column names, category names, and lookup keys remain unchanged.
smart_display_text <- function(x) {
  x <- as.character(x)
  
  # Paired straight double quotation marks.
  x <- gsub(
    '"([^"]+)"',
    '“\\1”',
    x,
    perl = TRUE
  )
  
  # Apostrophes within words, including possessives and contractions.
  x <- gsub(
    "(?<=\\p{L})'(?=\\p{L})",
    "’",
    x,
    perl = TRUE
  )
  
  # Paired straight single quotation marks used as quotations.
  x <- gsub(
    "(^|[[:space:]\\(\\[])'([^']+)'",
    "\\1‘\\2’",
    x,
    perl = TRUE
  )
  
  x
}

capitalize_source_terms <- function(x) {
  stringr::str_replace_all(
    x,
    c(
      "\\bfigure\\b"  = "Figure",
      "\\bfigures\\b" = "Figures",
      "\\btable\\b"   = "Table",
      "\\btables\\b"  = "Tables"
    )
  )
}


# Add a small directional indicator beside the displayed Change value.
#
# This is intentionally based on the formatted Change cell so every chapter
# receives the same treatment without requiring chapter-specific code.
#
#   ▴ green = numeric increase
#   ▾ red   = numeric decrease
#   blank   = no numeric direction / no comparison / no change
#
# Colors indicate direction only, not whether the change is normatively good
# or bad.
infographic_trend_arrow <- function(change) {
  x <- trimws(as.character(change))

  value <- suppressWarnings(
    readr::parse_number(x)
  )

  is_blank <- is.na(x) |
    !nzchar(x) |
    x %in% c("—", "–")

  is_multiple <- grepl(
    "×\\s*$",
    x,
    perl = TRUE
  )

  signed_up <- grepl(
    "^\\s*\\+",
    x,
    perl = TRUE
  ) &
    is.finite(value) &
    value > 0

  signed_down <- grepl(
    "^\\s*[-−]",
    x,
    perl = TRUE
  ) &
    is.finite(value) &
    value < 0

  multiple_up <- is_multiple &
    is.finite(value) &
    value > 1

  multiple_down <- is_multiple &
    is.finite(value) &
    value < 1

  out <- rep("", length(x))

  out[
    !is_blank &
      (signed_up | multiple_up)
  ] <- "\u25B2"  # ▲

  out[
    !is_blank &
      (signed_down | multiple_down)
  ] <- "\u25BC"  # ▼

  out
}

build_infographic_table <- function(
    data,
    id,
    chapter_number,
    title,
    subtitle = NULL,
    panel_label = NULL,
    column_labels = c(
      Earlier = "Earlier<br>value",
      Latest = "Latest or<br>Projected Value",
      Change = "Change"
    ),
    notes = character(),
    layout = INFOGRAPHIC_FIXED_LAYOUT,
    row_padding_px = INFOGRAPHIC_FIXED_LAYOUT$row_padding_px,
    nowrap_values = TRUE,
    top_border = FALSE
) {
  sizes <- font_sizes(layout)
  
  # Apply editorial changes only to rendered text. Do not alter source keys.
  data <- data %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::any_of(c("Section", "Measure", "Earlier", "Latest", "Change")),
        smart_display_text
      ),
      Trend = infographic_trend_arrow(.data$Change)
    )
  
  title <- smart_display_text(title)
  
  if (!is.null(subtitle)) {
    subtitle <- smart_display_text(subtitle)
  }
  
  notes <- notes %>%
    smart_display_text() %>%
    capitalize_source_terms()
  
  column_labels[["Latest"]] <- "Latest or<br>Projected Value"
  
  hidden_columns <- intersect(
    c(
      "StatID",
      "SectionID",
      "Emphasis",
      "RowOrder",
      "SnapshotOrder",
      "DisplayOrder",
      "PageOrder"
    ),
    names(data)
  )
  
  title_html <- if (is.null(chapter_number)) {
    paste0("<span class='infographic-panel-title'>", title, "</span>")
  } else {
    paste0(
      "<span class='infographic-kicker'>CHAPTER ", chapter_number, "</span>",
      "<br>",
      "<span class='infographic-main-title'>", title, "</span>"
    )
  }
  
  subtitle_html <- NULL
  if (!is.null(subtitle)) {
    subtitle_html <- paste0(
      "<span class='infographic-subtitle'>", subtitle, "</span>"
    )
  }
  
  tbl <- data %>%
    gt::gt(
      rowname_col = "Measure",
      groupname_col = "Section",
      id = id
    ) %>%
    gt::tab_stubhead(label = "Measure") %>%
    gt::cols_label(
      Earlier = gt::html(unname(column_labels[["Earlier"]])),
      Latest = gt::html(unname(column_labels[["Latest"]])),
      Change = gt::html(unname(column_labels[["Change"]])),
      Trend = gt::html("")
    ) %>%
    gt::cols_align(
      align = "center",
      columns = c(Earlier, Latest, Change, Trend)
    ) %>%
    gt::cols_hide(columns = tidyselect::any_of(hidden_columns)) %>%
    theme_gt_tpa() %>%
    gt::tab_header(
      title = gt::html(title_html),
      subtitle = if (is.null(subtitle_html)) NULL else gt::html(subtitle_html)
    )
  
  for (note in notes) {
    tbl <- gt::tab_source_note(tbl, source_note = gt::md(note))
  }
  
  tbl <- tbl %>%
    gt::tab_style(
      style = gt::cell_text(align = "left", weight = "normal"),
      locations = gt::cells_stub(rows = TRUE)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "normal"),
      locations = gt::cells_body(columns = tidyselect::everything())
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = INFOGRAPHIC_PALETTE$gray),
        gt::cell_text(color = "white", weight = "bold")
      ),
      locations = gt::cells_column_labels(tidyselect::everything())
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = "top",
        color = INFOGRAPHIC_PALETTE$red,
        weight = gt::px(3)
      ),
      locations = gt::cells_column_labels(tidyselect::everything())
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = INFOGRAPHIC_PALETTE$tint),
        gt::cell_text(
          color = INFOGRAPHIC_PALETTE$red,
          weight = "bold",
          transform = "uppercase"
        )
      ),
      locations = gt::cells_row_groups()
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = INFOGRAPHIC_PALETTE$pink),
        gt::cell_text(color = INFOGRAPHIC_PALETTE$red, weight = "bold")
      ),
      locations = gt::cells_body(columns = Latest)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(
        color = "#008a22",
        weight = "bold"
      ),
      locations = gt::cells_body(
        columns = Trend,
        rows = Trend == "▲"
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_text(
        color = "#8a0022",
        weight = "bold"
      ),
      locations = gt::cells_body(
        columns = Trend,
        rows = Trend == "▼"
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = INFOGRAPHIC_PALETTE$muted),
      locations = gt::cells_source_notes()
    ) %>%
    gt::tab_options(
      table.font.size = gt::px(sizes$data),
      table.font.color = INFOGRAPHIC_PALETTE$text,
      column_labels.font.size = gt::px(sizes$header),
      column_labels.padding = gt::px(infographic_pt_to_px(PUB$spacing$infographic_col_pad_pt)),
      data_row.padding = gt::px(row_padding_px),
      heading.title.font.size = gt::px(sizes$title),
      heading.subtitle.font.size = gt::px(sizes$subtitle),
      heading.padding = gt::px(infographic_pt_to_px(PUB$spacing$infographic_heading_pad_pt)),
      heading.align = "left",
      source_notes.font.size = gt::px(sizes$source),
      source_notes.padding = gt::px(infographic_pt_to_px(PUB$spacing$infographic_source_pad_pt)),
      table.background.color = "white",
      row.striping.background_color = "white",
      table_body.hlines.color = INFOGRAPHIC_PALETTE$rule,
      table_body.hlines.width = gt::px(1),
      row_group.border.top.color = INFOGRAPHIC_PALETTE$rule,
      row_group.border.bottom.color = INFOGRAPHIC_PALETTE$rule,
      table.width = gt::px(INFOGRAPHIC_SPEC$css_width_px),
      container.width = gt::px(INFOGRAPHIC_SPEC$css_width_px)
    ) %>%
    gt::opt_table_font(font = list("Gotham", gt::default_fonts())) %>%
    gt::opt_css(css = gotham_gt_css()) %>%
    fixed_table_width(
      profile = "ultrawide",
      width_px = INFOGRAPHIC_SPEC$css_width_px,
      stub_width_px = unname(INFOGRAPHIC_COLUMNS[["measure"]])
    ) %>%
    gt::cols_width(
      gt::stub() ~ gt::px(unname(INFOGRAPHIC_COLUMNS[["measure"]])),
      Earlier ~ gt::px(unname(INFOGRAPHIC_COLUMNS[["earlier"]])),
      Latest ~ gt::px(unname(INFOGRAPHIC_COLUMNS[["latest"]])),
      Change ~ gt::px(unname(INFOGRAPHIC_COLUMNS[["change"]])),
      Trend ~ gt::px(unname(INFOGRAPHIC_COLUMNS[["trend"]]))
    )
  
  css <- paste0(
    "#", id, " .infographic-kicker {",
    "color:", INFOGRAPHIC_PALETTE$red, ";",
    "font-size:", sizes$chapter, "px;",
    "font-weight:700;letter-spacing:",
    infographic_pt_to_px(PUB$type_metrics$infographic_kicker_letterspace_pt),
    "px;line-height:", PUB$type_metrics$infographic_kicker_lineheight, ";}",
    
    "#", id, " .infographic-main-title {",
    "color:", INFOGRAPHIC_PALETTE$text, ";",
    "font-size:", sizes$title, "px;",
    "font-weight:700;line-height:", PUB$type_metrics$infographic_title_lineheight, ";}",
    
    "#", id, " .infographic-subtitle {",
    "color:", INFOGRAPHIC_PALETTE$muted, ";",
    "font-size:", sizes$subtitle, "px;line-height:", PUB$type_metrics$infographic_subtitle_lineheight, ";}",
    
    "#", id, " .infographic-panel-label {",
    "color:", INFOGRAPHIC_PALETTE$red, ";",
    "font-size:", sizes$panel, "px;",
    "font-weight:700;letter-spacing:",
    infographic_pt_to_px(PUB$type_metrics$infographic_panel_label_letterspace_pt),
    "px;line-height:", PUB$type_metrics$infographic_panel_label_lineheight, ";}",
    
    "#", id, " .infographic-panel-title {",
    "color:", INFOGRAPHIC_PALETTE$text, ";",
    "font-size:", sizes$panel, "px;",
    "font-weight:700;letter-spacing:",
    infographic_pt_to_px(PUB$type_metrics$infographic_panel_title_letterspace_pt),
    "px;line-height:", PUB$type_metrics$infographic_panel_title_lineheight, ";}",
    
    "#", id, " .gt_heading {padding-left:",
    infographic_pt_to_px(PUB$spacing$infographic_side_pad_pt), "px !important;",
    "padding-right:", infographic_pt_to_px(PUB$spacing$infographic_side_pad_pt), "px !important;}",
    
    "#", id, " .gt_subtitle {font-size:", sizes$subtitle,
    "px !important;line-height:", PUB$type_metrics$infographic_subtitle_lineheight, " !important;}",
    
    "#", id, " .gt_group_heading {font-size:", sizes$group,
    "px !important;line-height:", PUB$type_metrics$infographic_group_lineheight, " !important;",
    "padding-top:", infographic_pt_to_px(PUB$spacing$infographic_group_pad_pt), "px !important;",
    "padding-bottom:", infographic_pt_to_px(PUB$spacing$infographic_group_pad_pt), "px !important;",
    "white-space:normal !important;}",
    
    "#", id, " .gt_row {line-height:", PUB$type_metrics$infographic_row_lineheight, " !important;}",
    "#", id, " .gt_stub, #", id, " .gt_rowname {",
    "padding-left:", infographic_pt_to_px(PUB$spacing$infographic_side_pad_pt), "px !important;",
    "padding-right:", infographic_pt_to_px(PUB$spacing$infographic_side_pad_pt), "px !important;",
    "font-weight:400 !important;}",
    "#", id, " tbody td {font-weight:400 !important;}",
    "#", id, " tbody td:nth-child(3) {font-weight:700 !important;}",
    "#", id, " td[data-column-id='Trend'] {",
    "padding-left:0 !important;padding-right:0 !important;",
    "text-align:center !important;",
    "font-size:", sizes$data * 0.82, "px !important;",
    "line-height:1 !important;}",
    "#", id, " .gt_source_notes {line-height:", PUB$type_metrics$infographic_source_lineheight, " !important;",
    "padding-left:", infographic_pt_to_px(PUB$spacing$infographic_side_pad_pt), "px !important;",
    "padding-right:", infographic_pt_to_px(PUB$spacing$infographic_side_pad_pt), "px !important;}",
    "#", id, " .gt_col_heading {line-height:", PUB$type_metrics$infographic_column_lineheight, " !important;}",
    if (nowrap_values) {
      paste0(
        "#", id,
        " tbody td, #", id, " .gt_stub, #", id, " .gt_rowname {white-space:nowrap !important;}"
      )
    } else {
      ""
    },
    if (top_border) {
      paste0(
        "#", id, " .gt_table {border-top:4px solid ",
        INFOGRAPHIC_PALETTE$red, " !important;}"
      )
    } else {
      ""
    }
  )
  
  gt::opt_css(tbl, css = css)
}

render_infographic_trial <- function(
    table,
    path,
    html_path = NULL
) {
  gt::gtsave(
    table,
    path,
    vwidth = INFOGRAPHIC_SPEC$css_width_px,
    vheight = INFOGRAPHIC_SPEC$css_height_px,
    zoom = INFOGRAPHIC_SPEC$zoom,
    expand = 0
  )
  
  if (!is.null(html_path)) {
    gt::gtsave(table, html_path)
  }
  
  info <- magick::image_info(magick::image_read(path))
  list(
    width_px = info$width[[1]],
    height_px = info$height[[1]]
  )
}

page_fits_infographic <- function(table, trial_path, tolerance_px = 4) {
  info <- render_infographic_trial(table, trial_path)
  info$width_px <= INFOGRAPHIC_SPEC$output_width_px + tolerance_px &&
    info$height_px <= INFOGRAPHIC_SPEC$output_height_px + tolerance_px
}

paginate_infographic_data <- function(
    data,
    build_page = NULL,
    name = NULL,
    output_dir = PATHS$final_infographics,
    rows_per_page = INFOGRAPHIC_ROWS_PER_PAGE,
    section_headers_per_page = INFOGRAPHIC_SECTION_HEADERS_PER_PAGE
) {
  if (nrow(data) < 1) {
    stop("The infographic contains no data rows.")
  }

  rows_per_page <- as.integer(rows_per_page)
  section_headers_per_page <- as.integer(section_headers_per_page)

  if (
    length(rows_per_page) != 1L ||
    !is.finite(rows_per_page) ||
    rows_per_page < 1L
  ) {
    stop("`rows_per_page` must be one positive integer.")
  }

  if (
    length(section_headers_per_page) != 1L ||
    !is.finite(section_headers_per_page) ||
    section_headers_per_page < 0L
  ) {
    stop("`section_headers_per_page` must be one nonnegative integer.")
  }

  if (!"Section" %in% names(data)) {
    stop(
      "Infographic pagination requires a `Section` column so section headers ",
      "can be counted in the fixed page-height budget."
    )
  }

  rendered_row_budget <- rows_per_page + section_headers_per_page

  # Greedy pagination in publication order.
  #
  # Cost model:
  #   each body/stat row        = 1 unit
  #   each visible section head = 1 unit
  #
  # The first row on every page necessarily creates a section header. A new
  # section within a page creates one additional header. If a section continues
  # onto the next page, mark_infographic_continuation() still renders a
  # "(cont.)" header there, so the first-header cost on each page is correct.
  pages <- list()
  page_start <- 1L
  body_rows_on_page <- 0L
  rendered_units_on_page <- 0L
  previous_section_on_page <- NULL

  for (i in seq_len(nrow(data))) {
    current_section <- as.character(data$Section[[i]])

    starts_new_section <- (
      body_rows_on_page == 0L ||
      is.null(previous_section_on_page) ||
      !identical(current_section, previous_section_on_page)
    )

    body_cost <- 1L
    header_cost <- if (starts_new_section) 1L else 0L

    would_exceed_body_cap <- (
      body_rows_on_page + body_cost >
        rows_per_page
    )

    would_exceed_rendered_budget <- (
      rendered_units_on_page +
        body_cost +
        header_cost >
        rendered_row_budget
    )

    if (
      body_rows_on_page > 0L &&
      (
        would_exceed_body_cap ||
        would_exceed_rendered_budget
      )
    ) {
      pages[[length(pages) + 1L]] <- data[
        page_start:(i - 1L),
        ,
        drop = FALSE
      ]

      page_start <- i
      body_rows_on_page <- 0L
      rendered_units_on_page <- 0L
      previous_section_on_page <- NULL

      starts_new_section <- TRUE
      header_cost <- 1L
    }

    body_rows_on_page <- body_rows_on_page + body_cost
    rendered_units_on_page <-
      rendered_units_on_page +
      body_cost +
      header_cost

    previous_section_on_page <- current_section
  }

  pages[[length(pages) + 1L]] <- data[
    page_start:nrow(data),
    ,
    drop = FALSE
  ]

  pages
}


mark_infographic_continuation <- function(
    page_data,
    previous_page_data = NULL
) {
  page_data <- page_data %>%
    dplyr::mutate(
      Section = as.character(.data$Section)
    )

  if (
    is.null(previous_page_data) ||
    nrow(previous_page_data) == 0L ||
    nrow(page_data) == 0L
  ) {
    return(page_data)
  }

  previous_section <- as.character(
    previous_page_data$Section[[nrow(previous_page_data)]]
  )

  first_section <- as.character(
    page_data$Section[[1]]
  )

  if (
    !is.na(previous_section) &&
    !is.na(first_section) &&
    identical(previous_section, first_section)
  ) {
    continued_label <- paste0(first_section, " (cont.)")

    page_data$Section[
      page_data$Section == first_section
    ] <- continued_label
  }

  page_data
}


export_infographic_page <- function(
    table,
    name,
    output_dir = PATHS$final_infographics
) {
  html_dir <- file.path(output_dir, "html")
  png_dir <- file.path(output_dir, "pngs")
  dir.create(html_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(png_dir, recursive = TRUE, showWarnings = FALSE)
  
  html_path <- file.path(html_dir, paste0(name, ".html"))
  raw_path <- file.path(png_dir, paste0(name, "_raw.png"))
  png_path <- file.path(png_dir, paste0(name, ".png"))
  
  info <- render_infographic_trial(table, raw_path, html_path)
  
  if (info$width_px > INFOGRAPHIC_SPEC$output_width_px + 4) {
    stop(
      "Rendered page width is ", info$width_px,
      " pixels; expected no more than ", INFOGRAPHIC_SPEC$output_width_px,
      ". The page was not resized."
    )
  }
  if (info$height_px > INFOGRAPHIC_SPEC$output_height_px + 4) {
    stop(
      "Rendered page height is ", info$height_px,
      " pixels; expected no more than ", INFOGRAPHIC_SPEC$output_height_px,
      ". The page was not shrunk."
    )
  }
  
  image <- magick::image_read(raw_path)
  image <- magick::image_extent(
    image,
    geometry = paste0(
      INFOGRAPHIC_SPEC$output_width_px,
      "x",
      INFOGRAPHIC_SPEC$output_height_px
    ),
    gravity = "northwest",
    color = "white"
  )
  
  magick::image_write(
    image,
    path = png_path,
    format = "png",
    density = paste0(INFOGRAPHIC_SPEC$dpi, "x", INFOGRAPHIC_SPEC$dpi)
  )
  
  if (file.exists(raw_path)) unlink(raw_path)
  
  final_info <- magick::image_info(magick::image_read(png_path))
  if (
    final_info$width[[1]] != INFOGRAPHIC_SPEC$output_width_px ||
    final_info$height[[1]] != INFOGRAPHIC_SPEC$output_height_px
  ) {
    stop("The final infographic page does not have the required dimensions.")
  }
  
  list(
    table = table,
    html_path = html_path,
    png_path = png_path,
    width_px = final_info$width[[1]],
    height_px = final_info$height[[1]]
  )
}

export_paginated_infographic <- function(
    data,
    build_page,
    name,
    output_dir = PATHS$final_infographics,
    rows_per_page = INFOGRAPHIC_ROWS_PER_PAGE,
    section_headers_per_page = INFOGRAPHIC_SECTION_HEADERS_PER_PAGE
) {
  pages <- paginate_infographic_data(
    data = data,
    build_page = build_page,
    name = name,
    output_dir = output_dir,
    rows_per_page = rows_per_page,
    section_headers_per_page = section_headers_per_page
  )

  total_pages <- length(pages)

  exports <- lapply(seq_along(pages), function(page_number) {
    page_name <- if (total_pages == 1L) {
      name
    } else {
      paste0(name, "_page_", sprintf("%02d", page_number))
    }

    previous_page <- if (page_number == 1L) {
      NULL
    } else {
      pages[[page_number - 1L]]
    }

    display_data <- mark_infographic_continuation(
      page_data = pages[[page_number]],
      previous_page_data = previous_page
    )

    table <- build_page(
      page_data = display_data,
      page_number = page_number,
      total_pages = total_pages,
      is_last_page = page_number == total_pages
    )

    export_infographic_page(
      table = table,
      name = page_name,
      output_dir = output_dir
    )
  })

  page_body_counts <- vapply(
    pages,
    nrow,
    integer(1)
  )

  page_header_counts <- vapply(
    pages,
    function(x) {
      if (nrow(x) == 0L) {
        return(0L)
      }

      section <- as.character(x$Section)

      sum(
        c(
          TRUE,
          section[-1] != section[-length(section)]
        ),
        na.rm = TRUE
      )
    },
    integer(1)
  )

  message(
    "Saved ", total_pages,
    " fixed-format page", if (total_pages == 1L) "" else "s",
    " for ", name, ". ",
    "Body-row cap = ", rows_per_page,
    "; section-header allowance = ", section_headers_per_page,
    "; rendered-row budget = ",
    rows_per_page + section_headers_per_page,
    ". Page body/header counts: ",
    paste0(
      page_body_counts,
      "+",
      page_header_counts,
      collapse = ", "
    ),
    ". Type size, row height, columns, and PNG dimensions remain fixed."
  )

  invisible(exports)
}

