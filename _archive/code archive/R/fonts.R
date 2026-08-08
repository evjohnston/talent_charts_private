# fonts.R
#
# Single source of truth for Gotham across report outputs.
#
# Charts:
#   - ggplot + ragg use systemfonts/textshaping.
#   - Local Gotham OTF files are registered directly with systemfonts.
#   - showtext is deliberately NOT used for chart rendering because mixing
#     showtext with ragg/ggtext can produce incorrect word/letter spacing.
#
# gt tables:
#   - Gotham is base64-embedded with @font-face so Chromium/webshot can render
#     it without requiring a system-wide font installation.
#
# IMPORTANT:
#   Source this file BEFORE theme.R.
#   theme.R should not call showtext_auto() or showtext_opts().
#
# Font files live in fonts/, relative to the knit working directory.

FONT_DIR        <- "fonts"
FONT_FAMILY     <- "gotham"  # family name used by ggplot/ragg
FONT_FAMILY_CSS <- "Gotham"  # family name used in gt/CSS


# =============================================================================
# Gotham face inventory
# =============================================================================

# Gotham has no face named "Regular"; Book is the normal/body face.
GOTHAM_FILES <- c(
  regular    = "Gotham-Book.otf",
  bold       = "Gotham-Bold.otf",
  italic     = "Gotham-BookItalic.otf",
  bolditalic = "Gotham-BoldItalic.otf"
)

# Kept as a list because the gt CSS helper needs weight/style metadata.
GOTHAM_FACES <- list(
  list(file = GOTHAM_FILES[["regular"]],    weight = "normal", style = "normal"),
  list(file = GOTHAM_FILES[["bold"]],       weight = "bold",   style = "normal"),
  list(file = GOTHAM_FILES[["italic"]],     weight = "normal", style = "italic"),
  list(file = GOTHAM_FILES[["bolditalic"]], weight = "bold",   style = "italic")
)


# =============================================================================
# Shared path helpers
# =============================================================================

gotham_font_paths <- function(dir = FONT_DIR) {
  paths <- file.path(dir, unname(GOTHAM_FILES))
  names(paths) <- names(GOTHAM_FILES)
  paths
}

check_gotham_files <- function(dir = FONT_DIR, warn = TRUE) {
  paths <- gotham_font_paths(dir)
  missing <- paths[!file.exists(paths)]
  
  if (length(missing) > 0) {
    msg <- paste0(
      "Gotham font files not found: ",
      paste(basename(missing), collapse = ", "),
      ". Expected them under: ",
      normalizePath(dir, winslash = "/", mustWork = FALSE)
    )
    
    if (warn) warning(msg, call. = FALSE)
    return(FALSE)
  }
  
  TRUE
}


# =============================================================================
# 1. Register Gotham for ggplot + ragg
# =============================================================================

# ragg resolves fonts through systemfonts. Registering the four faces here gives
# ggplot a stable family name ("gotham") while preserving bold/italic matching.
register_gotham_systemfonts <- function(
    dir = FONT_DIR,
    family = FONT_FAMILY
) {
  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    warning(
      "Package 'systemfonts' is not installed; Gotham was not registered ",
      "for ggplot/ragg rendering.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  
  if (!check_gotham_files(dir = dir, warn = TRUE)) {
    return(invisible(FALSE))
  }
  
  paths <- gotham_font_paths(dir)
  
  systemfonts::register_font(
    name       = family,
    plain      = unname(paths[["regular"]]),
    bold       = unname(paths[["bold"]]),
    italic     = unname(paths[["italic"]]),
    bolditalic = unname(paths[["bolditalic"]])
  )
  
  invisible(TRUE)
}


# Optional compatibility helper for old code that explicitly uses showtext.
# Do not call this for normal report charts.
register_gotham_showtext <- function(
    dir = FONT_DIR,
    family = FONT_FAMILY
) {
  if (!requireNamespace("sysfonts", quietly = TRUE)) {
    warning(
      "Package 'sysfonts' is not installed; Gotham was not registered ",
      "for legacy showtext rendering.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  
  if (!check_gotham_files(dir = dir, warn = TRUE)) {
    return(invisible(FALSE))
  }
  
  paths <- gotham_font_paths(dir)
  
  sysfonts::font_add(
    family     = family,
    regular    = unname(paths[["regular"]]),
    bold       = unname(paths[["bold"]]),
    italic     = unname(paths[["italic"]]),
    bolditalic = unname(paths[["bolditalic"]])
  )
  
  invisible(TRUE)
}


# =============================================================================
# 2. Base64 @font-face CSS for gt tables
# =============================================================================

# Embeds each OTF directly into the HTML used by gt/webshot. This is separate
# from ragg's systemfonts registration because gt is rendered in a browser.
gotham_font_face_css <- function(
    dir = FONT_DIR,
    faces = GOTHAM_FACES,
    family = FONT_FAMILY_CSS
) {
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    warning(
      "Package 'base64enc' is not installed; Gotham could not be embedded ",
      "in gt output.",
      call. = FALSE
    )
    return("")
  }
  
  if (!check_gotham_files(dir = dir, warn = TRUE)) {
    return("")
  }
  
  blocks <- vapply(
    faces,
    function(f) {
      path <- file.path(dir, f$file)
      b64  <- base64enc::base64encode(path)
      
      sprintf(
        paste0(
          "@font-face { ",
          "font-family: '%s'; ",
          "font-style: %s; ",
          "font-weight: %s; ",
          "font-display: block; ",
          "src: url(data:font/otf;base64,%s) format('opentype'); ",
          "}"
        ),
        family,
        f$style,
        f$weight,
        b64
      )
    },
    character(1)
  )
  
  paste(blocks[nzchar(blocks)], collapse = "\n")
}


# =============================================================================
# 3. CSS helper used by tables.R / infographics.R
# =============================================================================

# Applies Gotham to every element of a gt table, overriding fonts introduced by
# upstream gt themes. Pass an id to scope the CSS to one table; otherwise it
# applies to every .gt_table in the rendered page.
gotham_gt_css <- function(
    id = NULL,
    dir = FONT_DIR,
    family = FONT_FAMILY_CSS
) {
  face <- gotham_font_face_css(
    dir = dir,
    family = family
  )
  
  if (!nzchar(face)) return("")
  
  sel <- if (is.null(id)) {
    ".gt_table"
  } else {
    sprintf("#%s .gt_table", id)
  }
  
  paste0(
    face,
    "\n",
    sel,
    ", ",
    sel,
    " * { font-family: '",
    family,
    "', sans-serif !important; }"
  )
}


# =============================================================================
# 4. Diagnostics
# =============================================================================

# Run this manually if a chart appears to fall back to another font.
check_gotham_registration <- function(family = FONT_FAMILY) {
  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    stop("Package 'systemfonts' is required.", call. = FALSE)
  }
  
  styles <- data.frame(
    face   = c("regular", "bold", "italic", "bolditalic"),
    italic = c(FALSE, FALSE, TRUE, TRUE),
    weight = c("normal", "bold", "normal", "bold"),
    stringsAsFactors = FALSE
  )
  
  matches <- lapply(
    seq_len(nrow(styles)),
    function(i) {
      hit <- systemfonts::match_fonts(
        family = family,
        italic = styles$italic[i],
        weight = styles$weight[i]
      )
      
      data.frame(
        face = styles$face[i],
        path = hit$path[1],
        stringsAsFactors = FALSE
      )
    }
  )
  
  do.call(rbind, matches)
}


# =============================================================================
# Register immediately when sourced
# =============================================================================

register_gotham_systemfonts()

# If showtext was enabled earlier in the R session, turn it off so ragg remains
# the sole chart text renderer. theme.R must not turn it back on afterward.
if (requireNamespace("showtext", quietly = TRUE)) {
  showtext::showtext_auto(FALSE)
}