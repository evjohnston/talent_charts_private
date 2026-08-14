# setup_publication.R
# Shared setup for figures.Rmd, tables.Rmd, and infographics.Rmd.
# showtext is intentionally not attached; Gotham chart text is rendered by ragg
# through systemfonts/textshaping (see fonts.R).

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(ggtext)
  library(ggrepel)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(gt)
  library(gtExtras)
  library(webshot2)
  library(magick)
  library(dplyr)
  library(tidyr)
})

# ============================================================================
# Resolve the project root independently of RStudio / knitr working directory
# ============================================================================

find_setup_file <- function() {
  # When sourced with source(), sys.frame(1)$ofile normally contains this file.
  candidate <- tryCatch(
    sys.frame(1)$ofile,
    error = function(e) NULL
  )

  if (!is.null(candidate) && nzchar(candidate)) {
    return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  }

  NULL
}

setup_file <- find_setup_file()

if (!is.null(setup_file)) {
  # setup_publication.R lives at <project>/inputs/scripts/setup_publication.R
  PROJECT_ROOT <- normalizePath(
    file.path(dirname(setup_file), "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
} else {
  # Fallback for unusual interactive sourcing contexts.
  current <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

  repeat {
    if (length(list.files(current, pattern = "\\.Rproj$")) > 0) {
      PROJECT_ROOT <- current
      break
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

source(
  file.path(
    PROJECT_ROOT,
    "inputs",
    "scripts",
    "publication_spec.R"
  )
)

check_publication_paths()
create_publication_output_dirs()

for (script in c(
  "fonts.R",
  "theme.R",
  "helpers.R",
  "align_gutters.R",
  "tables.R",
  "chart_layers.R",
  "maps.R",
  "infographics.R"
)) {
  source(file.path(PATHS$scripts, script))
}
