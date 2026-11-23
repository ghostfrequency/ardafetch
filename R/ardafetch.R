# Internal package environment to hold loaded catalog in memory
.arda_env <- new.env(parent = emptyenv())

# Default remote URL for the master file-level crosswalk
.DEFAULT_CATALOG_URL <- "https://raw.githubusercontent.com/ghostfrequency/ardafetch/master/inst/extdata/ARDA_OSF_file_level_crosswalk.csv"

`%||%` <- function(x, y) if (is.null(x)) y else x

# Internal helper to validate required columns in a file-level crosswalk
validate_file_catalog <- function(cat_df) {
  required_cols <- c(
    "File_GUID", "Download_URL", "Original_Filename",
    "Standardized_Filename", "ARDA_ID", "Title"
  )
  missing_cols <- setdiff(required_cols, names(cat_df))
  if (length(missing_cols) > 0) {
    stop(
      "Catalog CSV is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(cat_df)
}

# Internal helper to standardize column names to deterministic lowercase snake_case
clean_df_names <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0) {
    return(df)
  }
  
  nms <- names(df)
  # Transliterate accented characters to ASCII when possible
  nms_ascii <- suppressWarnings(iconv(nms, to = "ASCII//TRANSLIT"))
  nms <- ifelse(is.na(nms_ascii), nms, nms_ascii)
  # Split camelCase / PascalCase boundaries (e.g., "ReligTrad" -> "Relig_Trad")
  nms <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", nms)
  nms <- tolower(nms)
  # Replace any non-alphanumeric characters with underscores
  nms <- gsub("[^a-z0-9]+", "_", nms)
  # Trim leading/trailing underscores
  nms <- gsub("^_+|_+$", "", nms)
  # Prefix names that are empty or start with a digit so they are valid R identifiers
  nms[!nzchar(nms)] <- "x"
  nms <- ifelse(grepl("^[0-9]", nms), paste0("x_", nms), nms)
  # Ensure uniqueness (e.g., "var", "var_2")
  names(df) <- make.unique(nms, sep = "_")
  df
}

#' Get or Set the Local Cache Directory for ARDA Datasets
#'
#' @param path Optional character path to override the default cache directory.
#' @return Character string path to the cache directory.
#' @export
arda_cache_dir <- function(path = NULL) {
  if (!is.null(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    .arda_env$cache_dir <- normalizePath(path, mustWork = FALSE)
    return(.arda_env$cache_dir)
  }
  if (!is.null(.arda_env$cache_dir)) {
    return(.arda_env$cache_dir)
  }
  default_dir <- tools::R_user_dir("ardafetch", which = "cache")
  dir.create(default_dir, recursive = TRUE, showWarnings = FALSE)
  .arda_env$cache_dir <- default_dir
  default_dir
}

#' Update the Local ARDA Catalog from the Master Repository
#'
#' Downloads the latest `ARDA_OSF_file_level_crosswalk.csv` from the master remote
#' URL into `arda_cache_dir()`, overriding the bundled package catalog for future
#' sessions.
#'
#' @param url Character URL to the remote master crosswalk CSV. Defaults to the
#'   official ARDA-OSF master crosswalk URL.
#' @return Invisibly returns the updated catalog tibble.
#' @export
arda_update_catalog <- function(url = .DEFAULT_CATALOG_URL) {
  dest <- file.path(arda_cache_dir(), "ARDA_OSF_file_level_crosswalk.csv")
  tmp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_file), add = TRUE)
  
  message("Downloading latest ARDA dataset catalog...")
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = "ardafetch R package") |>
    httr2::req_retry(max_tries = 3, backoff = ~ 2^.x)
  
  httr2::req_perform(req, path = tmp_file)
  
  cat_df <- readr::read_csv(tmp_file, show_col_types = FALSE)
  validate_file_catalog(cat_df)
  
  file.copy(tmp_file, dest, overwrite = TRUE)
  .arda_env$file_catalog <- cat_df
  
  message(sprintf("Catalog updated (%d files indexed) and saved to cache.", nrow(cat_df)))
  invisible(cat_df)
}

#' Register a Local File-Level Crosswalk Catalog
#'
#' Manually registers a local `ARDA_OSF_file_level_crosswalk.csv` file, overriding
#' the bundled catalog. Useful for testing unreleased ARDA datasets locally.
#'
#' @param csv_path Path to `ARDA_OSF_file_level_crosswalk.csv`.
#' @param copy_to_cache Logical; if `TRUE` (default), caches a copy in `arda_cache_dir()`.
#' @return Invisibly returns the loaded catalog tibble.
#' @export
arda_set_catalog <- function(csv_path, copy_to_cache = TRUE) {
  if (!file.exists(csv_path)) {
    stop("Catalog file not found: ", csv_path, call. = FALSE)
  }
  cat_df <- readr::read_csv(csv_path, show_col_types = FALSE)
  validate_file_catalog(cat_df)
  
  if (isTRUE(copy_to_cache)) {
    dest <- file.path(arda_cache_dir(), "ARDA_OSF_file_level_crosswalk.csv")
    file.copy(csv_path, dest, overwrite = TRUE)
  }
  .arda_env$file_catalog <- cat_df
  invisible(cat_df)
}

# Internal helper to locate and load the file-level catalog
# Priority: 1. In-memory session cache -> 2. Updated user cache -> 3. Bundled inst/extdata
get_file_catalog <- function() {
  if (!is.null(.arda_env$file_catalog)) {
    return(.arda_env$file_catalog)
  }
  
  candidates <- c(
    file.path(arda_cache_dir(), "ARDA_OSF_file_level_crosswalk.csv"),
    system.file("extdata", "ARDA_OSF_file_level_crosswalk.csv", package = "ardafetch")
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  
  if (length(candidates) == 0) {
    stop(
      "File-level catalog (`ARDA_OSF_file_level_crosswalk.csv`) not found in cache or package installation.\n",
      "Run `arda_update_catalog()` to download it or `arda_set_catalog('path/to/file.csv')` to register a local copy.",
      call. = FALSE
    )
  }
  
  cat_df <- readr::read_csv(candidates[1], show_col_types = FALSE)
  validate_file_catalog(cat_df)
  .arda_env$file_catalog <- cat_df
  .arda_env$file_catalog
}

# Internal helper to load project-level MARC metadata (keywords, authors, abstracts)
get_project_catalog <- function() {
  if (!is.null(.arda_env$project_catalog)) {
    return(.arda_env$project_catalog)
  }
  
  candidates <- c(
    file.path(arda_cache_dir(), "ARDA_OSF_crosswalk.csv"),
    system.file("extdata", "ARDA_OSF_crosswalk.csv", package = "ardafetch")
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  
  if (length(candidates) > 0) {
    .arda_env$project_catalog <- readr::read_csv(candidates[1], show_col_types = FALSE)
    return(.arda_env$project_catalog)
  }
  NULL
}

#' Search the ARDA Dataset Catalog
#'
#' Search datasets by title, ARDA ID, keywords, investigator, or publication year.
#'
#' @param query Optional search string (case-insensitive regex) matched against Title,
#'   ARDA_ID, Keywords, and Authors_Investigators.
#' @param year Optional integer or integer vector of publication years to filter by.
#' @param max_results Maximum number of matching datasets to return (default 25).
#' @return A tibble of matching ARDA datasets.
#' @export
arda_search <- function(query = NULL, year = NULL, max_results = 25) {
  proj <- get_project_catalog()
  if (is.null(proj)) {
    # Fallback to unique projects from file-level catalog
    proj <- get_file_catalog() |>
      dplyr::distinct(.data$ARDA_ID, .data$OSF_Project_ID, .data$Title, .data$Publication_Year)
  }
  
  res <- proj
  if (!is.null(year)) {
    res <- dplyr::filter(res, .data$Publication_Year %in% year)
  }
  if (!is.null(query) && nzchar(query)) {
    rx <- stringr::regex(query, ignore_case = TRUE)
    has_kw <- "Keywords" %in% names(res)
    has_au <- "Authors_Investigators" %in% names(res)
    res <- dplyr::filter(
      res,
      stringr::str_detect(.data$Title, rx) |
        stringr::str_detect(.data$ARDA_ID, rx) |
        (if (has_kw) stringr::str_detect(dplyr::coalesce(.data$Keywords, ""), rx) else FALSE) |
        (if (has_au) stringr::str_detect(dplyr::coalesce(.data$Authors_Investigators, ""), rx) else FALSE)
    )
  }
  
  cols <- intersect(
    c("ARDA_ID", "Title", "Publication_Year", "Authors_Investigators", "Keywords", "OSF_ID", "ARDA_URL"),
    names(res)
  )
  res |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::slice_head(n = max_results)
}

#' List Available Files for an ARDA Dataset
#'
#' @param arda_id Character ARDA dataset ID (e.g., `"1979ABC"` or `"1890CENSCT"`).
#' @param ext Optional file extension filter (e.g., `"sav"`, `"dta"`, `"por"`, `"pdf"`).
#' @return A tibble of files available for that dataset on OSF.
#' @export
arda_files <- function(arda_id, ext = NULL) {
  cat_df <- get_file_catalog()
  res <- cat_df |>
    dplyr::filter(toupper(.data$ARDA_ID) == toupper(arda_id)) |>
    dplyr::mutate(Extension = tolower(tools::file_ext(.data$Original_Filename)))
  
  if (!is.null(ext) && nzchar(ext)) {
    clean_ext <- tolower(sub("^\\.", "", ext))
    res <- dplyr::filter(res, .data$Extension == clean_ext)
  }
  
  res |>
    dplyr::select(
      "ARDA_ID", "File_GUID", "Original_Filename", "Standardized_Filename",
      "Extension", "File_Size_Bytes", "Download_URL", "Title"
    )
}

#' Download and Open an ARDA Dataset File on Request
#'
#' Looks up a dataset by `arda_id` (or `file_guid`), downloads the requested file
#' from OSF into a local cache (skipping re-download if already cached), and reads
#' it into R as a tibble/data.frame.
#'
#' @param arda_id Character ARDA dataset ID (e.g., `"1979ABC"`). Optional if `file_guid` is supplied.
#' @param format Preferred file format when multiple files exist for a dataset.
#'   Defaults to checking `"sav"`, `"dta"`, `"por"`, `"rds"`, `"csv"`, `"tsv"`, `"xlsx"` in order.
#' @param file_guid Optional specific 5-character OSF file GUID (e.g., `"8nc9b"`).
#' @param filename_pattern Optional regex to select a specific file when a dataset has multiple
#'   files of the same format (e.g., `"County"` or `"Wave1"`).
#' @param clean_names Logical; if `TRUE`, converts all variable names to lowercase
#'   `snake_case` while preserving `haven` variable and value labels. Defaults to `FALSE`.
#' @param as_factor Logical; if `TRUE`, converts `haven_labelled` variables (from `.sav`,
#'   `.por`, or `.dta` files) into standard R factors using `haven::as_factor()`.
#'   Defaults to `FALSE`.
#' @param cache_dir Directory to store downloaded files. Defaults to `arda_cache_dir()`.
#' @param refresh Logical; if `TRUE`, forces a fresh download even if cached.
#' @param download_only Logical; if `TRUE`, downloads the file and returns the local file path
#'   without reading it into R (useful for PDF codebooks or raw text files).
#' @return A data frame / tibble (or character file path if `download_only = TRUE` or non-tabular).
#' @export
arda_load <- function(arda_id = NULL,
                      format = c("sav", "dta", "por", "rds", "csv", "tsv", "xlsx"),
                      file_guid = NULL,
                      filename_pattern = NULL,
                      clean_names = FALSE,
                      as_factor = FALSE,
                      cache_dir = arda_cache_dir(),
                      refresh = FALSE,
                      download_only = FALSE) {
  cat_df <- get_file_catalog() |>
    dplyr::mutate(Extension = tolower(tools::file_ext(.data$Original_Filename)))
  
  if (!is.null(file_guid)) {
    matches <- dplyr::filter(cat_df, tolower(.data$File_GUID) == tolower(file_guid))
  } else if (!is.null(arda_id)) {
    matches <- dplyr::filter(cat_df, toupper(.data$ARDA_ID) == toupper(arda_id))
    if (!is.null(filename_pattern) && nzchar(filename_pattern)) {
      matches <- dplyr::filter(
        matches,
        stringr::str_detect(.data$Original_Filename, stringr::regex(filename_pattern, ignore_case = TRUE))
      )
    }
    # Filter to requested format priority
    fmt_matches <- dplyr::filter(matches, .data$Extension %in% tolower(format))
    if (nrow(fmt_matches) > 0) {
      fmt_matches <- fmt_matches |>
        dplyr::mutate(.fmt_rank = match(.data$Extension, tolower(format))) |>
        dplyr::arrange(.data$.fmt_rank)
      matches <- fmt_matches
    }
  } else {
    stop("Supply either `arda_id` or `file_guid`.", call. = FALSE)
  }
  
  if (nrow(matches) == 0) {
    stop("No matching file found in the ARDA-OSF catalog.", call. = FALSE)
  }
  
  target <- matches[1, ]
  if (nrow(matches) > 1 && is.null(file_guid)) {
    message(sprintf(
      "Multiple files matched (%d); selecting '%s' (%s). Use `arda_files('%s')` to see all options.",
      nrow(matches), target$Original_Filename, target$File_GUID, target$ARDA_ID
    ))
  }
  
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  dest_path <- file.path(cache_dir, target$Standardized_Filename)
  
  if (!file.exists(dest_path) || isTRUE(refresh)) {
    message(sprintf("Downloading %s (%s) from OSF...", target$Standardized_Filename, target$File_GUID))
    req <- httr2::request(target$Download_URL) |>
      httr2::req_headers(`User-Agent` = "ardafetch R package") |>
      httr2::req_retry(max_tries = 3, backoff = ~ 2^.x)
    httr2::req_perform(req, path = dest_path)
  } else {
    message(sprintf("Loading cached file: %s", dest_path))
  }
  
  if (isTRUE(download_only)) {
    return(dest_path)
  }
  
  ext <- target$Extension
  df <- switch(
    ext,
    "sav"  = haven::read_sav(dest_path),
    "zsav" = haven::read_sav(dest_path),
    "por"  = haven::read_por(dest_path),
    "dta"  = haven::read_dta(dest_path),
    "rds"  = readRDS(dest_path),
    "csv"  = readr::read_csv(dest_path, show_col_types = FALSE),
    "tsv"  = readr::read_tsv(dest_path, show_col_types = FALSE),
    "xlsx" = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Install the `readxl` package to open .xlsx files.", call. = FALSE)
      }
      readxl::read_excel(dest_path)
    },
    "xls"  = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Install the `readxl` package to open .xls files.", call. = FALSE)
      }
      readxl::read_excel(dest_path)
    },
    {
      message(sprintf("Format '.%s' is non-tabular; returning local file path.", ext))
      return(dest_path)
    }
  )
  
  if (is.data.frame(df)) {
    if (isTRUE(as_factor)) {
      df <- haven::as_factor(df)
    }
    if (isTRUE(clean_names)) {
      df <- clean_df_names(df)
    }
  }
  
  df
}

#' Extract a Searchable Codebook from a Loaded ARDA Dataset
#'
#' Inspects a data frame or tibble loaded via `arda_load()` and extracts variable
#' names, storage types, question text (`label` attributes), value labels (or factor
#' levels), and missing value counts/percentages.
#'
#' @param df A data frame or tibble returned by `arda_load()`.
#' @param query Optional search string (case-insensitive regex) to filter variables
#'   by variable name, question text (`label`), or value labels.
#' @param max_values Maximum number of value labels/levels to display per variable
#'   before truncating with `"..."` (default `15`). Set to `Inf` to show all.
#' @return A tibble with columns `pos`, `variable`, `type`, `label`, `value_labels`,
#'   `n_unique`, `n_missing`, and `pct_missing`.
#' @export
arda_codebook <- function(df, query = NULL, max_values = 15) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame or tibble.", call. = FALSE)
  }
  
  n_rows <- nrow(df)
  vars <- names(df)
  
  rows <- lapply(seq_along(df), function(i) {
    col <- df[[i]]
    var_name <- vars[i]
    
    # 1. Question text / variable label
    var_label <- attr(col, "label", exact = TRUE)
    if (is.null(var_label) || length(var_label) == 0 || !nzchar(as.character(var_label[1]))) {
      var_label <- NA_character_
    } else {
      var_label <- as.character(var_label[1])
    }
    
    # 2. Variable class/type
    col_type <- if (inherits(col, "haven_labelled")) {
      paste0("labelled (", typeof(col), ")")
    } else if (is.factor(col)) {
      if (is.ordered(col)) "ordered factor" else "factor"
    } else {
      class(col)[1]
    }
    
    # 3. Value labels (haven_labelled) or factor levels
    val_labs <- attr(col, "labels", exact = TRUE)
    if (!is.null(val_labs) && length(val_labs) > 0) {
      lab_names <- names(val_labs) %||% rep("", length(val_labs))
      formatted <- ifelse(
        nzchar(lab_names),
        paste0(unname(val_labs), " = ", lab_names),
        as.character(unname(val_labs))
      )
      if (length(formatted) > max_values) {
        formatted <- c(formatted[seq_len(max_values)], sprintf("... (+%d more)", length(formatted) - max_values))
      }
      val_str <- paste(formatted, collapse = "; ")
    } else if (is.factor(col)) {
      lvls <- levels(col)
      if (length(lvls) > max_values) {
        lvls <- c(lvls[seq_len(max_values)], sprintf("... (+%d more)", length(lvls) - max_values))
      }
      val_str <- paste(lvls, collapse = "; ")
    } else {
      val_str <- NA_character_
    }
    
    # 4. Missingness & unique non-missing values
    is_miss <- is.na(col)
    n_miss <- sum(is_miss)
    pct_miss <- if (n_rows > 0) round((n_miss / n_rows) * 100, 1) else NA_real_
    n_uniq <- length(unique(col[!is_miss]))
    
    tibble::tibble(
      pos = i,
      variable = var_name,
      type = col_type,
      label = var_label,
      value_labels = val_str,
      n_unique = n_uniq,
      n_missing = n_miss,
      pct_missing = pct_miss
    )
  })
  
  cb <- dplyr::bind_rows(rows)
  
  if (!is.null(query) && nzchar(query)) {
    rx <- stringr::regex(query, ignore_case = TRUE)
    cb <- dplyr::filter(
      cb,
      stringr::str_detect(.data$variable, rx) |
        stringr::str_detect(dplyr::coalesce(.data$label, ""), rx) |
        stringr::str_detect(dplyr::coalesce(.data$value_labels, ""), rx)
    )
  }
  
  cb
}

# Internal helper to retrieve project-level metadata for a single ARDA_ID
get_dataset_meta <- function(arda_id) {
  if (missing(arda_id) || is.null(arda_id) || !nzchar(trimws(arda_id))) {
    stop("Supply a valid `arda_id` (e.g., \"1979ABC\").", call. = FALSE)
  }
  
  target_id <- toupper(trimws(arda_id))
  proj <- get_project_catalog()
  meta <- NULL
  
  if (!is.null(proj)) {
    matches <- dplyr::filter(proj, toupper(.data$ARDA_ID) == target_id)
    if (nrow(matches) > 0) {
      meta <- matches[1, ]
    }
  }
  
  # Fallback to file-level catalog if project catalog is absent or lacks this ID
  if (is.null(meta)) {
    fcat <- get_file_catalog()
    fmatches <- dplyr::filter(fcat, toupper(.data$ARDA_ID) == target_id)
    if (nrow(fmatches) == 0) {
      stop(sprintf("Dataset '%s' not found in the ARDA catalog.", arda_id), call. = FALSE)
    }
    meta <- fmatches[1, ]
  }
  
  title <- as.character(meta$Title[1] %||% target_id)
  year <- if ("Publication_Year" %in% names(meta) && !is.na(meta$Publication_Year[1])) {
    as.character(meta$Publication_Year[1])
  } else {
    "n.d."
  }
  authors <- if ("Authors_Investigators" %in% names(meta) &&
                 !is.na(meta$Authors_Investigators[1]) &&
                 nzchar(trimws(as.character(meta$Authors_Investigators[1])))) {
    trimws(as.character(meta$Authors_Investigators[1]))
  } else {
    "Association of Religion Data Archives (ARDA)"
  }
  arda_url <- if ("ARDA_URL" %in% names(meta) &&
                  !is.na(meta$ARDA_URL[1]) &&
                  nzchar(trimws(as.character(meta$ARDA_URL[1])))) {
    trimws(as.character(meta$ARDA_URL[1]))
  } else {
    paste0("https://www.thearda.com/data-archive?fid=", target_id)
  }
  osf_id <- if ("OSF_ID" %in% names(meta) && !is.na(meta$OSF_ID[1])) {
    as.character(meta$OSF_ID[1])
  } else if ("OSF_Project_ID" %in% names(meta) && !is.na(meta$OSF_Project_ID[1])) {
    as.character(meta$OSF_Project_ID[1])
  } else {
    NA_character_
  }
  
  list(
    arda_id = target_id,
    title = title,
    year = year,
    authors = authors,
    arda_url = arda_url,
    osf_id = osf_id
  )
}

#' Generate a Formatted Citation for an ARDA Dataset
#'
#' Looks up bibliographic metadata for an ARDA dataset and prints a formatted
#' citation in APA, Chicago, or BibTeX style.
#'
#' @param arda_id Character ARDA dataset ID (e.g., `"1979ABC"` or `"MORMON"`).
#' @param style Citation format: `"apa"` (default), `"chicago"`, or `"bibtex"`.
#' @return Invisibly returns the formatted citation string.
#' @export
arda_cite <- function(arda_id, style = c("apa", "chicago", "bibtex")) {
  style <- match.arg(tolower(style), c("apa", "chicago", "bibtex"))
  meta <- get_dataset_meta(arda_id)
  
  citation_text <- switch(
    style,
    "apa" = sprintf(
      "%s. (%s). %s [Data set]. The Association of Religion Data Archives (ARDA). %s",
      meta$authors, meta$year, meta$title, meta$arda_url
    ),
    "chicago" = sprintf(
      "%s. %s. \"%s.\" The Association of Religion Data Archives (ARDA). %s.",
      meta$authors, meta$year, meta$title, meta$arda_url
    ),
    "bibtex" = {
      key <- tolower(gsub("[^a-zA-Z0-9]", "", paste0("arda_", meta$arda_id)))
      sprintf(
        paste0(
          "@misc{%s,\n",
          "  author       = {%s},\n",
          "  title        = {{%s}},\n",
          "  year         = {%s},\n",
          "  howpublished = {The Association of Religion Data Archives (ARDA)},\n",
          "  url          = {%s}\n",
          "}"
        ),
        key, meta$authors, meta$title, meta$year, meta$arda_url
      )
    }
  )
  
  cat(citation_text, "\n", sep = "")
  invisible(citation_text)
}

#' Open an ARDA Dataset's Archive Page in a Web Browser
#'
#' Launches the official `thearda.com` archive page (or OSF repository page) for
#' a given ARDA dataset ID.
#'
#' @param arda_id Character ARDA dataset ID (e.g., `"1979ABC"` or `"MORMON"`).
#' @param target Which page to open: `"arda"` (default, opens `thearda.com`) or
#'   `"osf"` (opens the underlying Open Science Framework project page).
#' @return Invisibly returns the URL opened.
#' @export
arda_browse <- function(arda_id, target = c("arda", "osf")) {
  target <- match.arg(tolower(target), c("arda", "osf"))
  meta <- get_dataset_meta(arda_id)
  
  url <- if (target == "osf") {
    if (is.na(meta$osf_id) || !nzchar(meta$osf_id)) {
      stop(sprintf("No OSF project ID recorded for dataset '%s'.", meta$arda_id), call. = FALSE)
    }
    paste0("https://osf.io/", meta$osf_id, "/")
  } else {
    meta$arda_url
  }
  
  if (interactive()) {
    utils::browseURL(url)
  } else {
    message("Non-interactive session; dataset URL: ", url)
  }
  
  invisible(url)
}

# Internal helper to format byte counts cleanly
format_bytes <- function(bytes) {
  vapply(bytes, function(b) {
    if (is.na(b) || b < 0) return(NA_character_)
    if (b < 1024) return(sprintf("%d B", as.integer(b)))
    if (b < 1024^2) return(sprintf("%.1f KB", b / 1024))
    if (b < 1024^3) return(sprintf("%.2f MB", b / 1024^2))
    sprintf("%.2f GB", b / 1024^3)
  }, character(1))
}

#' List Cached ARDA Datasets and Catalog Files
#'
#' Inspects the local cache directory (`arda_cache_dir()`) and returns a tibble
#' of all downloaded datasets and catalog files along with their sizes and modification times.
#'
#' @param cache_dir Directory to inspect. Defaults to `arda_cache_dir()`.
#' @param include_catalog Logical; if `TRUE` (default), includes cached crosswalk
#'   CSV files alongside downloaded dataset files.
#' @return A tibble with columns `arda_id`, `filename`, `type`, `size_bytes`,
#'   `size`, `modified`, and `path`.
#' @export
arda_cache_list <- function(cache_dir = arda_cache_dir(), include_catalog = TRUE) {
  if (!dir.exists(cache_dir)) {
    return(tibble::tibble(
      arda_id = character(),
      filename = character(),
      type = character(),
      size_bytes = numeric(),
      size = character(),
      modified = as.POSIXct(character()),
      path = character()
    ))
  }
  
  files <- list.files(cache_dir, full.names = TRUE, recursive = FALSE)
  files <- files[!dir.exists(files)]
  
  if (length(files) == 0) {
    message("ARDA cache is currently empty: ", cache_dir)
    return(tibble::tibble(
      arda_id = character(),
      filename = character(),
      type = character(),
      size_bytes = numeric(),
      size = character(),
      modified = as.POSIXct(character()),
      path = character()
    ))
  }
  
  fnames <- basename(files)
  is_cat <- fnames %in% c("ARDA_OSF_file_level_crosswalk.csv", "ARDA_OSF_crosswalk.csv")
  
  if (!isTRUE(include_catalog)) {
    files <- files[!is_cat]
    fnames <- fnames[!is_cat]
    is_cat <- is_cat[!is_cat]
  }
  
  if (length(files) == 0) {
    message("No cached dataset files found in: ", cache_dir)
    return(tibble::tibble(
      arda_id = character(),
      filename = character(),
      type = character(),
      size_bytes = numeric(),
      size = character(),
      modified = as.POSIXct(character()),
      path = character()
    ))
  }
  
  info <- file.info(files)
  # Extract ARDA_ID prefix before first underscore for dataset files
  arda_ids <- ifelse(
    is_cat,
    NA_character_,
    sub("_.*$", "", fnames)
  )
  
  res <- tibble::tibble(
    arda_id = arda_ids,
    filename = fnames,
    type = ifelse(is_cat, "catalog", "dataset"),
    size_bytes = info$size,
    size = format_bytes(info$size),
    modified = info$mtime,
    path = normalizePath(files, mustWork = FALSE)
  ) |>
    dplyr::arrange(.data$type, dplyr::desc(.data$modified))
  
  total_str <- format_bytes(sum(res$size_bytes, na.rm = TRUE))
  message(sprintf("ARDA cache (%s): %d file(s), %s total", cache_dir, nrow(res), total_str))
  res
}

#' Clear Cached ARDA Dataset Files
#'
#' Removes cached dataset files from `arda_cache_dir()`. By default, preserves
#' any updated catalog CSVs so you do not have to re-download the index.
#'
#' @param arda_id Optional character ARDA dataset ID (e.g., `"1979ABC"`) to remove
#'   only files belonging to that dataset. If `NULL` (default), clears all cached datasets.
#' @param include_catalog Logical; if `TRUE`, also deletes cached catalog CSV files
#'   and resets the in-memory catalog cache. Defaults to `FALSE`.
#' @param cache_dir Directory to clean. Defaults to `arda_cache_dir()`.
#' @param ask Logical; if `TRUE` (default in interactive sessions), asks for confirmation
#'   before deleting files.
#' @return Invisibly returns the number of files deleted.
#' @export
arda_clear_cache <- function(arda_id = NULL,
                             include_catalog = FALSE,
                             cache_dir = arda_cache_dir(),
                             ask = interactive()) {
  if (!dir.exists(cache_dir)) {
    message("Cache directory does not exist: ", cache_dir)
    return(invisible(0L))
  }
  
  files <- list.files(cache_dir, full.names = TRUE, recursive = FALSE)
  files <- files[!dir.exists(files)]
  fnames <- basename(files)
  is_cat <- fnames %in% c("ARDA_OSF_file_level_crosswalk.csv", "ARDA_OSF_crosswalk.csv")
  
  if (!is.null(arda_id)) {
    target_prefix <- paste0(toupper(trimws(arda_id)), "_")
    keep <- startsWith(toupper(fnames), target_prefix) & !is_cat
    files <- files[keep]
  } else if (!isTRUE(include_catalog)) {
    files <- files[!is_cat]
  }
  
  if (length(files) == 0) {
    message("No matching cached files to remove.")
    return(invisible(0L))
  }
  
  total_size <- format_bytes(sum(file.info(files)$size, na.rm = TRUE))
  
  if (isTRUE(ask) && interactive()) {
    ans <- readline(sprintf(
      "Delete %d cached file(s) (%s) from %s? [y/N]: ",
      length(files), total_size, cache_dir
    ))
    if (!tolower(trimws(ans)) %in% c("y", "yes")) {
      message("Cache clear cancelled.")
      return(invisible(0L))
    }
  }
  
  removed <- file.remove(files)
  n_removed <- sum(removed, na.rm = TRUE)
  
  if (isTRUE(include_catalog)) {
    .arda_env$file_catalog <- NULL
    .arda_env$project_catalog <- NULL
  }
  
  message(sprintf("Removed %d cached file(s) (%s freed).", n_removed, total_size))
  invisible(n_removed)
}