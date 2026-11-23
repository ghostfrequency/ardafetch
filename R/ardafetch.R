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
  switch(
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
      dest_path
    }
  )
}
