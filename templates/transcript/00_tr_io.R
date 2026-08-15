# =============================================================================
# io.R — Read once, cache locally, write only what matters
# =============================================================================
# DISTRICT-AGNOSTIC.
#
# The old pipeline made eight SharePoint round trips and wrote six intermediate
# files nobody read. This does one read per source file, caches it as .rds, and
# writes only the final deliverables.
#
# IMPORTANT — the cache does NOT auto-invalidate. It has no way to know the
# district re-sent a file. When new raw data lands, either:
#     read_source(..., refresh = TRUE)
#     or set params$refresh_raw: true in the .qmd header
#     or delete the cache folder
#
# SECURITY — the cache holds student-level data. Point cache_dir at a location
# covered by the data sharing agreement, and keep it out of git. The supplied
# .gitignore blocks it, but confirm the path is inside your secure area.
# =============================================================================

#' Where cached extracts live for this district.
cache_dir <- function(cfg) {
  d <- cfg$paths$cache_dir %||% here::here("cache", tolower(cfg$.meta$district))
  d
}

.slug <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

#' Resolve a source key to folder / file / drive using the config.
.resolve_source <- function(key, cfg) {
  if (!is.null(cfg$raw_files[[key]])) {
    return(list(folder = cfg$raw_data_folder_path,
                file   = cfg$raw_files[[key]],
                drive  = cfg$drives$raw_data,
                group  = "raw"))
  }
  if (!is.null(cfg$source_of_truth_files[[key]])) {
    return(list(folder = cfg$source_of_truth_folder_path,
                file   = cfg$source_of_truth_files[[key]],
                drive  = cfg$drives$source_of_truth,
                group  = "source_of_truth"))
  }
  stop("Source key `", key, "` is not in raw_files or source_of_truth_files. ",
       "Available: ",
       paste(c(names(cfg$raw_files), names(cfg$source_of_truth_files)), collapse = ", "),
       call. = FALSE)
}

#' Read a source file, using the local cache when available.
#'
#' @param key         Name under raw_files or source_of_truth_files in the config.
#' @param cfg         The config object.
#' @param refresh     TRUE forces a fresh SharePoint read and rewrites the cache.
#' @param sheet       Sheet name, for Excel sources.
#' @param clean_names Apply janitor::clean_names(). Default TRUE.
read_source <- function(key, cfg, refresh = FALSE, sheet = NULL, clean_names = TRUE) {

  src   <- .resolve_source(key, cfg)
  dir   <- cache_dir(cfg)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  cpath <- file.path(dir, paste0(.slug(key), "__", .slug(basename(src$file)), ".rds"))

  if (file.exists(cpath) && !refresh) {
    dat <- readRDS(cpath)
    chk_info(paste0("Source loaded: ", key),
             paste0(format(nrow(dat), big.mark = ","), " rows x ", ncol(dat), " cols"),
             detail = paste0("from cache, written ",
                             format(file.info(cpath)$mtime, "%Y-%m-%d %H:%M")))
    return(dat)
  }

  args <- list(folder_path = src$folder, file_name_with_extension = src$file)
  if (!is.null(src$drive)) args$drive_name  <- src$drive
  if (!is.null(sheet))     args$sheet_name  <- sheet

  message("Reading from SharePoint: ", src$file, " ...")
  dat <- do.call(ers_read_sharepoint, args)
  if (clean_names) dat <- janitor::clean_names(dat)

  saveRDS(dat, cpath)
  chk_info(paste0("Source loaded: ", key),
           paste0(format(nrow(dat), big.mark = ","), " rows x ", ncol(dat), " cols"),
           detail = paste0("fresh from SharePoint, cached to ", basename(cpath)))
  dat
}

#' Clear the cache for this district.
clear_cache <- function(cfg, key = NULL) {
  dir <- cache_dir(cfg)
  if (!dir.exists(dir)) return(invisible(FALSE))
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  if (!is.null(key)) files <- files[grepl(paste0("^", .slug(key), "__"), basename(files))]
  unlink(files)
  message("Removed ", length(files), " cached file(s) from ", dir)
  invisible(TRUE)
}

#' Write a final deliverable to SharePoint, and drop a local copy.
#'
#' Deliberately not used for intermediates. If something is worth writing,
#' name it here on purpose.
write_output <- function(data, file_name, cfg, to_sharepoint = TRUE, local = TRUE) {
  if (local) {
    dir <- cfg$paths$output_dir %||% here::here("output", tolower(cfg$.meta$district))
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    readr::write_csv(data, file.path(dir, file_name))
    message("Wrote local copy: ", file.path(dir, file_name))
  }
  if (to_sharepoint) {
    args <- list(data                     = data,
                 folder_path              = cfg$paths$output_folder_path %||% cfg$raw_data_folder_path,
                 file_name_with_extension = file_name)
    if (!is.null(cfg$drives$output)) args$drive_name <- cfg$drives$output
    do.call(ers_write_sharepoint, args)
    message("Wrote to SharePoint: ", file_name)
  }
  chk_info(paste0("Output written: ", file_name),
           paste0(format(nrow(data), big.mark = ","), " rows x ", ncol(data), " cols"))
  invisible(data)
}

#' Write the check log next to the outputs, stamped with the run.
write_check_log <- function(cfg, file_name = NULL, to_sharepoint = FALSE) {
  log <- chk_log()
  if (is.null(log) || nrow(log) == 0) {
    warning("No checks to write.")
    return(invisible(NULL))
  }
  stamp <- format(Sys.time(), "%Y%m%d_%H%M")
  file_name <- file_name %||% paste0("check_log_", tolower(cfg$.meta$district), "_", stamp, ".csv")
  log$district    <- cfg$district
  log$config_md5  <- substr(cfg$.meta$md5, 1, 8)
  write_output(log, file_name, cfg, to_sharepoint = to_sharepoint, local = TRUE)
}
