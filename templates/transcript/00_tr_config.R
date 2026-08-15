# =============================================================================
# config.R — Load and validate the district config
# =============================================================================
# DISTRICT-AGNOSTIC.
#
# The point of validation here is that a malformed config should fail on line
# one with a list of everything that is wrong, rather than producing plausible
# wrong numbers three stages later.
# =============================================================================

#' Load a district YAML config and validate it.
#'
#' @param district Slug, e.g. "nashville". Resolves to
#'   config/districts/tr_<district>.yaml.
#' @param path     Full path, overriding `district`.
#' @param validate Run validate_config(). Leave TRUE.
load_config <- function(district = "nashville", path = NULL, validate = TRUE) {
  path <- path %||% here::here("config", "districts", paste0("tr_", district, ".yaml"))
  if (!file.exists(path)) stop("Config not found: ", path, call. = FALSE)

  cfg <- yaml::read_yaml(path)
  cfg$.meta <- list(
    path     = path,
    district = district,
    loaded   = Sys.time(),
    md5      = tools::md5sum(path)[[1]]
  )
  if (validate) validate_config(cfg)
  cfg
}

#' Validate a config. Collects every problem, then stops with all of them.
validate_config <- function(cfg) {
  problems <- character()
  need <- function(cond, msg) if (!isTRUE(cond)) problems <<- c(problems, msg)
  has  <- function(x) !is.null(x) && length(x) > 0 && !identical(x, "")

  # --- top level ------------------------------------------------------------
  for (k in c("district", "year", "raw_data_folder_path", "raw_files",
              "column_map", "cohort", "derived", "thresholds")) {
    need(has(cfg[[k]]), paste0("Missing top-level key: `", k, "`"))
  }

  # --- files ----------------------------------------------------------------
  for (k in c("transcript_raw", "stu_demographics")) {
    need(has(cfg$raw_files[[k]]), paste0("Missing `raw_files$", k, "`"))
  }
  need(has(cfg$source_of_truth_files$course_coding),
       "Missing `source_of_truth_files$course_coding` (the final coded course file)")

  # --- column map -----------------------------------------------------------
  required_cols <- list(
    transcript   = c("D_stu_id", "D_course_id", "D_course_name",
                     "D_creds_earn", "D_creds_possible"),
    demographics = c("D_stu_id", "D_location_name", "D_stu_grade_level",
                     "D_stu_enter_date", "D_stu_exit_date", "D_year")
  )
  for (tbl in names(required_cols)) {
    map <- cfg$column_map[[tbl]]
    need(has(map), paste0("Missing `column_map$", tbl, "`"))
    if (has(map)) {
      for (std in required_cols[[tbl]]) {
        need(has(map[[std]]),
             paste0("`column_map$", tbl, "` has no entry for `", std, "`"))
      }
      dupes <- names(which(table(unlist(map)) > 1))
      need(length(dupes) == 0,
           paste0("`column_map$", tbl, "` maps the same district column to more ",
                  "than one standard name: ", paste(dupes, collapse = ", ")))
    }
  }

  # --- cohort ---------------------------------------------------------------
  ch <- cfg$cohort
  for (k in c("entry_year", "entry_grade", "years_required",
              "duration_min_days", "schools_to_include")) {
    need(has(ch[[k]]), paste0("Missing `cohort$", k, "`"))
  }
  need(is.null(ch$duration_rule) || ch$duration_rule %in% c("all_years", "bookends", "none"),
       "`cohort$duration_rule` must be all_years, bookends, or none")
  need(is.null(ch$progression_rule) || ch$progression_rule %in% c("all_years", "bookends", "none"),
       "`cohort$progression_rule` must be all_years, bookends, or none")

  if (has(ch$progression) && has(ch$years_required)) {
    need(length(ch$progression) == ch$years_required,
         paste0("`cohort$progression` has ", length(ch$progression), " year(s) but ",
                "`cohort$years_required` is ", ch$years_required))
  }
  if (has(ch$schools_to_include)) {
    trimmed <- trimws(ch$schools_to_include)
    need(!any(duplicated(trimmed)),
         paste0("`cohort$schools_to_include` has duplicate entries: ",
                paste(unique(trimmed[duplicated(trimmed)]), collapse = ", ")))
  }

  # --- derived / thresholds -------------------------------------------------
  need(has(cfg$derived$transcript_year_from),
       "Missing `derived$transcript_year_from` (which column becomes D_year on the transcript)")
  need(has(cfg$thresholds$min_course_coding_match_pct),
       "Missing `thresholds$min_course_coding_match_pct`")

  if (length(problems) > 0) {
    stop("\nConfig validation failed (", length(problems), " problem",
         if (length(problems) > 1) "s", "):\n",
         paste0("  - ", problems, collapse = "\n"), "\n", call. = FALSE)
  }
  invisible(TRUE)
}

#' One-screen summary of what the pipeline is about to run on.
config_summary <- function(cfg) {
  cat(strrep("=", 70), "\n", sep = "")
  cat("DISTRICT : ", cfg$district, "\n", sep = "")
  cat("YEARS    : ", cfg$year, "\n", sep = "")
  cat("CONFIG   : ", basename(cfg$.meta$path), "  (md5 ",
      substr(cfg$.meta$md5, 1, 8), ")\n", sep = "")
  cat(strrep("-", 70), "\n", sep = "")
  cat("COHORT   : entered grade ", cfg$cohort$entry_grade,
      " in ", cfg$cohort$entry_year,
      ", ", cfg$cohort$years_required, " years enrolled\n", sep = "")
  cat("           duration rule = ", cfg$cohort$duration_rule %||% "none",
      " (>= ", cfg$cohort$duration_min_days, " days/yr)\n", sep = "")
  cat("           progression rule = ", cfg$cohort$progression_rule %||% "none", "\n", sep = "")
  cat("           ", length(cfg$cohort$schools_to_include), " school(s) in scope\n", sep = "")
  cat(strrep("-", 70), "\n", sep = "")
  cat("SOURCES  : ", length(cfg$raw_files), " raw, ",
      length(cfg$source_of_truth_files), " source-of-truth\n", sep = "")
  cat("CACHE    : ", cache_dir(cfg), "\n", sep = "")
  cat(strrep("=", 70), "\n", sep = "")
  invisible(cfg)
}
