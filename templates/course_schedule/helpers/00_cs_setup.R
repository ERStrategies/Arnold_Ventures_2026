# =============================================================================
# 00_cs_setup.R — One line at the top of the .qmd pulls in everything
# =============================================================================
#   config <- yaml::read_yaml(here::here("config", "districts", "cs_nashville.yaml"))
#   source(here::here(config$helpers_path, "00_cs_setup.R"))
#
# Sourcing order matters: the shared check log has to exist before the course
# schedule review helpers load, because check_gt() reports into it.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(lubridate)
  library(glue)
  library(stringr)
  library(openxlsx)
  library(readxl)
  library(here)
  library(yaml)
  library(Microsoft365R)
  library(erstools)
  library(gt)
  library(htmltools)
  library(scales)
})

options(scipen = 99)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

if (!exists("config")) {
  stop("Load `config` before sourcing this file.", call. = FALSE)
}

# --- Shared machinery, used by every ERS pipeline ----------------------------
.shared <- here::here(config$shared_helpers_path %||% "templates/shared")
for (f in c("00_checks.R", "00_io.R", "00_joins.R", "00_prep.R")) {
  p <- file.path(.shared, f)
  if (!file.exists(p)) stop("Shared helper not found: ", p, call. = FALSE)
  source(p)
}

# --- Course schedule engine ---------------------------------------------------
.hp <- here::here(config$helpers_path)
.cs_helpers <- c(
  "01_cs_explore_helpers.R",
  "01_cs_review_helpers.R",
  "02_cs_db_helpers.R",
  "02_cs_expression_helpers.R",
  "03_cs_weight_helpers.R",
  "03_cs_explosion_helpers.R",
  "03_cs_flagging_helpers.R",
  "03_cs_consolidation_helpers.R",
  "03_cs_resolution_helpers.R",
  "03_cs_unification_helpers.R",
  "04_cs_rollup_helpers.R",
  "04_cs_addon_teacher_hr.R",
  "04_cs_addon_course_grades.R"
)

for (f in .cs_helpers) {
  p <- file.path(.hp, f)
  if (!file.exists(p)) stop("Course schedule helper not found: ", p, call. = FALSE)
  source(p)
}

message("Helpers loaded: ", length(.cs_helpers), " course schedule + 4 shared")


# =============================================================================
# tr_safe, narrowed
# =============================================================================
# tr_safe() swallows any error and returns `otherwise`. That is right for an
# input the district may simply not have supplied, and wrong for the engine:
# if resolve_double_bookings() fails, tr_safe hands back NULL, every stage
# downstream degrades to NULL or NA, and the document renders as though
# nothing happened.
#
# cs_require() is the counterpart. It runs the expression, and if the result
# is missing, empty, or errors, it fails the run with the check named — so the
# failure surfaces where it happened rather than as an empty rollup later.
# =============================================================================

cs_require <- function(expr, check, min_rows = 1) {
  res <- tryCatch(expr, error = function(e) structure(list(msg = conditionMessage(e)),
                                                      class = "cs_error"))

  if (inherits(res, "cs_error")) {
    chk_stop(check, FALSE, detail = paste("Errored:", res$msg))
  }
  if (is.null(res)) {
    chk_stop(check, FALSE, detail = "Returned NULL")
  }
  if (is.data.frame(res) && nrow(res) < min_rows) {
    chk_stop(check, FALSE,
             value  = paste0(nrow(res), " rows"),
             detail = paste0("Returned fewer than ", min_rows, " row(s)"))
  }

  chk_stop(check, TRUE,
           value = if (is.data.frame(res))
             paste0(format(nrow(res), big.mark = ","), " rows x ", ncol(res), " cols")
           else NULL)
  res
}

#' Optional input: absence is a fact to report, not a failure.
cs_optional <- function(expr, label) {
  res <- tryCatch(expr, error = function(e) NULL)
  if (is.null(res)) {
    chk_info(paste0("Optional input not available: ", label), "skipped")
  } else {
    chk_info(paste0("Optional input loaded: ", label),
             if (is.data.frame(res))
               paste0(format(nrow(res), big.mark = ","), " rows x ", ncol(res), " cols")
             else "loaded")
  }
  res
}
