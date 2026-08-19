# =============================================================================
# 00_cs_setup.R — One line at the top of the .qmd pulls in everything
# =============================================================================
#   config <- yaml::read_yaml(here::here("config", "districts", "cs_nashville.yaml"))
#   source(here::here(config$helpers_path, "00_cs_setup.R"))
#
# Sourcing order matters: the shared check log has to exist before the course
# schedule review helpers load, because check_gt() reports into it.
# =============================================================================

# LOAD ORDER MATTERS. The pipeline code is written without namespace prefixes,
# so whichever package attaches LAST wins any name collision. data.table masks
# first(), last() and between() from dplyr, so tidyverse goes last and dplyr's
# versions are the ones in scope.
suppressPackageStartupMessages({
  library(data.table)     # first, so tidyverse can mask it
  library(openxlsx)
  library(readxl)
  library(here)
  library(yaml)
  library(janitor)        # clean_names()
  library(rlang)          # parse_expr()
  library(Microsoft365R)
  library(erstools)
  library(gt)
  library(htmltools)
  library(scales)
  library(tidyverse)      # last: dplyr wins every collision
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
for (f in c("00_io.R", "00_joins.R", "00_prep.R")) {
  p <- file.path(.shared, f)
  if (!file.exists(p)) stop("Shared helper not found: ", p, call. = FALSE)
  source(p)
}

# --- Course schedule engine ---------------------------------------------------
.hp <- here::here(config$helpers_path)
.cs_helpers <- c(
  "00_cs_gates.R",
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

# =============================================================================
# How far the pipeline runs
# =============================================================================
# run_through is the only control. Stages after it do not execute — not in Run
# All, not in Render. Everything that does run shows its checks.
#
# Set config$report$run_through: "01" while you work through the explore stage,
# then raise it as each stage settles. Nothing is written to SharePoint unless
# it is "04".
# =============================================================================

.cs_run_through <- "04"

cs_set_run_through <- function(config) {

  # Accepts either a single value ("02") or a list. With a list, the pipeline
  # runs through the HIGHEST stage supplied — so commenting lines out is how
  # you pull the stopping point back:
  #
  #   run_through: ["01", "02"]        -> runs 01 and 02
  #   run_through: ["01", "02",
  #   #             "03"]              -> runs 01 and 02
  stages <- as.character(unlist(config$report$run_through %||% "04"))

  unknown <- setdiff(stages, c("01", "02", "03", "04"))
  if (length(unknown) > 0) {
    stop("config$report$run_through must contain only the quoted strings ",
         '"01", "02", "03", "04" — got: ', paste(unknown, collapse = ", "),
         call. = FALSE)
  }

  # Everything commented out means the explore stage only; Stage 01 always runs.
  .cs_run_through <<- if (length(stages) == 0) "01" else max(stages)

  message("Pipeline runs through stage ", .cs_run_through,
          if (.cs_run_through < "04")
            " — later stages skipped, nothing written to SharePoint" else "")
  invisible(.cs_run_through)
}

#' TRUE when this stage should execute.
run_stage <- function(stage) as.character(stage) <= .cs_run_through

#' Emit a heading only when its stage runs, so a skipped stage leaves no empty
#' section behind.
cs_heading <- function(stage, text) {
  if (run_stage(stage)) cat("\n", text, "\n\n", sep = "")
  invisible(NULL)
}


# =============================================================================
# Config readiness
# =============================================================================
# The config is filled in over several sittings as the data reveals itself.
# This reports which tiers are complete, so "what do I still need to learn?"
# is answerable from the report rather than by scrolling the YAML.
# =============================================================================

.cs_tier_spec <- list(
  list(tier = "0", when = "Before you run anything",
       items = c("district", "year", "folders", "sources")),
  list(tier = "1", when = "After the raw profile",
       items = c("column_map", "intake", "stu_demographics_join",
                 "derivations", "scope")),
  list(tier = "2", when = "Once you understand each file",
       items = c("add_ons", "coding", "exclusions")),
  list(tier = "3", when = "After the district explains bells and terms",
       items = c("class_id", "time_format", "time_cols", "weights",
                 "full_utilization", "bell_minutes")),
  list(tier = "4", when = "After you see DB rates",
       items = c("db_sizing_filters", "db_resolution", "teacher_db")),
  list(tier = "5", when = "Reporting choices",
       items = c("self_contained", "buckets", "report", "outputs"))
)

.cs_filled <- function(x) {
  if (is.null(x)) return("empty")
  if (is.list(x) && length(x) == 0) return("empty")
  if (is.list(x)) {
    n_empty <- sum(vapply(x, function(v)
      is.null(v) || (is.list(v) && length(v) == 0), logical(1)))
    if (n_empty == length(x)) return("empty")
    if (n_empty > 0) return(paste0("partial (", length(x) - n_empty, "/", length(x), ")"))
    return("set")
  }
  if (length(x) == 0 || all(is.na(x))) return("empty")
  "set"
}

config_readiness <- function(config) {
  rows <- lapply(.cs_tier_spec, function(t) {
    data.frame(
      tier    = t$tier,
      when    = t$when,
      section = t$items,
      status  = vapply(t$items, function(k) .cs_filled(config[[k]]), character(1)),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  incomplete <- unique(out$tier[out$status != "set"])
  chk_info("Config tiers not yet complete",
           if (length(incomplete) == 0) "none" else paste(incomplete, collapse = ", "))
  out
}