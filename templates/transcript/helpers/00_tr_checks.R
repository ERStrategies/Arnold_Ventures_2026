# =============================================================================
# checks.R — Two-severity check framework
# =============================================================================
# DISTRICT-AGNOSTIC. Do not put Nashville logic in this file.
#
# Two severities, by design:
#
#   stop  Structural failure. The pipeline cannot produce a correct number
#         from here. Records FAIL, prints the offending rows, halts the render.
#
#   info  A count or table to eyeball. Always recorded, never halts.
#
# Everything lands in one log. chk_report() prints it at the end, so there is
# a single place that says "22 checks, 22 passed."
#
# Typical use:
#   chk_stage("03 — Student cleaning")
#   chk_unique(stu, c("D_stu_id", "D_year"), "One row per student per year")
#   chk_info("Students in demographics file", n_distinct(stu$D_stu_id))
# =============================================================================

.chk <- new.env(parent = emptyenv())
.chk$log       <- NULL
.chk$stage     <- NA_character_
.chk$offenders <- list()
.chk$out_dir   <- NULL

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}


# --- Log lifecycle -----------------------------------------------------------

#' Start a fresh check log. Call once, at the top of the pipeline.
chk_reset <- function() {
  .chk$log <- tibble::tibble(
    seq      = integer(),
    stage    = character(),
    check    = character(),
    severity = character(),
    result   = character(),
    value    = character(),
    detail   = character(),
    time     = as.POSIXct(character())
  )
  .chk$offenders <- list()
  .chk$stage     <- NA_character_
  invisible(TRUE)
}

#' Where failed-check artefacts get written.
#'
#' When a check fails mid-render, knitr discards the chunk's output, so
#' anything chk_stop() printed is lost and all you see is the error. Setting a
#' directory means the offending rows and the log-so-far are written to disk
#' before the run halts, and the error message names the file.
chk_set_output_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  .chk$out_dir <- path
  invisible(path)
}

#' Label the stage that subsequent checks belong to.
chk_stage <- function(stage) {
  .chk$stage <- as.character(stage)
  cat("\n=== ", stage, " ", strrep("=", max(0, 60 - nchar(stage))), "\n", sep = "")
  invisible(stage)
}

#' Return the check log as a tibble.
chk_log <- function() .chk$log

#' Retrieve the offending rows stored by a failed stop-check.
#' @param which Check name, or an index. Omit to list what is stored.
chk_offenders <- function(which = NULL) {
  if (is.null(which)) return(names(.chk$offenders))
  if (is.numeric(which)) return(.chk$offenders[[which]])
  .chk$offenders[[which]]
}


# --- Internals ---------------------------------------------------------------

.chk_fmt <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  if (is.numeric(x) && length(x) == 1 && !is.na(x)) {
    return(formatC(x, big.mark = ",", format = if (x %% 1 == 0) "d" else "g"))
  }
  paste(as.character(x), collapse = ", ")
}

.chk_record <- function(check, severity, result, value = NULL, detail = NULL) {
  if (is.null(.chk$log)) chk_reset()
  .chk$log <- dplyr::bind_rows(
    .chk$log,
    tibble::tibble(
      seq      = nrow(.chk$log) + 1L,
      stage    = .chk$stage %||% NA_character_,
      check    = check,
      severity = severity,
      result   = result,
      value    = .chk_fmt(value),
      detail   = .chk_fmt(detail),
      time     = Sys.time()
    )
  )
  invisible(.chk$log)
}


# --- The two primitives ------------------------------------------------------

#' A structural check. Halts the render if it fails.
#'
#' @param check     Human-readable name. This is what appears in the report.
#' @param condition Logical. TRUE passes. FALSE or NA fails.
#' @param value     Optional number to record alongside the result.
#' @param detail    Optional note, shown in the report.
#' @param offenders Optional data frame of the rows that broke the check.
#'                  Printed on failure and kept in chk_offenders().
#' @param n_show    How many offending rows to print.
chk_stop <- function(check, condition, value = NULL, detail = NULL,
                     offenders = NULL, n_show = 10) {

  passed <- isTRUE(tryCatch(all(condition), error = function(e) NA))

  if (passed) {
    .chk_record(check, "stop", "PASS", value, detail)
    cat("[PASS] ", check,
        if (!is.null(value)) paste0(" — ", .chk_fmt(value)) else "", "\n", sep = "")
    return(invisible(TRUE))
  }

  .chk_record(check, "stop", "FAIL", value, detail)
  if (!is.null(offenders)) .chk$offenders[[check]] <- offenders

  cat("\n[FAIL] ", check, "\n", sep = "")
  if (!is.null(detail)) cat("       ", .chk_fmt(detail), "\n", sep = "")
  if (!is.null(offenders) && nrow(offenders) > 0) {
    cat("       ", nrow(offenders), " offending row(s). First ",
        min(n_show, nrow(offenders)), ":\n\n", sep = "")
    print(utils::head(as.data.frame(offenders), n_show))
    cat("\n       Full set: chk_offenders(\"", check, "\")\n", sep = "")
  }
  cat("\n--- Check log up to this point ---\n")
  print(as.data.frame(.chk$log[, c("seq", "stage", "check", "result", "value")]))

  # Persist the evidence. knitr throws away chunk output when a chunk errors,
  # so without this the only thing that survives the render is the error line.
  written <- NULL
  if (!is.null(.chk$out_dir)) {
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    slug  <- tolower(gsub("[^A-Za-z0-9]+", "_", check))
    slug  <- substr(gsub("^_|_$", "", slug), 1, 60)
    try({
      utils::write.csv(as.data.frame(.chk$log),
                       file.path(.chk$out_dir, paste0(stamp, "__check_log.csv")),
                       row.names = FALSE)
      if (!is.null(offenders)) {
        written <- file.path(.chk$out_dir, paste0(stamp, "__", slug, ".csv"))
        utils::write.csv(as.data.frame(offenders), written, row.names = FALSE)
      }
    }, silent = TRUE)
  }

  # Fold the diagnosis into the error itself, so it is visible even when the
  # chunk output is discarded and no file could be written.
  preview <- ""
  if (!is.null(offenders) && nrow(offenders) > 0) {
    pv <- utils::capture.output(print(utils::head(as.data.frame(offenders), 8)))
    preview <- paste0("\n", paste(pv, collapse = "\n"))
  }

  stop("Structural check failed: ", check,
       if (!is.null(detail)) paste0("\n  ", .chk_fmt(detail)) else "",
       preview,
       if (!is.null(written)) paste0("\n\n  Offenders written to: ", written) else "",
       "\n  In an interactive session: chk_offenders(\"", check, "\")",
       call. = FALSE)
}

#' A number or table to eyeball. Never halts.
#'
#' @param data Optional data frame to print under the value.
chk_info <- function(check, value = NULL, detail = NULL, data = NULL, n_show = 20) {
  .chk_record(check, "info", "INFO", value, detail)
  cat("[info] ", check,
      if (!is.null(value)) paste0(": ", .chk_fmt(value)) else "", "\n", sep = "")
  if (!is.null(detail)) cat("       ", .chk_fmt(detail), "\n", sep = "")
  if (!is.null(data)) {
    print(utils::head(as.data.frame(data), n_show))
    if (nrow(data) > n_show) cat("       ... ", nrow(data) - n_show, " more row(s)\n", sep = "")
  }
  invisible(value)
}


# --- Convenience wrappers ----------------------------------------------------
# These are the checks that turn out to be needed over and over. Each is a
# thin call to chk_stop() with the offenders already assembled.

#' Stop unless `keys` uniquely identify rows in `data`.
chk_unique <- function(data, keys, check = NULL) {
  check <- check %||% paste0("Unique on ", paste(keys, collapse = " + "))
  missing_cols <- setdiff(keys, names(data))
  if (length(missing_cols) > 0) {
    chk_stop(check, FALSE, detail = paste("Column(s) not in data:",
                                          paste(missing_cols, collapse = ", ")))
  }
  dupes <- data |>
    dplyr::count(dplyr::across(dplyr::all_of(keys)), name = "n_rows") |>
    dplyr::filter(n_rows > 1) |>
    dplyr::arrange(dplyr::desc(n_rows))
  chk_stop(check, nrow(dupes) == 0,
           value  = paste0(nrow(data), " rows"),
           detail = if (nrow(dupes) > 0) paste0(nrow(dupes), " duplicated key combination(s)"),
           offenders = dupes)
}

#' Stop if any of `cols` contain NA.
chk_no_na <- function(data, cols, check = NULL) {
  check <- check %||% paste0("No missing ", paste(cols, collapse = ", "))
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    chk_stop(check, FALSE, detail = paste("Column(s) not in data:",
                                          paste(missing_cols, collapse = ", ")))
  }
  bad <- data |> dplyr::filter(dplyr::if_any(dplyr::all_of(cols), is.na))
  chk_stop(check, nrow(bad) == 0,
           detail = if (nrow(bad) > 0) paste0(nrow(bad), " row(s) with NA"),
           offenders = bad)
}

#' Stop if the row count changed. Pass data frames or plain counts.
chk_rows_stable <- function(before, after, check = "Row count unchanged") {
  n_before <- if (is.data.frame(before)) nrow(before) else as.numeric(before)
  n_after  <- if (is.data.frame(after))  nrow(after)  else as.numeric(after)
  chk_stop(check, n_before == n_after,
           value  = paste0(format(n_before, big.mark = ","), " rows"),
           detail = if (n_before != n_after)
             paste0("before = ", format(n_before, big.mark = ","),
                    ", after = ",  format(n_after,  big.mark = ","),
                    " (", ifelse(n_after > n_before, "+", ""), n_after - n_before, ")"))
}

#' Stop if the set of distinct IDs changed.
chk_ids_stable <- function(before, after, id_col, check = NULL) {
  check <- check %||% paste0("Distinct ", id_col, " unchanged")
  ids_before <- unique(before[[id_col]])
  ids_after  <- unique(after[[id_col]])
  lost   <- setdiff(ids_before, ids_after)
  gained <- setdiff(ids_after, ids_before)
  chk_stop(check, length(lost) == 0 && length(gained) == 0,
           value  = paste0(length(ids_before), " ids"),
           detail = if (length(lost) || length(gained))
             paste0(length(lost), " lost, ", length(gained), " gained"),
           offenders = tibble::tibble(
             id_col    = id_col,
             direction = c(rep("lost", length(lost)), rep("gained", length(gained))),
             value     = c(lost, gained)))
}

#' Stop unless every value of `col` is in `allowed`.
chk_values_in <- function(data, col, allowed, check = NULL) {
  check <- check %||% paste0(col, " uses only expected values")
  observed <- unique(stats::na.omit(data[[col]]))
  unexpected <- setdiff(observed, allowed)
  chk_stop(check, length(unexpected) == 0,
           value  = paste0(length(observed), " distinct value(s)"),
           detail = if (length(unexpected) > 0)
             paste("Unexpected:", paste(utils::head(unexpected, 20), collapse = " | ")),
           offenders = data |> dplyr::filter(.data[[col]] %in% unexpected))
}

#' Info: distinct count of a column.
chk_n_distinct <- function(data, col, check = NULL) {
  check <- check %||% paste0("Distinct ", col)
  chk_info(check, dplyr::n_distinct(data[[col]]))
}


# --- Final report ------------------------------------------------------------

#' Print the run summary. Call at the end of the pipeline.
#' Returns the log invisibly so it can be written to disk or knitted.
chk_report <- function(print_table = TRUE) {
  log <- .chk$log
  if (is.null(log) || nrow(log) == 0) {
    cat("No checks recorded.\n")
    return(invisible(NULL))
  }
  n_stop <- sum(log$severity == "stop")
  n_fail <- sum(log$result   == "FAIL")
  n_info <- sum(log$severity == "info")

  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("CHECK SUMMARY\n")
  cat(strrep("=", 70), "\n", sep = "")
  cat("  Structural checks : ", n_stop, " (", n_stop - n_fail, " passed, ",
      n_fail, " failed)\n", sep = "")
  cat("  Info checks       : ", n_info, "\n", sep = "")
  cat("  Stages            : ", dplyr::n_distinct(log$stage), "\n", sep = "")
  cat(strrep("=", 70), "\n\n", sep = "")

  if (print_table) {
    print(as.data.frame(log[, c("seq", "stage", "check", "severity", "result", "value")]))
  }
  invisible(log)
}