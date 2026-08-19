# =============================================================================
# 00_cs_gates.R — structural gates, without a check log
# =============================================================================
# Replaces the shared check-log framework for the course schedule pipeline.
#
# WHAT WENT: the running [info] commentary, the accumulating record, and the
# summary table at the end. The rendered report already shows what happened —
# check_gt() tables with PASS / CHECK badges, and the profiles. A second,
# parallel account of the same run was noise.
#
# WHAT STAYED: the gates. chk_stop() and its wrappers still halt the pipeline
# on a structural failure and still print the offending rows. That is the part
# doing real work — it is what stops the resolution engine from returning NULL
# and every later stage quietly producing empty rollups.
#
# The function NAMES are unchanged on purpose, so no call site needs editing.
# chk_info() and friends are now no-ops; chk_stop() still stops.
# =============================================================================

# --- No-ops: the log is gone, the calls are harmless -------------------------
chk_reset          <- function(...) invisible(TRUE)
chk_stage          <- function(...) invisible(TRUE)
chk_info           <- function(...) invisible(NULL)
chk_log            <- function(...) invisible(NULL)
chk_report         <- function(...) invisible(NULL)
chk_set_output_dir <- function(...) invisible(TRUE)
chk_offenders      <- function(...) invisible(NULL)
chk_from_badge     <- function(...) invisible(NULL)


# --- The gate ----------------------------------------------------------------

#' Halt the pipeline on a structural failure.
#'
#' @param check     What was being verified. Appears in the error.
#' @param condition TRUE passes. FALSE or NA fails.
#' @param value,detail Optional context shown on failure.
#' @param offenders Optional data frame of the rows that broke it. Printed, and
#'   folded into the error message so it survives knitr discarding the chunk's
#'   output when the chunk errors.
chk_stop <- function(check, condition, value = NULL, detail = NULL,
                     offenders = NULL, n_show = 10) {

  passed <- isTRUE(tryCatch(all(condition), error = function(e) NA))
  if (passed) return(invisible(TRUE))

  preview <- ""
  if (!is.null(offenders) && nrow(offenders) > 0) {
    pv <- utils::capture.output(print(utils::head(as.data.frame(offenders), n_show)))
    preview <- paste0("\n", paste(pv, collapse = "\n"))
  }

  stop("Check failed: ", check,
       if (!is.null(value))  paste0("\n  value: ",  paste(value,  collapse = ", ")) else "",
       if (!is.null(detail)) paste0("\n  ",         paste(detail, collapse = ", ")) else "",
       preview,
       call. = FALSE)
}


# --- Wrappers ----------------------------------------------------------------

#' Stop unless `keys` uniquely identify rows.
chk_unique <- function(data, keys, check = NULL) {
  check <- check %||% paste0("Unique on ", paste(keys, collapse = " + "))
  missing_cols <- setdiff(keys, names(data))
  if (length(missing_cols) > 0)
    chk_stop(check, FALSE,
             detail = paste("Column(s) not in data:", paste(missing_cols, collapse = ", ")))

  dupes <- data |>
    dplyr::count(dplyr::across(dplyr::all_of(keys)), name = "n_rows") |>
    dplyr::filter(n_rows > 1) |>
    dplyr::arrange(dplyr::desc(n_rows))

  chk_stop(check, nrow(dupes) == 0,
           detail = if (nrow(dupes) > 0)
             paste0(nrow(dupes), " duplicated key combination(s)"),
           offenders = dupes)
}

#' Stop if any of `cols` contain NA.
chk_no_na <- function(data, cols, check = NULL) {
  check <- check %||% paste0("No missing ", paste(cols, collapse = ", "))
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0)
    chk_stop(check, FALSE,
             detail = paste("Column(s) not in data:", paste(missing_cols, collapse = ", ")))

  bad <- data |> dplyr::filter(dplyr::if_any(dplyr::all_of(cols), is.na))
  chk_stop(check, nrow(bad) == 0,
           detail = if (nrow(bad) > 0) paste0(nrow(bad), " row(s) with NA"),
           offenders = bad)
}

#' Stop if the row count changed.
chk_rows_stable <- function(before, after, check = "Row count unchanged") {
  n_before <- if (is.data.frame(before)) nrow(before) else as.numeric(before)
  n_after  <- if (is.data.frame(after))  nrow(after)  else as.numeric(after)
  chk_stop(check, n_before == n_after,
           detail = if (n_before != n_after)
             paste0("before = ", format(n_before, big.mark = ","),
                    ", after = ",  format(n_after,  big.mark = ",")))
}

#' Stop if the set of distinct IDs changed.
chk_ids_stable <- function(before, after, id_col, check = NULL) {
  check  <- check %||% paste0("Distinct ", id_col, " unchanged")
  lost   <- setdiff(unique(before[[id_col]]), unique(after[[id_col]]))
  gained <- setdiff(unique(after[[id_col]]),  unique(before[[id_col]]))
  chk_stop(check, length(lost) == 0 && length(gained) == 0,
           detail = if (length(lost) || length(gained))
             paste0(length(lost), " lost, ", length(gained), " gained"),
           offenders = tibble::tibble(
             direction = c(rep("lost", length(lost)), rep("gained", length(gained))),
             value     = c(lost, gained)))
}

#' Stop unless every value of `col` is in `allowed`.
chk_values_in <- function(data, col, allowed, check = NULL) {
  check      <- check %||% paste0(col, " uses only expected values")
  observed   <- unique(stats::na.omit(data[[col]]))
  unexpected <- setdiff(observed, allowed)
  chk_stop(check, length(unexpected) == 0,
           detail = if (length(unexpected) > 0)
             paste("Unexpected:", paste(utils::head(unexpected, 20), collapse = " | ")))
}
