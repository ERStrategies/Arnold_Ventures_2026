# =============================================================================
# joins.R — Left joins that cannot silently go wrong
# =============================================================================
# DISTRICT-AGNOSTIC.
#
# Every join in the old scripts was followed by a hand-written "did that work?"
# block. This does it automatically:
#
#   - stops if the right-hand table has duplicate keys (the usual cause of
#     silent row inflation)
#   - stops if the row count changed, unless expansion is explicitly allowed
#   - reports the match rate as an info check
#   - optionally stops if the match rate is below a threshold
#
# Usage:
#   stu_tr <- join_checked(transcript, student_year,
#                          by = c("D_stu_id", "D_year"),
#                          check = "Student demographics onto transcript")
# =============================================================================

#' Left join with diagnostics.
#'
#' @param x,y             Data frames. y is joined onto x.
#' @param by              Join key(s). Named vector allowed, as in dplyr.
#' @param check           Label used in the check log.
#' @param allow_expansion Permit y to have duplicate keys and x to grow.
#'                        Default FALSE, which is almost always what you want.
#' @param min_match_pct   If set, stop when the share of x rows finding a match
#'                        falls below this. Use for the course coding join.
#' @param report_unmatched Print a sample of unmatched keys.
join_checked <- function(x, y, by, check,
                         allow_expansion  = FALSE,
                         min_match_pct    = NULL,
                         report_unmatched = TRUE,
                         n_show           = 15) {

  x_keys <- if (is.null(names(by))) by else ifelse(names(by) == "", by, names(by))
  y_keys <- unname(by)

  missing_x <- setdiff(x_keys, names(x))
  missing_y <- setdiff(y_keys, names(y))
  if (length(missing_x) || length(missing_y)) {
    chk_stop(paste0(check, " — join keys present"), FALSE,
             detail = paste0(
               if (length(missing_x)) paste("missing in left:",  paste(missing_x, collapse = ", ")),
               if (length(missing_y)) paste(" missing in right:", paste(missing_y, collapse = ", "))))
  }

  # --- right-hand key uniqueness -------------------------------------------
  y_dupes <- y |>
    dplyr::count(dplyr::across(dplyr::all_of(y_keys)), name = "n_rows") |>
    dplyr::filter(n_rows > 1)

  if (!allow_expansion) {
    chk_stop(paste0(check, " — right side has unique keys"),
             nrow(y_dupes) == 0,
             detail = if (nrow(y_dupes) > 0)
               paste0(nrow(y_dupes), " duplicated key(s) in the right-hand table; ",
                      "this would multiply rows on join"),
             offenders = y_dupes)
  }

  n_before <- nrow(x)
  out <- dplyr::left_join(x, y, by = by)
  n_after <- nrow(out)

  if (!allow_expansion) {
    chk_stop(paste0(check, " — row count unchanged"),
             n_before == n_after,
             value  = paste0(format(n_after, big.mark = ","), " rows"),
             detail = if (n_before != n_after)
               paste0("before = ", format(n_before, big.mark = ","),
                      ", after = ", format(n_after, big.mark = ",")))
  } else {
    chk_info(paste0(check, " — rows after expansion"),
             paste0(format(n_before, big.mark = ","), " -> ",
                    format(n_after, big.mark = ",")))
  }

  # --- match rate -----------------------------------------------------------
  # A row matched if it picked up a non-key column from y that is not NA.
  probe <- setdiff(names(y), y_keys)
  if (length(probe) > 0) {
    probe_col <- probe[1]
    probe_out <- if (probe_col %in% names(out)) probe_col else paste0(probe_col, ".y")
    matched   <- if (probe_out %in% names(out)) !is.na(out[[probe_out]]) else rep(NA, n_after)
    n_matched <- sum(matched, na.rm = TRUE)
    pct       <- round(100 * n_matched / n_after, 2)

    chk_info(paste0(check, " — match rate"),
             paste0(pct, "% (", format(n_matched, big.mark = ","), " of ",
                    format(n_after, big.mark = ","), " rows)"))

    if (!is.null(min_match_pct)) {
      unmatched_keys <- out[!matched, x_keys, drop = FALSE] |> dplyr::distinct()
      chk_stop(paste0(check, " — match rate at or above ", min_match_pct, "%"),
               pct >= min_match_pct,
               value  = paste0(pct, "%"),
               detail = paste0(format(n_after - n_matched, big.mark = ","),
                               " unmatched row(s), ",
                               format(nrow(unmatched_keys), big.mark = ","),
                               " distinct unmatched key(s)"),
               offenders = unmatched_keys)
    } else if (report_unmatched && n_matched < n_after) {
      unmatched_keys <- out[!matched, x_keys, drop = FALSE] |> dplyr::distinct()
      chk_info(paste0(check, " — unmatched keys"),
               nrow(unmatched_keys),
               data = utils::head(unmatched_keys, n_show))
    }
  }

  out
}
