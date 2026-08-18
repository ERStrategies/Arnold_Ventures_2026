# =============================================================================
# 00_prep.R — Preparation steps common to every pipeline
# =============================================================================
# PIPELINE-AGNOSTIC. Sourced by both the transcript and course-schedule
# pipelines. Everything here takes its content from config.
# =============================================================================

# --- Row filters ---------------------------------------------------------------

#' Apply the config's scope rules, reporting what each one costs.
#'
#' The old scripts filtered without saying how many rows went, which makes an
#' unintended filter invisible. Every rule here reports its own attrition.
apply_scope <- function(data, scope, id_col = NULL, sum_cols = NULL) {
  n0 <- nrow(data)
  ids0 <- if (!is.null(id_col)) dplyr::n_distinct(data[[id_col]]) else NA
  sums0 <- if (!is.null(sum_cols))
    vapply(sum_cols, function(c) sum(data[[c]], na.rm = TRUE), numeric(1)) else NULL

  for (col in scope$drop_when_null %||% character()) {
    if (!col %in% names(data)) {
      chk_stop(paste0("Scope column exists: ", col), FALSE)
    }
    before <- nrow(data)
    data <- data |> dplyr::filter(!is.na(.data[[col]]),
                                  trimws(as.character(.data[[col]])) != "")
    chk_info(paste0("Scope — drop null ", col),
             paste0(format(before - nrow(data), big.mark = ","), " row(s) dropped"))
  }

  for (expr in scope$filters %||% character()) {
    before <- nrow(data)
    data <- data |> dplyr::filter(!!rlang::parse_expr(expr))
    chk_info(paste0("Scope — filter ", expr),
             paste0(format(before - nrow(data), big.mark = ","), " row(s) dropped"))
  }

  chk_info("Scope — total retained",
           paste0(format(nrow(data), big.mark = ","), " of ",
                  format(n0, big.mark = ","), " rows (",
                  round(100 * nrow(data) / n0, 1), "%)"))
  if (!is.null(id_col)) {
    chk_info("Scope — distinct ids retained",
             paste0(format(dplyr::n_distinct(data[[id_col]]), big.mark = ","),
                    " of ", format(ids0, big.mark = ",")))
  }
  # Dropping a row removes whatever it was carrying. At scale that silently
  # changes totals, so report what each summed column lost.
  if (!is.null(sum_cols)) {
    sums1 <- vapply(sum_cols, function(c) sum(data[[c]], na.rm = TRUE), numeric(1))
    chk_info("Scope — analytic weight removed", "see table",
             data = tibble::tibble(column = sum_cols,
                                   before = sums0,
                                   after  = sums1,
                                   removed = sums0 - sums1))
  }
  data
}
