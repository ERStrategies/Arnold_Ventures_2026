# =============================================================================
# explore_helpers.R
# Config-FREE profiling for the Tier 0 "Explore" pass.
#
# Looks at TRULY RAW data (original headers, nothing mapped) so you can author
# the column_map + prep recipe. Depends on NO config and NO ERS D_ names.
# Nothing here decides anything; it only describes what's in the file.
# =============================================================================

suppressPackageStartupMessages({
  library(gt)
})

# ---- One row per column: type, distinctness, missingness, range, values -----
# For small columns (<= max_full distinct) the FULL value set is shown inline,
# so this single table replaces the old separate low-cardinality dumps.
# min/max are computed for numeric / date columns (and numeric-looking strings).
explore_profile <- function(df, max_full = 12, n_sample = 6) {
  n <- nrow(df)
  rows <- lapply(names(df), function(nm) {
    x  <- df[[nm]]
    m  <- is.na(x)
    if (is.character(x)) m <- m | trimws(x) %in% c("", "NA", "MISSING", "NULL")
    xv <- x[!m]
    u  <- unique(xv)
    nd <- length(u)

    rng <- tryCatch({
      if (is.numeric(xv) || inherits(xv, "Date") || inherits(xv, "POSIXct")) {
        c(min(xv), max(xv))
      } else {
        num <- suppressWarnings(as.numeric(as.character(xv)))
        if (length(num) && !any(is.na(num))) c(min(num), max(num)) else c(NA, NA)
      }
    }, error = function(e) c(NA, NA))

    samp <- if (nd == 0) ""
            else if (nd <= max_full) paste(as.character(u), collapse = ", ")
            else paste0(paste(as.character(utils::head(u, n_sample)), collapse = ", "), ", \u2026")

    data.frame(
      column      = nm,
      type        = class(x)[1],
      n_distinct  = nd,
      n_missing   = sum(m),
      pct_missing = round(100 * sum(m) / n, 1),
      min         = as.character(rng[1]),
      max         = as.character(rng[2]),
      values      = samp,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

# ---- Full value set (with counts) for one column ----------------------------
# Still useful for peeking at HIGH-cardinality columns (expression, course num).
explore_value_counts <- function(df, col, top = NULL) {
  x <- df[[col]]
  t <- as.data.frame(table(value = x, useNA = "ifany"), stringsAsFactors = FALSE)
  names(t) <- c(col, "n")
  t <- t[order(-t$n), , drop = FALSE]
  rownames(t) <- NULL
  if (!is.null(top)) t <- utils::head(t, top)
  t
}

# ---- Shared table styling (same visual family as the review artifact) -------
explore_gt <- function(df, title = NULL) {
  g <- gt(df)
  if (!is.null(title)) g <- tab_header(g, title = md(paste0("**", title, "**")))
  g |>
    tab_options(table.font.size = px(12), data_row.padding = px(4),
                column_labels.font.weight = "bold", table.width = pct(100),
                heading.align = "left", heading.title.font.size = px(13),
                table.border.top.style = "none") |>
    opt_table_font(font = "system-ui")
}

# ---- Student snapshot filtering ---------------------------------------------
# For districts that provide longitudinal enrollment data (not a single
# snapshot), this function flags each row as "Include" or "Exclude" based on
# whether the student was enrolled on the configured snapshot date for that term.
#
# A student is included if:
#   D_stu_enter_date <= snapshot_date AND D_stu_exit_date >= snapshot_date
#
# Triggered in the prep chunk when config$intake$enrollment_type != "snapshot".
# Snapshot dates are set per-term in config$intake$snapshot_dates.
apply_stu_snapshot <- function(df, config) {
  snapshot_dates <- lapply(
    config$intake$snapshot_dates,
    function(d) as.Date(d)
  )

  df |>
    mutate(C_stu_snapshot = {
      term     <- as.character(D_term)
      snap     <- as.Date(unlist(snapshot_dates)[term])
      entered  <- as.Date(D_stu_enter_date)
      exited   <- as.Date(D_stu_exit_date)
      ifelse(
        !is.na(snap) & !is.na(entered) & !is.na(exited) &
          entered <= snap & exited >= snap,
        "Include", "Exclude"
      )
    })
}

# Render a named list of frames as stacked tables.
explore_render <- function(named_frames, prefix = "Values \u00B7 ") {
  htmltools::tagList(lapply(names(named_frames), function(nm) {
    htmltools::HTML(as_raw_html(explore_gt(named_frames[[nm]], paste0(prefix, nm))))
  }))
}
