# =============================================================================
# 00_review_helpers.R
# Rendering engine for the stage review docs (02, 03, ...). No content of its
# own — the toolkit each stage sources. A stage = a title, a question, and a
# list of checks. Each check is a DECISION and may own EVIDENCE (summary tables).
#
# Design tenets:
#   - Compute when practical  : a status is shown ONLY when it can be calculated
#                               from the data; otherwise the check is evidence-
#                               only (status = "none" -> no badge).
#   - Detect, don't depend    : checks inspect the data for what's present and
#                               degrade gracefully; a missing input never jams.
#   - Flag, don't drop        : a status reports; it never deletes or halts.
#   - Config is the template   : district knobs live in the YAML config.
# =============================================================================

suppressPackageStartupMessages({
  library(gt)
  library(glue)
  library(htmltools)
})

# =============================================================================
# Shared utility — available to all scripts that source explore_helpers
tr_safe <- function(expr, otherwise = NA) {
  tryCatch(expr, error = function(e) otherwise)
}

# ---- Status vocabulary ------------------------------------------------------
# "none" = no computable status -> render the check with no badge (evidence-only)
tr_status_levels <- c("ok", "flag", "missing", "todo", "na", "none")

tr_status_meta <- list(
  ok      = list(label = "OK",        icon = "\u2713"),
  flag    = list(label = "Flag",      icon = "\u26A0"),
  missing = list(label = "Missing",   icon = "\u2717"),
  todo    = list(label = "To decide", icon = "\u270E"),
  na      = list(label = "N/A",       icon = "\u2014")
)

tr_badge_colors <- list(
  ok      = list(bg = "#ecfdf3", fg = "#067647"),
  flag    = list(bg = "#fffaeb", fg = "#b54708"),
  missing = list(bg = "#fef3f2", fg = "#b42318"),
  todo    = list(bg = "#eff8ff", fg = "#175cd3"),
  na      = list(bg = "#f2f4f7", fg = "#667085")
)

# tr_safe() lives in 00_explore_helpers.R — sourced before this file.

# ---- Status helpers ---------------------------------------------------------
# Compute a status from a numeric value + threshold; NA/unusable -> "none"
# (evidence-only) rather than a fake badge.
tr_threshold_status <- function(value, threshold, ok_when = c("below", "above")) {
  ok_when <- match.arg(ok_when)
  if (is.null(value) || length(value) != 1 || is.na(value)) return("none")
  ok <- if (ok_when == "below") value <= threshold else value >= threshold
  if (isTRUE(ok)) "ok" else "flag"
}

tr_presence_status <- function(present, partial = FALSE) {
  if (isTRUE(partial)) return("flag")
  if (isTRUE(present)) "ok" else "missing"
}

# % of a flag column that is "true", robust to Y/N, 1/0, TRUE/FALSE encodings.
tr_pct_true <- function(x) {
  round(100 * mean(x %in% c(1, "1", "Y", "y", "TRUE", TRUE), na.rm = TRUE), 1)
}

# ---- Constructors -----------------------------------------------------------
# evidence: NULL, OR a function returning a data.frame, OR a named list of them.
#           Deferred so it evaluates at render time and can be tr_safe()'d.
tr_check <- function(id, question,
                     good     = NA_character_,
                     value    = NULL,
                     status   = "none",
                     records  = NA_character_,
                     note     = NA_character_,
                     evidence = NULL) {
  if (!status %in% tr_status_levels) {
    stop(glue::glue("Unknown status '{status}'. Use one of: ",
              paste(tr_status_levels, collapse = ", ")))
  }
  structure(
    list(id = id, question = question, good = good, value = value,
         status = status, records = records, note = note, evidence = evidence),
    class = "tr_check"
  )
}

tr_stage <- function(number, title, question, checks, enabled = TRUE) {
  structure(
    list(number = number, title = title, question = question,
         checks = checks, enabled = enabled),
    class = "tr_stage"
  )
}

# ---- Formatters -------------------------------------------------------------
tr_fmt_value <- function(v) {
  if (is.null(v) || (length(v) == 1 && is.na(v))) return("\u2014")
  if (is.numeric(v) && length(v) == 1) return(format(round(v, 2), big.mark = ","))
  paste(as.character(v), collapse = ", ")
}

.tr_lbl <- "font-weight:600;font-size:10px;text-transform:uppercase;letter-spacing:.04em;"

tr_decision_block <- function(chk, disabled = FALSE) {
  status <- if (disabled) "na" else chk$status

  # status = "none" -> no badge (evidence-only check)
  badge <- if (identical(status, "none")) NULL else {
    col <- tr_badge_colors[[status]]; meta <- tr_status_meta[[status]]
    htmltools::tags$span(
      style = paste0("display:inline-block;background:", col$bg, ";color:", col$fg,
                     ";font-weight:500;font-size:11px;padding:2px 8px;border-radius:999px;",
                     "white-space:nowrap;flex:none;"),
      paste(meta$icon, meta$label))
  }

  value <- if (!is.null(chk$value) && !(length(chk$value) == 1 && is.na(chk$value)))
    htmltools::tags$span(style = "color:#475467;font-size:12px;", paste0("  \u2014  ", tr_fmt_value(chk$value)))
  else NULL

  good <- if (!is.na(chk$good) && nzchar(chk$good))
    htmltools::tags$div(style = "color:#667085;font-size:12px;margin-top:3px;",
             htmltools::tags$span(style = paste0(.tr_lbl, "color:#98a2b3;"), "good \u00B7 "), chk$good)
  else NULL

  # only render the "recorded" line when there's something recorded
  recorded <- if (!is.na(chk$records) && nzchar(chk$records))
    htmltools::tags$div(style = "font-size:13px;margin-top:4px;color:#1d2939;",
             htmltools::tags$span(style = paste0(.tr_lbl, "color:#667085;"), "recorded \u00B7 "),
             tr_fmt_value(chk$records))
  else NULL

  note <- if (!is.na(chk$note) && nzchar(chk$note))
    htmltools::tags$div(style = paste0("margin-top:5px;background:#fff7ed;border-left:3px solid #f79009;",
                            "padding:4px 8px;color:#7a4708;font-size:12px;"),
             htmltools::tags$span(style = "font-weight:600;", "note \u00B7 "), chk$note)
  else NULL

  htmltools::tags$div(style = "padding:11px 0;border-bottom:1px solid #eef1f4;",
    htmltools::tags$div(style = "display:flex;align-items:baseline;gap:8px;",
      htmltools::tags$span(style = "color:#98a2b3;font-size:12px;min-width:28px;flex:none;", chk$id),
      htmltools::tags$span(style = "font-weight:500;color:#1d2939;flex:1;", chk$question, value),
      badge),
    good, recorded, note)
}

.tr_evidence_gt <- function(df) {
  gt::gt(df) |>
    gt::tab_options(table.font.size = gt::px(10), data_row.padding = gt::px(4),
                column_labels.font.size = gt::px(10),
                column_labels.font.weight = "bold", table.width = gt::pct(100),
                table.border.top.style = "none") |>
  gt::tab_style(
    style = gt::cell_text(size = gt::px(10)),
    locations = gt::cells_body()) |>
    gt::opt_table_font(font = "system-ui")
}

tr_render_evidence <- function(chk) {
  if (is.null(chk$evidence)) return(NULL)
  res <- tr_safe(chk$evidence(), otherwise = NULL)

  if (is.data.frame(res)) res <- setNames(list(res), glue::glue("Evidence \u00B7 {chk$id}"))

  if (is.null(res) || !is.list(res) || length(res) == 0) {
    return(htmltools::tags$div(style = "margin:2px 0 10px 36px;color:#98a2b3;font-size:12px;font-style:italic;",
                    glue::glue("Evidence \u00B7 {chk$id} \u2014 data not loaded")))
  }

  blocks <- lapply(seq_along(res), function(i) {
    cap <- names(res)[i]; df <- res[[i]]
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(htmltools::tags$div(style = "color:#98a2b3;font-size:12px;font-style:italic;margin-bottom:8px;",
                      glue::glue("{cap} \u2014 no rows")))
    }
    htmltools::tags$div(style = "margin-bottom:12px;",
      htmltools::tags$div(style = paste0(.tr_lbl, "color:#667085;margin-bottom:4px;"), cap),
      htmltools::HTML(gt::as_raw_html(.tr_evidence_gt(df))))
  })

  htmltools::tags$div(style = "margin:8px 0 10px 36px;", blocks)
}

# ---- The one renderer -------------------------------------------------------
render_stage <- function(stage) {
  disabled <- !isTRUE(stage$enabled)

  st <- vapply(stage$checks, function(c) if (disabled) "na" else c$status, character(1))
  counts <- table(factor(st, levels = tr_status_levels))
  labs <- vapply(setdiff(tr_status_levels, "none"), function(s) {   # "none" isn't a status to tally
    n <- as.integer(counts[[s]])
    if (n == 0) NA_character_ else glue::glue("{n} {tr_status_meta[[s]]$label}")
  }, character(1))
  summ <- paste(labs[!is.na(labs)], collapse = "  \u00B7  ")

  q <- if (disabled) glue::glue("{stage$question}  \u2014  stage not run (entered downstream)")
       else stage$question

  header <- htmltools::tags$div(
    style = paste0("background:", if (disabled) "#8794a3" else "#0f2a43",
                   ";color:#fff;padding:12px 16px;"),
    htmltools::tags$div(style = "display:flex;align-items:baseline;gap:8px;",
      htmltools::tags$span(style = "font-weight:700;font-size:12px;background:rgba(255,255,255,.15);padding:2px 8px;border-radius:999px;",
                stage$number),
      htmltools::tags$span(style = "font-weight:600;font-size:16px;", stage$title)),
    htmltools::tags$div(style = "margin-top:5px;font-size:13px;opacity:.9;font-style:italic;", q),
    if (nzchar(summ))
      htmltools::tags$div(style = "margin-top:7px;font-size:12px;opacity:.95;",
               htmltools::tags$span(style = "font-weight:600;", "Status: "), summ)
    else NULL)

  body_blocks <- lapply(stage$checks, function(chk) {
    htmltools::tagList(tr_decision_block(chk, disabled),
            if (!disabled) tr_render_evidence(chk) else NULL)
  })

  htmltools::tags$section(
    style = "border:1px solid #e3e6ea;border-radius:10px;overflow:hidden;margin:1.4rem 0;font-family:system-ui,-apple-system,Segoe UI,Roboto,sans-serif;",
    header,
    htmltools::tags$div(style = "padding:2px 16px 10px 16px;background:#fff;", body_blocks))
}
