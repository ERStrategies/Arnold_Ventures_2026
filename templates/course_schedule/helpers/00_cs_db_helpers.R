# =============================================================================
# 00_db_helpers.R
# Class id + term-aware conflict keys + three-type double-booking SIZING.
# Depends on 00_expression_helpers.R (parse_expression / expr_spec).
#
# A conflict key = "<period-atom>@<rotation>|<term-slot>". Two records conflict
# if their conflict-key sets intersect. Term expands to its finest slots so an
# FY class and an S1 class at the same meeting collide, but S1 and S2 do not.
#
# The class distinguisher for sizing is C_class_id (built from the config
# recipe, which includes section) — two different classes = different ids;
# the same class twice = a duplicate, not a booking. Verified in Python.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# C_class_id from the config recipe (uses whichever recipe cols are present)
build_class_id <- function(df, recipe, sep = "_") {
  cols <- recipe[recipe %in% names(df)]
  if (!length(cols)) return(rep(NA_character_, nrow(df)))
  do.call(paste, c(lapply(cols, function(c) as.character(df[[c]])), sep = sep))
}

# finest term slots for a term value (e.g. 3500 -> 3501,3502)
term_expands_spec <- function(config) {
  lapply(config$weights$term_expands, function(x) unlist(x, use.names = FALSE))
}
term_slots <- function(term, term_expands) {
  s <- term_expands[[as.character(term)]]
  if (is.null(s)) as.character(term) else as.character(s)
}

# conflict keys for one record (meeting set x term-slot set)
conflict_keys <- function(expr, term, spec, term_expands) {
  m <- parse_expression(expr, spec)
  if (!length(m)) return(character(0))
  ts <- term_slots(term, term_expands)
  as.vector(outer(m, ts, function(a, b) paste0(a, "|", b)))
}

# long form: one row per (record, conflict key). Keys computed once per distinct
# (expression, term) combo, then joined — fast on large data.
explode_conflicts <- function(df, spec, term_expands,
                              expr_col = "D_expression", term_col = "D_term") {
  df <- as.data.frame(df)   # robust to data.table's non-standard `[, j]`
  combos <- unique(df[, c(expr_col, term_col), drop = FALSE])
  combos[[".ckey"]] <- Map(function(e, t) conflict_keys(e, t, spec, term_expands),
                           combos[[expr_col]], combos[[term_col]])
  df |>
    left_join(combos, by = c(expr_col, term_col)) |>
    unnest_longer(".ckey", values_to = "M_conflict_key")
}

# entities (student or teacher) double-booked: >1 distinct class at a key
db_flagged <- function(long, entity_col, class_col = "C_class_id") {
  long |>
    filter(!is.na(M_conflict_key)) |>
    group_by(.data[[entity_col]], M_conflict_key) |>
    summarise(n_classes = n_distinct(.data[[class_col]]), .groups = "drop") |>
    filter(n_classes > 1)
}
