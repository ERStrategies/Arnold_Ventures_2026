# =============================================================================
# 00_unification_helpers.R   —   UNIFICATION (faithful port of 07).
#
# Brings the resolved DB rows (exploded, per meeting-slot) back together with the
# non-DB rows (un-exploded, one per record), matched on H_row_id_before_explosions.
# DB rows keep their resolved/exploded values; non-DB rows keep their originals.
# Then splits the shared exclusion into class-size vs teacher-load, folding in the
# load-off teachers.
#
# Grain: mixed on purpose — DB rows per-slot, non-DB rows per-class. The rollups
# group by (..., D_period, C_class_id) and aggregate, so both land correctly.
#
# Requires: resolved frame (from resolve_double_bookings), the UN-exploded course
# data (with the SAME H_row_id_before_explosions), and the load-off table.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr) })

unify_resolution <- function(resolved, unexploded, loadoff = NULL,
                             load_exclude_col = "C_class_size_and_teacher_load_exclude") {
  if (!"H_row_id_before_explosions" %in% names(unexploded))
    stop("unexploded data needs H_row_id_before_explosions (create it before exploding).")

  # 1. non-DB rows = un-exploded rows NOT represented in the resolved data
  matched <- resolved |> distinct(H_row_id_before_explosions) |> mutate(.matched = TRUE)
  non_db  <- unexploded |>
    left_join(matched, by = "H_row_id_before_explosions") |>
    filter(is.na(.matched)) |> select(-.matched)

  # 2. append (coerce shared slot keys to character so bind_rows aligns cleanly)
  chr <- function(d) {
    for (c in intersect(c("D_term", "D_period", "D_rotation"), names(d)))
      d[[c]] <- as.character(d[[c]])
    d
  }
  unified <- bind_rows(chr(resolved), chr(non_db))

  # 3. unify: resolved/exploded value if present, else original.
  #    Expression districts have no D_period/D_rotation on the un-exploded data —
  #    the exploded slot columns carry that info, so only coalesce what exists and
  #    always surface the exploded period/rotation/term.
  nm <- names(unified)
  unified <- unified |>
    mutate(
      D_term = if ("D_term" %in% nm)
                 ifelse(is.na(C_term_exploded), D_term, C_term_exploded) else C_term_exploded,
      D_period = if ("D_period" %in% nm)
                   ifelse(is.na(C_period_exploded), D_period, C_period_exploded) else C_period_exploded,
      D_rotation = if ("D_rotation" %in% nm)
                     ifelse(is.na(C_rotation_exploded), D_rotation, C_rotation_exploded) else C_rotation_exploded,
      M_class_weight = ifelse(is.na(M_class_weight_post_student),
                              M_cls_class_weight, M_class_weight_post_student),
      C_class_id = ifelse(is.na(C_class_id_post_teachers), C_class_id, C_class_id_post_teachers))

  # 4. split the shared exclusion: teacher-load excludes = original excludes PLUS
  #    the load-off teachers; class-size exclude = the original field, renamed.
  loadoff_teachers <- if (!is.null(loadoff)) unique(loadoff$D_employee_id[loadoff$load_off]) else integer(0)
  unified$C_teacher_load_exclude <- ifelse(
    unified[[load_exclude_col]] == "Exclude" | unified$D_employee_id %in% loadoff_teachers,
    "Exclude", "Include")
  names(unified)[names(unified) == load_exclude_col] <- "C_class_size_exclude"
  unified
}

# Verification helper (mirrors 07's row-accounting checks).
unification_summary <- function(unified, resolved, unexploded) {
  n_matched <- unexploded |>
    semi_join(distinct(resolved, H_row_id_before_explosions), by = "H_row_id_before_explosions") |>
    nrow()
  data.frame(
    check = c("resolved (DB, exploded) rows", "unexploded records matched (went to resolution)",
              "non-DB rows appended (un-exploded)", "unified rows",
              "= resolved + non-DB?",
              "teacher-load excludes", "class-size excludes"),
    value = c(nrow(resolved), n_matched, nrow(unexploded) - n_matched, nrow(unified),
              nrow(unified) == nrow(resolved) + (nrow(unexploded) - n_matched),
              sum(unified$C_teacher_load_exclude == "Exclude"),
              sum(unified$C_class_size_exclude == "Exclude")))
}