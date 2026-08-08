# =============================================================================
# 00_consolidation_helpers.R   —   TEACHER-STAGE consolidation (part of stage 3).
#
# A teacher's sections that overlap in time are really one class taught to sub-
# groups (push-in, Band I/II, Alg/Alg-H, SPED ELA/Math). We DETECT them as
# distinct (section-based ids) then CONSOLIDATE:
#   * graph: within a teacher, an edge connects two sections that share a slot
#            (term x period x rotation);
#   * connected components (union-find) = the groups to merge — handles chains
#            (A-B overlap, B-C overlap, A & C don't touch -> all one class);
#   * union all their meetings -> serialize_meetings() -> merged expression;
#   * new id = CONS_<location>_<merged-expr>_<term>  (filter with ^CONS_).
#
# GUARDRAIL: this only maps sections -> a consolidated id + expression. It never
# fabricates a (student, meeting) row, so no student's time inflates to the union.
#
# Depends on 00_expression_helpers.R (serialize_meetings). Verified in Python.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr) })

# Map each section (C_class_id_original) to its consolidation group + merged
# expression + CONS_ id. Sections in a 1-section component are left untouched.
consolidate_sections <- function(flagged) {
  # one row per (section, teacher, slot)
  ss <- flagged |>
    distinct(C_class_id_original, D_employee_id, D_location_id, D_term,
             C_term_exploded, C_period_exploded, C_rotation_exploded)

  nodes  <- unique(ss$C_class_id_original)
  idx    <- setNames(seq_along(nodes), nodes)
  parent <- seq_along(nodes)
  find  <- function(x) { while (parent[x] != x) x <- parent[x]; x }
  unite <- function(a, b) { ra <- find(a); rb <- find(b); if (ra != rb) parent[rb] <<- ra }

  # link sections sharing a (teacher, term-slot, period, rotation)
  ss$.slotkey <- paste(ss$D_employee_id, ss$C_term_exploded,
                       ss$C_period_exploded, ss$C_rotation_exploded, sep = "|")
  for (grp in split(ss$C_class_id_original, ss$.slotkey)) {
    u <- unique(grp)
    if (length(u) > 1) for (b in u[-1]) unite(idx[[u[1]]], idx[[b]])
  }

  sec_comp <- data.frame(C_class_id_original = nodes,
                         .comp = vapply(seq_along(nodes), find, integer(1)),
                         stringsAsFactors = FALSE)

  # meetings per section -> per component union -> merged expression
  sec_mtg <- flagged |>
    distinct(C_class_id_original, C_period_exploded, C_rotation_exploded) |>
    mutate(.m = paste0(C_period_exploded, "@", C_rotation_exploded)) |>
    left_join(sec_comp, by = "C_class_id_original")

  comp <- sec_mtg |>
    group_by(.comp) |>
    summarise(n_sections  = n_distinct(C_class_id_original),
              merged_expr = serialize_meetings(unique(.m)), .groups = "drop")

  # anchor (location/teacher/term) per component for the CONS_ id.
  # TEACHER is part of the id here: one component is one teacher by construction,
  # so two teachers whose merges happen to share an expression must NOT collide.
  anchor <- flagged |>
    distinct(C_class_id_original, D_location_id, D_employee_id, D_term) |>
    left_join(sec_comp, by = "C_class_id_original") |>
    group_by(.comp) |>
    summarise(D_location_id = first(D_location_id),
              D_employee_id = first(D_employee_id),
              D_term = first(D_term), .groups = "drop")

  comp <- comp |> left_join(anchor, by = ".comp") |>
    mutate(C_class_id_consolidated = if_else(
      n_sections > 1,
      paste0("CONS_", D_location_id, "_", D_employee_id, "_",
             gsub(", ", "+", merged_expr), "_", D_term),
      NA_character_))

  # GUARD: a CONS_ id must map to exactly one component. If two DIFFERENT
  # components ever produced the same id, they'd silently fuse (the multi-teacher
  # bug, one level deeper) — so fail loudly instead.
  dup <- comp |> filter(!is.na(C_class_id_consolidated)) |>
    count(C_class_id_consolidated) |> filter(n > 1)
  if (nrow(dup) > 0)
    stop("Consolidation id collision (two components share an id): ",
         paste(dup$C_class_id_consolidated, collapse = ", "))

  # section -> (component, merged expr, consolidated id or original if untouched)
  sec_comp |>
    left_join(comp, by = ".comp") |>
    mutate(C_class_id_resolved = coalesce(C_class_id_consolidated, C_class_id_original),
           is_consolidated = !is.na(C_class_id_consolidated)) |>
    select(C_class_id_original, .comp, n_sections, merged_expr,
           C_class_id_consolidated, C_class_id_resolved, is_consolidated)
}

# Component-size distribution + example merges (what you wanted to see first).
consolidation_summary <- function(mapping) {
  comp <- mapping |> distinct(.comp, n_sections, merged_expr, C_class_id_consolidated)
  dist <- comp |> count(n_sections, name = "n_components") |> arrange(n_sections) |>
    mutate(kind = case_when(n_sections == 1 ~ "untouched",
                            n_sections == 2 ~ "pair",
                            TRUE ~ "chain (3+)"))
  examples <- comp |> filter(n_sections > 1) |> arrange(desc(n_sections)) |>
    slice_head(n = 10) |>
    transmute(n_sections, merged_expr, C_class_id_consolidated)
  list("Component-size distribution" = dist,
       "Example consolidations (largest first)" = examples,
       "Totals" = data.frame(
         sections_total       = nrow(mapping),
         sections_consolidated = sum(mapping$is_consolidated),
         consolidated_classes  = dplyr::n_distinct(mapping$C_class_id_consolidated[mapping$is_consolidated])))
}


# --- apply the mapping -------------------------------------------------------
# Re-id consolidated sections to their CONS_ id and set the class-level merged
# expression. GUARDRAIL: this only re-labels existing rows via a join — it never
# adds a (student, meeting) row, so a student stays at exactly their own meetings
# and no one's time inflates to the union. Consolidated rows are identifiable by
# the "CONS_" prefix on C_class_id_post_teachers.
apply_consolidation <- function(flagged, mapping) {
  m <- mapping |>
    filter(is_consolidated) |>
    select(C_class_id_original, C_class_id_consolidated, merged_expr)
  flagged |>
    left_join(m, by = "C_class_id_original") |>
    mutate(
      C_class_id_post_teachers = coalesce(C_class_id_consolidated, C_class_id_original),
      C_class_expression       = coalesce(
        merged_expr,
        if ("D_expression" %in% names(flagged)) D_expression
        else if (all(c("D_period", "D_rotation") %in% names(flagged))) paste0(D_period, "(", D_rotation, ")")
        else NA_character_)) |>
    select(-C_class_id_consolidated, -merged_expr)
}

# --- teacher load-off --------------------------------------------------------
# The class still consolidates (for class size / student time), but a teacher
# whose merged class exceeds consolidate_max_size AND isn't an exempt subject
# gets flagged for teacher-load exclusion (resolved in unification). Exempt if
# ALL of the merged class's sections are in an exempt subject area (e.g. a large
# but legitimate music/PE block stays consolidated and counted).
teacher_loadoff <- function(applied, config,
                            subject_col = "C_course_subject_area") {
  td       <- config$teacher_db
  max_size <- if (!is.null(td$consolidate_max_size)) td$consolidate_max_size else 35
  exempt   <- if (!is.null(td$exempt_subjects)) td$exempt_subjects else character(0)
  applied |>
    filter(startsWith(C_class_id_post_teachers, "CONS_")) |>
    group_by(C_class_id_post_teachers, D_employee_id) |>
    summarise(n_students = n_distinct(D_stu_id),
              all_exempt = all(.data[[subject_col]] %in% exempt),
              .groups = "drop") |>
    mutate(load_off = n_students > max_size & !all_exempt)
}
