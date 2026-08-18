# =============================================================================
# 00_resolution_helpers.R   —   the resolution STAGES (faithful port of 06).
# Order: class -> teacher (consolidation, in 00_consolidation_helpers.R) -> student.
# Each stage carries a weight snapshot forward:
#   M_class_weight_adj_same_course  (from flagging)
#     -> M_class_weight_post_class      (class stage, below)
#     -> M_class_weight_post_teachers   (teacher stage = consolidation re-id)
#     -> M_class_weight_post_student    (student stage, below)
# =============================================================================

suppressPackageStartupMessages({ library(dplyr) })

# --- CLASS STAGE -------------------------------------------------------------
# Split a student's weight across their 100%-DB classes, but ONLY when the
# student's ENTIRE TPR is 100%-DB classes. A mixed TPR reverts (count = 1) and is
# handled later by the student stage. A class is "100%-DB" when ALL its students
# are double-booked. Verified in Python.
resolve_class_stage <- function(flagged) {
  # 1. 100%-DB per class-slot: every student in it is double-booked
  cls100 <- flagged |>
    group_by(C_class_id) |>
    summarise(H_db_100p_class = as.integer(mean(H_db_student_row) == 1), .groups = "drop")
  d <- left_join(flagged, cls100, by = "C_class_id")

  # 2. per student-TPR: is the WHOLE tpr 100%-DB? if so, split count = n such classes
  tpr <- d |>
    group_by(D_stu_id, C_term_exploded, C_period_exploded, C_rotation_exploded) |>
    summarise(n_classes = n_distinct(C_class_id),
              n_100p    = n_distinct(C_class_id[H_db_100p_class == 1]),
              .groups = "drop") |>
    mutate(H_tpr_100p_percent = n_100p / n_classes,
           H_student_count_of_100p_classes =
             if_else(H_tpr_100p_percent == 1 & n_100p > 0, as.integer(n_100p), 1L))

  d |>
    left_join(tpr |> select(D_stu_id, C_term_exploded, C_period_exploded, C_rotation_exploded,
                            H_tpr_100p_percent, H_student_count_of_100p_classes),
              by = c("D_stu_id", "C_term_exploded", "C_period_exploded", "C_rotation_exploded")) |>
    mutate(M_class_weight_post_class =
             M_class_weight_adj_same_course / H_student_count_of_100p_classes)
}


# --- STUDENT STAGE -----------------------------------------------------------
# Final 1/N split on what remains, PLUS the class-size hook.
#   WEIGHT: students the class stage already handled (H_tpr_100p_percent == 1) are
#           NOT re-split (adjuster = 1); everyone else is split by the number of
#           distinct POST-TEACHER classes at their TPR (consolidated classes count
#           once). -> M_class_weight_post_student.
#   SIZE  : M_student_count_adjusted = 1 / (TOTAL classes the student is split
#           across, either stage) -> a double-booked student is a fraction of a
#           head in each class. This is the step that was orphaned in 08 rollups,
#           now living with the resolution that produces the N.
resolve_student_stage <- function(applied) {
  npt <- applied |>
    group_by(D_stu_id, C_term_exploded, C_period_exploded, C_rotation_exploded) |>
    summarise(N_pt = n_distinct(C_class_id_post_teachers), .groups = "drop")

  applied |>
    left_join(npt, by = c("D_stu_id", "C_term_exploded", "C_period_exploded", "C_rotation_exploded")) |>
    mutate(
      .class_handled = H_tpr_100p_percent == 1,
      student_adjuster_post_student = if_else(.class_handled, 1L, N_pt),
      .effective_N = if_else(.class_handled, as.integer(H_student_count_of_100p_classes), N_pt),
      M_class_weight_post_student = M_class_weight_post_teachers / student_adjuster_post_student,
      M_student_count_adjusted    = 1 / .effective_N) |>
    select(-.class_handled, -.effective_N)
}

# --- ASSEMBLY: class -> teacher -> student, in order -------------------------
# Runs the full resolution and returns the resolved exploded frame + the
# consolidation mapping. Requires 00_consolidation_helpers.R.
resolve_double_bookings <- function(flagged, config) {
  post_class <- resolve_class_stage(flagged)                 # class stage
  mapping    <- consolidate_sections(post_class)             # teacher stage: graph
  applied    <- apply_consolidation(post_class, mapping)     # teacher stage: re-id
  applied$M_class_weight_post_teachers <- applied$M_class_weight_post_class  # re-id only; weight unchanged per row
  resolved   <- resolve_student_stage(applied)               # student stage
  list(resolved = resolved, mapping = mapping)
}


# --- BEFORE -> AFTER SUMMARY (the XYZ -> ABC headline) ------------------------
# Reads off the resolved frame. "before" = the picture with no resolution
# (raw counts, full weights, original sections); "after" = resolved (DBs split,
# sections consolidated, headcounts adjusted). Re-flags on the resolved weights
# to show DBs collapse. Requires 00_flagging_helpers.R.
resolution_before_after <- function(resolved, config) {
  # --- DBs before (from flags) ---
  stu_b <- resolved |> group_by(D_stu_id) |> summarise(db = max(H_db_student_row), .groups = "drop")
  tch_b <- resolved |> group_by(D_employee_id) |> summarise(db = max(H_db_teacher_row), .groups = "drop")

  # --- DBs after: re-flag on consolidated ids + resolved weights ---
  rf <- resolved
  rf$C_class_id <- rf$C_class_id_post_teachers
  rf$M_class_weight_adj_same_course <- rf$M_class_weight_post_student
  rf <- rf |> select(-any_of(c("H_db_student_row","H_db_teacher_row",
                               "H_class_with_teacher_db","H_class_with_student_db","H_class_with_db")))
  rf <- flag_student_db(rf); rf <- flag_teacher_db(rf)
  stu_a <- rf |> group_by(D_stu_id) |> summarise(db = max(H_db_student_row), .groups = "drop")
  tch_a <- rf |> group_by(D_employee_id) |> summarise(db = max(H_db_teacher_row), .groups = "drop")

  # --- weight per student ---
  ws_b <- mean(tapply(resolved$M_class_weight_exploded,   resolved$D_stu_id, sum))
  ws_a <- mean(tapply(resolved$M_class_weight_post_student, resolved$D_stu_id, sum))

  # --- teacher load = sum of distinct class weights per teacher ---
  tload <- function(class_col) {
    resolved |>
      distinct(D_employee_id, .cls = .data[[class_col]],
               C_term_exploded, C_period_exploded, C_rotation_exploded, M_class_weight_exploded) |>
      group_by(D_employee_id, .cls) |> summarise(cw = sum(M_class_weight_exploded), .groups = "drop") |>
      group_by(D_employee_id) |> summarise(load = sum(cw), .groups = "drop") |> pull(load) |> mean()
  }

  # --- class size per class-slot: raw distinct vs adjusted sum ---
  sz_b <- resolved |>
    group_by(C_class_id_original, C_term_exploded, C_period_exploded, C_rotation_exploded) |>
    summarise(sz = n_distinct(D_stu_id), .groups = "drop") |> summarise(m = mean(sz)) |> pull(m)
  sz_a <- resolved |>
    group_by(C_class_id_post_teachers, C_term_exploded, C_period_exploded, C_rotation_exploded) |>
    summarise(sz = sum(M_student_count_adjusted), .groups = "drop") |> summarise(m = mean(sz)) |> pull(m)

  data.frame(
    metric = c("Students double-booked", "Teachers double-booked", "Distinct sections",
               "Avg weight / student", "Avg teacher load", "Avg class size (per slot)"),
    before = c(sum(stu_b$db), sum(tch_b$db), dplyr::n_distinct(resolved$C_class_id_original),
               round(ws_b, 2), round(tload("C_class_id_original"), 2), round(sz_b, 1)),
    after  = c(sum(stu_a$db), sum(tch_a$db), dplyr::n_distinct(resolved$C_class_id_post_teachers),
               round(ws_a, 2), round(tload("C_class_id_post_teachers"), 2), round(sz_a, 1)),
    stringsAsFactors = FALSE)
}
