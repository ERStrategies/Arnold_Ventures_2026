# =============================================================================
# 00_flagging_helpers.R
#
# WHAT THIS DOES:
#   After we "explode" each class into individual time-slots, this file
#   DETECTS double-bookings. A double-booking means a student or teacher
#   is assigned to more than one class in the same time-slot.
#
# HOW IT DETECTS DOUBLE-BOOKINGS:
#   For each person in each time-slot, we add up the class weights.
#   If the total exceeds what a single class should weigh in that slot,
#   that's a double-booking. (If you're only in one class, the sum equals
#   the expected weight exactly. If you're in two, the sum is ~2x the expected.)
#
# THE STEPS (in order):
#   1. Build a class ID from the exploded time columns (so classes are
#      identified at the individual-slot level, not the expression level).
#   2. Adjust for duplicate rows — if a student appears twice in the same
#      class (a data artifact), divide the weight by the duplicate count
#      so they don't look double-booked with themselves.
#   3. Flag students who are double-booked in any time-slot.
#   4. Flag teachers who are double-booked in any time-slot.
#   5. Flag classes that contain ANY double-booked row.
#
# Depends on: dplyr, tidyr. Works on the exploded frame from 00_explosion_helpers.R.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })


# =============================================================================
# build_exploded_class_id()
#
# Creates a class ID using the EXPLODED time columns instead of the original ones.
# Why? Because after explosion, a class that meets "periods 1-3 on A days" becomes
# three separate rows (P1@A, P2@A, P3@A). We need an ID that works at THIS level.
#
# The recipe comes from config$class_id$recipe, but we swap out the original
# time columns (D_expression, D_period, D_rotation, D_term) for their exploded
# versions (C_term_exploded, C_period_exploded, C_rotation_exploded).
#
# IMPORTANT: The recipe should use SECTION (D_course_section_id), NOT teacher
# (D_employee_id). If teacher is in the ID, it merges a teacher's sections
# together and hides teacher double-bookings.
# =============================================================================

build_exploded_class_id <- function(exploded, config, sep = "_") {
  recipe <- config$class_id$recipe

  # Swap original time columns for their exploded counterparts
  original_time_cols <- c("D_expression", "D_period", "D_rotation", "D_term")
  exploded_time_cols <- c("C_term_exploded", "C_period_exploded", "C_rotation_exploded")

  non_time_recipe_cols <- setdiff(recipe, original_time_cols)
  id_columns <- c(non_time_recipe_cols, exploded_time_cols)

  # Only use columns that actually exist in the data

  id_columns <- id_columns[id_columns %in% names(exploded)]

  # Paste them together to form the class ID
  do.call(
    paste,
    c(lapply(id_columns, function(col_name) as.character(exploded[[col_name]])), sep = sep)
  )
}


# =============================================================================
# add_same_course_adjuster()
#
# Handles a data quirk: sometimes a student appears multiple times in the
# same class (duplicate rows — NOT a real double-booking). If we don't
# account for this, the weight sum will be inflated and we'll incorrectly
# flag them as double-booked.
#
# Fix: count how many times each student appears in each class, then divide
# the weight by that count. So 2 duplicate rows each get half the weight,
# and the sum stays correct.
# =============================================================================

add_same_course_adjuster <- function(exploded) {

  # Count duplicates: how many rows does this student have in this class?
  duplicate_counts <- exploded |>
    count(C_class_id, D_stu_id, name = "H_student_adjuster_same_course")

  exploded |>
    left_join(duplicate_counts, by = c("C_class_id", "D_stu_id")) |>
    mutate(
      H_student_adjuster_same_course = replace_na(H_student_adjuster_same_course, 1L),
      # Adjusted weight = original weight ÷ number of duplicate rows
      M_class_weight_adj_same_course = M_class_weight_exploded / H_student_adjuster_same_course
    )
}


# =============================================================================
# flag_student_db()
#
# For each student in each time-slot:
#   - Sum up the adjusted class weights across all their classes in that slot.
#   - If the sum exceeds what a single class should weigh → double-booked (flag = 1).
#   - If the sum equals the expected weight → not double-booked (flag = 0).
# =============================================================================

flag_student_db <- function(exploded) {

  # Calculate the weight sum per student per time-slot
  student_slot_flags <- exploded |>
    group_by(
      D_location_id, D_stu_id,
      C_term_exploded, C_rotation_exploded, C_period_exploded,
      M_class_weight_exploded
    ) |>
    summarise(
      weight_sum = sum(M_class_weight_adj_same_course),
      .groups = "drop"
    ) |>
    # Flag: is the sum greater than what one class should weigh?
    mutate(
      H_db_student_row = as.integer(weight_sum > M_class_weight_exploded)
    ) |>
    # Take the max flag per slot (in case multiple weight levels exist)
    group_by(
      D_location_id, D_stu_id,
      C_term_exploded, C_rotation_exploded, C_period_exploded
    ) |>
    summarise(H_db_student_row = max(H_db_student_row), .groups = "drop")

  # Join the flag back onto the full exploded data
  exploded |>
    left_join(
      student_slot_flags,
      by = c("D_location_id", "D_stu_id", "C_term_exploded",
             "C_rotation_exploded", "C_period_exploded")
    ) |>
    mutate(H_db_student_row = replace_na(H_db_student_row, 0L))
}


# =============================================================================
# flag_teacher_db()
#
# Same logic as student flagging, but with one extra step:
#   - A teacher might have many students in one class, so we first collapse
#     to ONE weight per class (using the max adjusted weight across students).
#   - Then we sum across classes per teacher per time-slot.
#   - If the sum exceeds expected → double-booked.
# =============================================================================

flag_teacher_db <- function(exploded) {

  # Step 1: Get one weight per class per teacher per time-slot
  # (A class has many student rows — take the max weight to represent the class)
  teacher_slot_flags <- exploded |>
    group_by(
      D_location_id, D_employee_id, C_class_id,
      C_period_exploded, C_term_exploded, C_rotation_exploded,
      M_class_weight_exploded
    ) |>
    summarise(
      weight_for_class = max(M_class_weight_adj_same_course),
      .groups = "drop"
    ) |>
    # Step 2: Sum across all classes this teacher has in this time-slot
    group_by(
      D_location_id, D_employee_id,
      C_period_exploded, C_term_exploded, C_rotation_exploded,
      M_class_weight_exploded
    ) |>
    summarise(
      weight_sum_across_classes = sum(weight_for_class),
      .groups = "drop"
    ) |>
    # Flag: does the sum exceed what one class should weigh?
    mutate(
      H_db_teacher_row = as.integer(weight_sum_across_classes > M_class_weight_exploded)
    ) |>
    # Take the max flag per slot
    group_by(
      D_location_id, D_employee_id,
      C_period_exploded, C_term_exploded, C_rotation_exploded
    ) |>
    summarise(H_db_teacher_row = max(H_db_teacher_row), .groups = "drop")

  # Join the flag back onto the full exploded data
  exploded |>
    left_join(
      teacher_slot_flags,
      by = c("D_location_id", "D_employee_id", "C_period_exploded",
             "C_term_exploded", "C_rotation_exploded")
    ) |>
    mutate(H_db_teacher_row = replace_na(H_db_teacher_row, 0L))
}


# =============================================================================
# add_class_db_flags()
#
# A class is flagged as having double-bookings if ANY of its rows are flagged
# (either student-DB or teacher-DB). This lets us identify which classes are
# involved in double-bookings at the class level.
# =============================================================================

add_class_db_flags <- function(exploded) {
  exploded |>
    group_by(C_class_id) |>
    mutate(
      H_class_with_teacher_db = max(H_db_teacher_row),
      H_class_with_student_db = max(H_db_student_row),
      H_class_with_db = pmax(H_class_with_teacher_db, H_class_with_student_db)
    ) |>
    ungroup()
}


# =============================================================================
# flag_double_bookings()
#
# The main wrapper that runs all flagging steps in order.
# Takes the exploded data and returns it with DB flags added.
#
# Set filter_to_db = TRUE to return ONLY the rows involved in double-bookings
# (this is the input for the resolution step). Otherwise returns all rows.
# =============================================================================

flag_double_bookings <- function(exploded, config, filter_to_db = FALSE) {

  # Start with only rows that are "Include" for both metrics
  included_rows <- exploded |>
    filter(
      C_course_time_exclude == "Include",
      C_class_size_and_teacher_load_exclude == "Include"
    )

  # Save the original class ID (before we rebuild it from exploded columns)
  included_rows$C_class_id_original <- included_rows$C_class_id

  # Rebuild class ID using the exploded time columns
  included_rows$C_class_id <- build_exploded_class_id(included_rows, config)

  # Run the flagging pipeline
  included_rows <- add_same_course_adjuster(included_rows)
  included_rows <- flag_student_db(included_rows)
  included_rows <- flag_teacher_db(included_rows)
  included_rows <- add_class_db_flags(included_rows)

  # Optionally filter to only double-booked classes (for resolution input)
  if (filter_to_db) {
    included_rows <- filter(included_rows, H_class_with_db == 1)
  }

  included_rows
}


# =============================================================================
# flagging_summary()
#
# A quick cross-check: how many students/teachers are flagged as double-booked?
# Use this to compare against Stage 02's db_flagged results — they should agree.
# =============================================================================

flagging_summary <- function(flagged) {

  # One row per student: are they double-booked anywhere?
  student_flags <- flagged |>
    group_by(D_stu_id) |>
    summarise(is_db = max(H_db_student_row), .groups = "drop")

  # One row per teacher: are they double-booked anywhere?
  teacher_flags <- flagged |>
    group_by(D_employee_id) |>
    summarise(is_db = max(H_db_teacher_row), .groups = "drop")

  data.frame(
    metric = c(
      "students flagged",
      "teachers flagged",
      "student-DB rows (%)",
      "teacher-DB rows (%)"
    ),
    value = c(
      paste0(sum(student_flags$is_db), " of ", nrow(student_flags),
             " (", round(100 * mean(student_flags$is_db), 1), "%)"),
      paste0(sum(teacher_flags$is_db), " of ", nrow(teacher_flags),
             " (", round(100 * mean(teacher_flags$is_db), 1), "%)"),
      paste0(round(100 * mean(flagged$H_db_student_row), 1), "%"),
      paste0(round(100 * mean(flagged$H_db_teacher_row), 1), "%")
    )
  )
}
