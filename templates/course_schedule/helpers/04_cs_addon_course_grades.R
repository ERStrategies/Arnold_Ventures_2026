# =============================================================================
# 04_cs_addon_course_grades.R   —   ADD-ON: course grades -> proficiency (Tier 3).
#
# Optional. Prior-year course grades (technically transcript data) commonly come
# with a course-schedule analysis, so this lives in the template. The CORE
# rollups never depend on it — without it, the student rollup carries the
# proficiency fields as NA and VISTA drops the metrics that need them.
#
#   add_ons:
#     course_grades:
#       enabled: true
#       file: "course_grades.csv"
#       columns: { D_stu_id: "Student ID", D_course_name: "Course Description", ... }
#       grade_mode: letter          # letter | percentage
#       proficiency_threshold_pct: 70  # percentage mode only
#       join_keys: [D_stu_id]       # keys for merging onto student rollup
#       credit_recovery_categories: ["Credit Recovery", "SUM Summer School", "VHS"]
#       core_subjects: ["English", "Math", "Science", "Social Studies"]
#
# Returns list(course_grades_rollup, student_proficiency). The proficiency table
# left-joins onto the student rollup by join_keys; course_grades_rollup exports.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(stringr) })

process_course_grades <- function(course_grades_raw, course_coding, config) {
  cg_cfg    <- config$add_ons$course_grades
  colmap    <- cg_cfg$columns
  cr_cats   <- if (!is.null(cg_cfg$credit_recovery_categories)) cg_cfg$credit_recovery_categories
               else c("Credit Recovery", "SUM Summer School", "VHS")
  core_subj <- if (!is.null(cg_cfg$core_subjects)) cg_cfg$core_subjects
               else c("English", "Math", "Science", "Social Studies")
  grade_mode <- if (!is.null(cg_cfg$grade_mode)) cg_cfg$grade_mode else "letter"
  prof_threshold <- if (!is.null(cg_cfg$proficiency_threshold_pct)) cg_cfg$proficiency_threshold_pct else 70

  # rename district columns -> D_ names via the config map (new = old)
  cg <- course_grades_raw
  for (new in names(colmap)) {
    old <- colmap[[new]]
    if (old %in% names(cg)) names(cg)[names(cg) == old] <- new
  }

  cg <- cg |> mutate(D_course_credit_recovery = if_else(D_course_category %in% cr_cats, 1, 0))

  # attach coded subject + credit type by course name
  coding_merge <- course_coding |>
    distinct(D_course_name, C_course_subject, C_course_credit_type)
  cg <- left_join(cg, coding_merge, by = "D_course_name")

  if (grade_mode == "letter") {
    course_grades_rollup <- .rollup_letter(cg)
    student_proficiency  <- .proficiency_letter(cg, core_subj)
  } else if (grade_mode == "percentage") {
    course_grades_rollup <- .rollup_percentage(cg, prof_threshold)
    student_proficiency  <- .proficiency_percentage(cg, core_subj, prof_threshold)
  } else {
    stop("Unknown grade_mode: '", grade_mode, "' (use letter | percentage)")
  }

  list(course_grades_rollup = course_grades_rollup, student_proficiency = student_proficiency)
}

# --- letter mode (original logic) -------------------------------------------

.rollup_letter <- function(cg) {
  cg |>
    filter(D_course_category != "TRN Transfer", D_course_credit_recovery == 0,
           !is.na(D_stu_course_grade), !is.na(D_stu_id)) |>
    mutate(C_stu_course_grade = str_extract(D_stu_course_grade, "^[A-F]"))
}

.proficiency_letter <- function(cg, core_subj) {
  core <- cg |>
    filter(C_course_credit_type == "Graduation Required",
           D_course_subject %in% core_subj,
           D_course_category != "TRN Transfer", D_course_credit_recovery == 0,
           !is.na(D_stu_course_grade), !is.na(D_stu_id)) |>
    mutate(C_course_grade_letter = str_extract(D_stu_course_grade, "^[A-F]"),
           C_course_grade_rank   = match(C_course_grade_letter, c("A","B","C","D","F")),
           C_course_subject_name = str_replace_all(str_to_lower(D_course_subject), " ", "_")) |>
    filter(!is.na(C_course_grade_rank)) |>
    group_by(D_location_name, D_stu_id, D_stu_grade, D_stu_cohort, C_course_subject_name) |>
    summarise(rank = max(C_course_grade_rank, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(id_cols = c(D_location_name, D_stu_id, D_stu_grade, D_stu_cohort),
                names_from = C_course_subject_name, values_from = rank,
                names_glue = "C_course_grade_{C_course_subject_name}")

  if (!"C_course_grade_english" %in% names(core)) core$C_course_grade_english <- NA_real_
  if (!"C_course_grade_math"    %in% names(core)) core$C_course_grade_math    <- NA_real_

  .derive_combined_proficiency(core,
    ela_proficient  = ~.x <= 3,
    math_proficient = ~.x <= 3)
}

# --- percentage mode ---------------------------------------------------------

.rollup_percentage <- function(cg, threshold) {
  cg |>
    filter(D_course_category != "TRN Transfer", D_course_credit_recovery == 0,
           !is.na(D_stu_course_grade), !is.na(D_stu_id)) |>
    mutate(C_stu_course_grade_pct = suppressWarnings(as.numeric(D_stu_course_grade)),
           C_stu_proficient = case_when(
             C_stu_course_grade_pct >= threshold ~ "Proficient",
             C_stu_course_grade_pct <  threshold ~ "Below Proficient",
             TRUE ~ NA_character_))
}

.proficiency_percentage <- function(cg, core_subj, threshold) {
  core <- cg |>
    filter(C_course_credit_type == "Graduation Required",
           D_course_subject %in% core_subj,
           D_course_category != "TRN Transfer", D_course_credit_recovery == 0,
           !is.na(D_stu_course_grade), !is.na(D_stu_id)) |>
    mutate(grade_pct = suppressWarnings(as.numeric(D_stu_course_grade)),
           C_course_subject_name = str_replace_all(str_to_lower(D_course_subject), " ", "_")) |>
    filter(!is.na(grade_pct)) |>
    # Worst grade per student × subject (min percentage)
    group_by(D_location_name, D_stu_id, D_stu_grade, D_stu_cohort, C_course_subject_name) |>
    summarise(grade_pct = min(grade_pct, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(id_cols = c(D_location_name, D_stu_id, D_stu_grade, D_stu_cohort),
                names_from = C_course_subject_name, values_from = grade_pct,
                names_glue = "C_course_grade_{C_course_subject_name}")

  if (!"C_course_grade_english" %in% names(core)) core$C_course_grade_english <- NA_real_
  if (!"C_course_grade_math"    %in% names(core)) core$C_course_grade_math    <- NA_real_

  .derive_combined_proficiency(core,
    ela_proficient  = ~.x >= threshold,
    math_proficient = ~.x >= threshold)
}

# --- shared proficiency derivation -------------------------------------------

.derive_combined_proficiency <- function(core, ela_proficient, math_proficient) {
  core |>
    mutate(
      C_proficiency_ela = case_when(
        ela_proficient(C_course_grade_english) ~ "Proficient",
        !ela_proficient(C_course_grade_english) ~ "Below Proficient",
        TRUE ~ NA_character_),
      C_proficiency_math = case_when(
        math_proficient(C_course_grade_math) ~ "Proficient",
        !math_proficient(C_course_grade_math) ~ "Below Proficient",
        TRUE ~ NA_character_),
      C_proficiency = case_when(
        C_proficiency_ela == "Below Proficient" & C_proficiency_math == "Below Proficient" ~ "Below Proficient",
        C_proficiency_ela == "Below Proficient" & C_proficiency_math == "Proficient"       ~ "Partially Proficient",
        C_proficiency_ela == "Proficient"       & C_proficiency_math == "Below Proficient" ~ "Partially Proficient",
        C_proficiency_ela == "Below Proficient" & is.na(C_proficiency_math)                ~ "Below Proficient",
        C_proficiency_ela == "Proficient"       & is.na(C_proficiency_math)                ~ "Proficient",
        is.na(C_proficiency_ela)                & C_proficiency_math == "Below Proficient" ~ "Below Proficient",
        is.na(C_proficiency_ela)                & C_proficiency_math == "Proficient"       ~ "Proficient",
        C_proficiency_ela == "Proficient"       & C_proficiency_math == "Proficient"       ~ "Proficient",
        TRUE ~ NA_character_)) |>
    select(D_stu_id, C_proficiency_ela, C_proficiency_math, C_proficiency)
}

# Merge the derived proficiency onto the student rollup (or unified) by student.
apply_course_grades <- function(x, student_proficiency, config = NULL) {
  join_keys <- if (!is.null(config$add_ons$course_grades$join_keys))
    config$add_ons$course_grades$join_keys else "D_stu_id"
  left_join(x, student_proficiency, by = join_keys)
}
