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
#       credit_recovery_categories: ["Credit Recovery", "SUM Summer School", "VHS"]
#       core_subjects: ["English", "Math", "Science", "Social Studies"]
#
# Returns list(course_grades_rollup, student_proficiency). The proficiency table
# left-joins onto the student rollup by D_stu_id; course_grades_rollup exports.
# Faithful to 08 Part IV. Grade -> rank A=1..F=5; proficiency uses the WORST
# (max rank) grad-required grade per subject; <=3 (A/B/C) Proficient, >=4 Below.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(stringr) })

process_course_grades <- function(course_grades_raw, course_coding, config) {
  cg_cfg    <- config$add_ons$course_grades
  colmap    <- cg_cfg$columns
  cr_cats   <- if (!is.null(cg_cfg$credit_recovery_categories)) cg_cfg$credit_recovery_categories
               else c("Credit Recovery", "SUM Summer School", "VHS")
  core_subj <- if (!is.null(cg_cfg$core_subjects)) cg_cfg$core_subjects
               else c("English", "Math", "Science", "Social Studies")

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

  # export rollup: real, non-recovery, graded rows with the letter grade
  course_grades_rollup <- cg |>
    filter(D_course_category != "TRN Transfer", D_course_credit_recovery == 0,
           !is.na(D_stu_course_grade), !is.na(D_stu_id)) |>
    mutate(C_stu_course_grade = str_extract(D_stu_course_grade, "^[A-F]"))

  # grad-required core -> letter -> rank -> worst per subject -> wide
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

  # proficiency (ELA from english, Math from math; <=3 Proficient, >=4 Below)
  if (!"C_course_grade_english" %in% names(core)) core$C_course_grade_english <- NA_real_
  if (!"C_course_grade_math"    %in% names(core)) core$C_course_grade_math    <- NA_real_
  student_proficiency <- core |>
    mutate(
      C_proficiency_ela = case_when(C_course_grade_english <= 3 ~ "Proficient",
                                    C_course_grade_english >= 4 ~ "Below Proficient",
                                    TRUE ~ NA_character_),
      C_proficiency_math = case_when(C_course_grade_math <= 3 ~ "Proficient",
                                     C_course_grade_math >= 4 ~ "Below Proficient",
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

  list(course_grades_rollup = course_grades_rollup, student_proficiency = student_proficiency)
}

# Merge the derived proficiency onto the student rollup (or unified) by student.
apply_course_grades <- function(x, student_proficiency) {
  left_join(x, student_proficiency, by = "D_stu_id")
}
