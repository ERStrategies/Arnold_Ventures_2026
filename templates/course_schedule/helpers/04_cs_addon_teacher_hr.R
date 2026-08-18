# =============================================================================
# 04_cs_addon_teacher_hr.R   —   ADD-ON: teacher HR flags (Tier 3).
#
# Optional. Derives teacher experience / novice flags from whatever HR data the
# district provided. The CORE rollups never depend on this — if no HR file, the
# teacher rollup just carries these fields as NA. Config declares the mode:
#
#   add_ons:
#     teacher_hr:
#       enabled: true
#       file: "teacher_licensure.csv"
#       mode: "licensure"          # licensure | experience | evaluation
#       license_ranks: { "Professional": 1, "Initial": 3, ... }   # licensure mode
#       novice_rank_threshold: 7   # rank >= this -> Novice        (licensure mode)
#       novice_years_threshold: 2  # years <= this -> Novice       (experience mode)
#
# Each mode returns one row per D_employee_id with its derived fields; the
# dispatcher left-joins onto the teacher rollup (or unified). Teams run the block
# that matches the data they received.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr) })

# --- mode: LICENSURE (rank-based; faithful to 08 3.2) ------------------------
teacher_hr_licensure <- function(teacher_licensure, config) {
  hr    <- config$add_ons$teacher_hr
  ranks <- hr$license_ranks
  nthr  <- if (!is.null(hr$novice_rank_threshold)) hr$novice_rank_threshold else 7
  rank_of <- function(t) vapply(as.character(t),
    function(x) { v <- ranks[[x]]; if (is.null(v)) NA_real_ else as.numeric(v) }, numeric(1))

  ranked <- teacher_licensure |>
    mutate(D_employee_license_type_rank = rank_of(D_employee_license_type)) |>
    group_by(D_employee_id) |>
    mutate(top_rank = min(D_employee_license_type_rank, na.rm = TRUE),
           D_employee_license_subject_concat = paste(
             unique(D_employee_license_subject[!is.na(D_employee_license_subject) &
                                               D_employee_license_subject != ""]), collapse = ", ")) |>
    ungroup()

  ranked |>
    filter(D_employee_license_type_rank == top_rank) |>
    distinct(D_employee_id, .keep_all = TRUE) |>
    transmute(
      D_employee_id,
      D_employee_license_type_rank = top_rank,
      D_employee_license_subject_concat,
      D_employee_license_type = case_when(
        D_employee_license_type == "Professional" ~ "1 - Professional",
        D_employee_license_type %in% c("Initial - Extension", "Initial") ~ "2 - Initial",
        D_employee_license_type %in% c("Provisional", "Preliminary-Extension",
                                       "Preliminary", "Temporary") ~ "3 - Provisional",
        D_employee_license_type == "Emergency-Extension II" ~ "4 - Emergency",
        is.na(D_employee_license_type) ~ "5 - Not licensed", TRUE ~ NA_character_),
      C_tch_novice_indicator = case_when(
        top_rank >= nthr ~ "Novice",
        top_rank <  nthr ~ "Experienced",
        is.na(top_rank)  ~ "Not licensed", TRUE ~ NA_character_))
}

# --- mode: EXPERIENCE (years of service) -------------------------------------
teacher_hr_experience <- function(teacher_exp, config,
                                  years_col = "D_tch_yrs_experience") {
  thr <- if (!is.null(config$add_ons$teacher_hr$novice_years_threshold))
    config$add_ons$teacher_hr$novice_years_threshold else 2
  teacher_exp |>
    transmute(D_employee_id,
              D_tch_yrs_experience = .data[[years_col]],
              C_tch_novice_indicator = ifelse(.data[[years_col]] <= thr, "Novice", "Not Novice"))
}

# --- mode: EVALUATION (rating) -----------------------------------------------
teacher_hr_evaluation <- function(teacher_eval, config,
                                  rating_col = "D_employee_rating") {
  teacher_eval |>
    transmute(D_employee_id, D_employee_rating = .data[[rating_col]])
}

# --- dispatcher --------------------------------------------------------------
# Left-joins the derived HR fields onto `x` (teacher rollup or unified) by teacher.
apply_teacher_hr <- function(x, hr_data, config) {
  hr <- config$add_ons$teacher_hr
  if (is.null(hr) || !isTRUE(hr$enabled)) return(x)
  derived <- switch(hr$mode,
    licensure  = teacher_hr_licensure(hr_data, config),
    experience = teacher_hr_experience(hr_data, config),
    evaluation = teacher_hr_evaluation(hr_data, config),
    stop("Unknown teacher_hr mode: '", hr$mode, "' (use licensure | experience | evaluation)"))
  left_join(x, derived, by = "D_employee_id")
}
