# =============================================================================
# 00_rollup_helpers.R   —   TIER 1 CORE rollup computations (port of 08).
#
# Everything here is derivable from the required course-schedule fields alone —
# no HR or grades data needed. Buckets and periods-per-school are config-driven.
# The add-on modules (course grades, teacher HR) left-join onto these later.
#
# Requires the UNIFIED frame (from unify_resolution).
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

# config-driven bucketing: buckets are an ordered list of [min, max] PAIRS
# (last max may be null = no upper bound). Labels derived as "min-max" / "min+".
# A value falls in the bucket whose min it meets but the next bucket's does not.
.bucketize <- function(x, buckets) {
  mins   <- vapply(buckets, function(b) as.numeric(b[[1]]), numeric(1))
  maxs   <- vapply(buckets, function(b) { m <- b[[2]]; if (is.null(m)) Inf else as.numeric(m) }, numeric(1))
  labels <- vapply(seq_along(buckets), function(k)
    if (is.infinite(maxs[k])) paste0(mins[k], "+") else paste0(mins[k], "-", maxs[k]), character(1))
  i   <- findInterval(x, mins)                       # x >= mins[i]
  lab <- ifelse(is.na(x) | i < 1 | i > length(labels), NA_character_, labels[i])
  factor(lab, levels = labels)
}

# "primary" (dominant-by-weight) value of a field per teacher: the value carrying
# the most summed class weight. Ties broken deterministically. Returns
# (D_employee_id, <out_name>). Mirrors 08's primary().
primary_field <- function(unified, field, out_name, exclude_advisory = FALSE) {
  src <- unified
  if (exclude_advisory && "C_course_subject" %in% names(src))
    src <- src |> filter(is.na(C_course_subject) | C_course_subject != "Advisory")
  src |>
    group_by(D_employee_id, C_class_id, .val = .data[[field]]) |>
    summarise(hmax = max(M_class_weight, na.rm = TRUE), .groups = "drop") |>
    group_by(D_employee_id, .val) |>
    summarise(wsum = sum(hmax, na.rm = TRUE), .groups = "drop") |>
    group_by(D_employee_id) |>
    arrange(desc(wsum), as.character(.val), .by_group = TRUE) |>
    slice_head(n = 1) |>
    ungroup() |>
    transmute(D_employee_id, !!out_name := .val)
}

# --- minutes per meeting -----------------------------------------------------
# M_meeting_minutes = the bell-schedule clock-length of ONE meeting, from config
# bell_minutes.by_school (honoring per-meeting overrides like advisory "8@A": 55).
# ONLY meaningful per exploded meeting, so it's populated on exploded (DB) rows
# and NA on non-DB rows (a whole un-exploded class has no single meeting length).
# Instructional-time metrics (weekly mins, annual hours) are derived DOWNSTREAM by
# combining this with the weight columns on the exploded grain.
add_minutes <- function(unified, config, spec) {
  u <- as.data.frame(unified) |> select(-any_of("M_meeting_minutes"))

  bell_lookup <- function(meeting, school) {
    bm <- config$bell_minutes$by_school[[as.character(school)]]
    if (is.null(bm)) return(NA_real_)
    ov <- bm$overrides
    if (!is.null(ov) && !is.null(ov[[meeting]])) return(as.numeric(ov[[meeting]]))
    if (!is.null(bm$default)) return(as.numeric(bm$default))
    NA_real_
  }

  # meeting only exists on exploded rows; non-DB rows -> NA (no single length)
  u$.meeting <- ifelse(is.na(u$C_period_exploded), NA_character_,
                       paste0(u$C_period_exploded, "@", u$C_rotation_exploded))

  ms <- unique(data.frame(.meeting = u$.meeting, D_location_name = u$D_location_name,
                          stringsAsFactors = FALSE))
  ms$M_meeting_minutes <- ifelse(is.na(ms$.meeting), NA_real_,
                                 mapply(bell_lookup, ms$.meeting, ms$D_location_name))
  u <- left_join(u, ms, by = c(".meeting", "D_location_name"))
  u$.meeting <- NULL
  u
}

# --- class size --------------------------------------------------------------
# M_cls_class_size = sum(M_student_count_adjusted) per class-slot (the hook coming
# home). Non-DB rows never got an adjuster -> they are 1 full head.
add_class_size <- function(unified, config) {
  unified |>
    mutate(M_student_count_adjusted = ifelse(is.na(M_student_count_adjusted), 1, M_student_count_adjusted)) |>
    group_by(C_class_id, D_term, D_period, D_rotation) |>
    mutate(M_cls_num_stu     = n_distinct(D_stu_id),
           M_cls_class_size  = as.numeric(sum(M_student_count_adjusted, na.rm = TRUE)),
           M_cls_class_size_diff = M_cls_num_stu - M_cls_class_size) |>
    ungroup() |>
    mutate(C_cls_class_size_bucket = .bucketize(M_cls_class_size, config$buckets$class_size))
}

# --- teacher metrics ---------------------------------------------------------
# M_tch_num_periods   = sum over the teacher's distinct class-slots of max class
#                   weight (term/rotation/period already baked in).
# M_tch_utilization = M_tch_num_periods / periods_per_school (config; e.g. Sci
#                   Tech = 4.2 to account for the once-a-week advisory).
# M_tch_load  = distinct students taught (load-off teachers excluded).
add_teacher_metrics <- function(unified, config) {
  # primary school per teacher (needed for grouping + the utilization denominator)
  if (!"C_tch_location_name" %in% names(unified)) {
    prim_loc <- primary_field(unified, "D_location_name", "C_tch_location_name")
    unified <- left_join(unified, prim_loc, by = "D_employee_id")
  }

  # weighted periods -> utilization
  fu_def    <- config$full_utilization$default
  fu_school <- config$full_utilization$by_school
  util <- unified |>
    filter(C_teacher_load_exclude == "Include") |>
    group_by(C_tch_location_name, D_employee_id, D_term, D_rotation, D_period) |>
    summarise(M_teacher_class_weight = max(M_class_weight, na.rm = TRUE), .groups = "drop") |>
    group_by(C_tch_location_name, D_employee_id) |>
    summarise(M_tch_num_periods = sum(M_teacher_class_weight, na.rm = TRUE), .groups = "drop") |>
    mutate(C_school_total_periods = vapply(as.character(C_tch_location_name),
             function(s) {
               v <- fu_school[[s]]
               if (!is.null(v)) as.numeric(v)
               else if (!is.null(fu_def)) as.numeric(fu_def)
               else NA_real_
             }, numeric(1)),
           M_tch_utilization = M_tch_num_periods / C_school_total_periods)

  unified <- unified |>
    left_join(util |> select(D_employee_id, C_tch_location_name,
                             M_tch_num_periods, C_school_total_periods, M_tch_utilization),
              by = c("D_employee_id", "C_tch_location_name"))

  # teacher load (distinct students) + bucket
  unified |>
    group_by(C_tch_location_name, D_employee_id) |>
    mutate(M_tch_load = n_distinct(D_stu_id[C_teacher_load_exclude == "Include"])) |>
    ungroup() |>
    mutate(M_tch_load_bucket = .bucketize(M_tch_load, config$buckets$teacher_load))
}


# =============================================================================
# CLASS-LEVEL DERIVATIONS (faithful to 08 — multi-valued on purpose: a bundled
# class genuinely spans several courses/subjects, unlike teacher/student level).
# =============================================================================

# Collapse a class's course names into one label: common prefix + " / " suffixes
# (e.g. "Band I","Band II" -> "Band I / II"). Faithful port of 08.
collapse_course_names <- function(course_names) {
  cn <- stringr::str_squish(course_names)
  cn <- unique(cn[!is.na(cn) & cn != ""])
  cn <- stringr::str_sort(cn, numeric = TRUE)
  if (length(cn) == 0) return(NA_character_)
  if (length(cn) == 1) return(cn)
  # drop names fully contained in longer ones ("Phys Ed" in "Phys Ed 10")
  keep <- !vapply(seq_along(cn), function(i)
    any(stringr::str_detect(cn[-i], stringr::fixed(cn[i]))), logical(1))
  cn <- stringr::str_sort(cn[keep], numeric = TRUE)
  if (length(cn) == 1) return(cn)
  words <- stringr::str_split(cn, "\\s+")
  prefix_len <- 0
  for (i in seq_len(min(lengths(words)))) {
    if (length(unique(vapply(words, function(x) x[i], character(1)))) == 1) prefix_len <- i else break
  }
  if (prefix_len == 0) return(paste(cn, collapse = " / "))
  common <- paste(words[[1]][seq_len(prefix_len)], collapse = " ")
  suffixes <- vapply(words, function(x) paste(x[-seq_len(prefix_len)], collapse = " "), character(1))
  suffixes <- stringr::str_sort(suffixes[suffixes != ""], numeric = TRUE)
  stringr::str_squish(paste0(common, " ", paste(suffixes, collapse = " / ")))
}

# class primaries: dominant-by-weight value per class (class-level twin of
# primary_field / faithful to 08's primary_class). Used for subject/grade/rigor/
# location — these are single dominant values, NOT multi-valued.
primary_class <- function(unified, field, out_name) {
  dom <- unified |>
    group_by(C_class_id, .val = .data[[field]]) |>
    summarise(wsum = sum(M_class_weight, na.rm = TRUE), .groups = "drop") |>
    group_by(C_class_id) |>
    arrange(desc(wsum), as.character(.val), .by_group = TRUE) |>
    slice_head(n = 1) |>
    ungroup() |>
    transmute(C_class_id, !!out_name := .val)
  left_join(unified, dom, by = "C_class_id")
}

add_class_primaries <- function(unified) {
  specs <- list(
    c("D_location_id",         "C_cls_location_id"),
    c("D_location_name",       "C_cls_location_name"),
    c("D_stu_grade",           "C_cls_grade"),
    c("C_course_subject",      "C_cls_course_subject"),
    c("C_course_subject_area", "C_cls_course_subject_area"),
    c("C_course_rigor",        "C_cls_course_rigor"),
    c("C_course_rigor_detail", "C_cls_course_rigor_detail"))
  for (s in specs)
    if (s[1] %in% names(unified) && !s[2] %in% names(unified))
      unified <- primary_class(unified, s[1], s[2])
  unified
}

# course NAME stays multi-valued (collapsed: "Band I / II"); credit type is
# priority-ranked. These are the class-level fields that are NOT dominant-by-weight.
add_class_fields <- function(unified) {
  unified <- unified |>
    select(-any_of("C_cls_course_name")) |>
    group_by(C_class_id) |>
    mutate(C_cls_course_name = collapse_course_names(C_course_name)) |>
    ungroup()
  cred <- unified |>
    filter(!is.na(C_course_credit_type)) |>
    group_by(C_class_id, C_course_credit_type) |>
    summarise(n_students = n(), .groups = "drop") |>
    mutate(priority = case_when(
      C_course_credit_type == "Graduation Required"  ~ 1,
      C_course_credit_type == "Support & Enrichment" ~ 2,
      C_course_credit_type == "Elective"             ~ 3,
      C_course_credit_type == "UNSURE"               ~ 4, TRUE ~ 5)) |>
    group_by(C_class_id) |>
    arrange(desc(n_students), priority, .by_group = TRUE) |>
    slice(1) |>
    transmute(C_class_id, C_cls_course_credit_type = C_course_credit_type)
  left_join(unified, cred, by = "C_class_id")
}

# TIER 2: EL/SWD class characteristics (counts, pcts, buckets). Only if flags exist.
add_class_demographics <- function(unified) {
  if (!all(c("D_stu_ell_flag", "D_stu_swd_flag") %in% names(unified))) return(unified)
  helper <- unified |>
    group_by(C_class_id, D_stu_id, D_stu_ell_flag, D_stu_swd_flag) |> count() |>
    group_by(C_class_id) |>
    summarise(M_cls_num_ell = sum(D_stu_ell_flag),
              M_cls_num_swd = sum(D_stu_swd_flag),
              M_cls_pct_ell = sum(D_stu_ell_flag) / n(),
              M_cls_pct_swd = sum(D_stu_swd_flag) / n(), .groups = "drop") |>
    mutate(C_cls_ell_bucket = case_when(M_cls_pct_ell <= 0.2 ~ "0-20%",
                                        M_cls_pct_ell <  0.5 ~ "20-50%",
                                        M_cls_pct_ell <= 1   ~ ">50%", TRUE ~ "Error"),
           C_cls_swd_bucket = case_when(M_cls_pct_swd <= 0.2 ~ "0-20%",
                                        M_cls_pct_swd <  0.5 ~ "20-50%",
                                        M_cls_pct_swd <= 1   ~ ">50%", TRUE ~ "Error"))
  left_join(unified, helper, by = "C_class_id")
}

# M_cls_class_weight_times_class_size = max class weight x max class size per class.
add_class_weight_times_size <- function(unified) {
  unified |>
    group_by(C_class_id) |>
    mutate(M_cls_class_weight_times_class_size =
             .max_or_na(M_cls_class_weight) * .max_or_na(M_cls_class_size)) |>
    ungroup()
}

# Teacher dominant-by-weight primaries (the tch-level "primary" fields).
add_teacher_primary_fields <- function(unified) {
  specs <- list(
    list("C_course_subject",      "C_tch_course_subject",      TRUE),
    list("C_course_subject_area", "C_tch_course_subject_area", TRUE),
    list("C_course_credit_type",  "C_tch_course_credit_type",  TRUE),
    list("D_stu_grade",           "C_tch_grade",               FALSE))
  for (s in specs) {
    if (!s[[2]] %in% names(unified) && s[[1]] %in% names(unified))
      unified <- left_join(unified, primary_field(unified, s[[1]], s[[2]], s[[3]]), by = "D_employee_id")
  }
  unified
}

# create any missing columns as NA (so rollups always emit VISTA's full schema)
.ensure_cols <- function(df, cols) {
  for (c in cols) if (!c %in% names(df)) df[[c]] <- NA
  df
}

# TIER 2: teacher EL/SWD flags — mean of class pcts across a teacher's classes.
add_teacher_el_swd <- function(unified) {
  if (!all(c("M_cls_pct_ell", "M_cls_pct_swd") %in% names(unified))) return(unified)
  helper <- unified |>
    group_by(D_employee_id, C_class_id, M_cls_pct_ell, M_cls_pct_swd) |> count() |>
    group_by(D_employee_id) |>
    summarise(C_tch_percent_ell_classes = mean(M_cls_pct_ell, na.rm = TRUE),
              C_tch_percent_swd_classes = mean(M_cls_pct_swd, na.rm = TRUE), .groups = "drop") |>
    mutate(C_tch_ell_flag = ifelse(C_tch_percent_ell_classes > 0.5, "ELL teacher", "Not ELL teacher"),
           C_tch_swd_flag = ifelse(C_tch_percent_swd_classes > 0.5, "SWD teacher", "Not SWD teacher"),
           C_tch_ell_swd_flag = case_when(
             C_tch_ell_flag == "ELL teacher"     & C_tch_swd_flag == "SWD teacher"     ~ "ELL and SWD teacher",
             C_tch_ell_flag == "ELL teacher"     & C_tch_swd_flag == "Not SWD teacher" ~ "ELL teacher",
             C_tch_ell_flag == "Not ELL teacher" & C_tch_swd_flag == "SWD teacher"     ~ "SWD teacher",
             C_tch_ell_flag == "Not ELL teacher" & C_tch_swd_flag == "Not SWD teacher" ~ "Gen Ed teacher",
             TRUE ~ NA_character_))
  left_join(unified, helper, by = "D_employee_id")
}

# advisory patch on teacher primaries + 9th-grade teacher flag.
add_teacher_flags <- function(unified) {
  if ("C_tch_course_subject" %in% names(unified))
    unified$C_tch_course_subject <- ifelse(is.na(unified$C_tch_course_subject),
                                           "Advisory", unified$C_tch_course_subject)
  if ("C_tch_course_subject_area" %in% names(unified))
    unified$C_tch_course_subject_area <- ifelse(is.na(unified$C_tch_course_subject_area),
                                                "Support & Enrichment", unified$C_tch_course_subject_area)
  if ("C_tch_grade" %in% names(unified))
    unified$C_tch_ninth_grade_flag <- ifelse(unified$C_tch_grade == 9,
                                             "Ninth grade teacher", "Not Ninth grade teacher")
  unified
}

# =============================================================================
# THE THREE ROLLUPS (emit the new tch/cls vocabulary; add-on fields NA if absent)
# =============================================================================

build_student_rollup <- function(unified) {
  cols <- c("D_location_name","D_stu_id","D_stu_grade","D_stu_swd_flag","D_stu_ell_flag",
            "D_stu_poverty_flag","D_stu_demographic_flag","C_class_id","D_employee_id",
            "D_term","D_rotation","D_period","C_course_time_of_day","C_course_subject",
            "C_course_name","C_course_rigor","C_course_rigor_detail","C_course_subject_area",
            "C_course_credit_type","C_course_intervention","M_meeting_minutes","M_cls_pct_ell",
            "M_cls_pct_swd","M_class_weight","M_cls_num_stu","M_cls_class_size","C_cls_class_size_bucket",
            "C_proficiency_ela","C_proficiency_math","C_proficiency")
  unified |>
    filter(C_course_subject_area != "Untracked") |>
    .ensure_cols(cols) |>
    distinct(across(all_of(cols)))
}

build_teacher_rollup <- function(unified) {
  grp <- c("D_employee_id","C_tch_course_subject","C_tch_course_subject_area",
           "C_tch_course_credit_type","C_tch_grade","C_tch_location_name",
           "C_tch_percent_ell_classes","C_tch_percent_swd_classes","C_tch_ell_swd_flag",
           "C_tch_ell_flag","C_tch_swd_flag","C_tch_novice_indicator",
           "D_employee_license_type_rank","D_employee_license_type",
           "D_employee_license_subject_concat","C_tch_ninth_grade_flag",
           "M_tch_num_periods","M_tch_utilization","M_tch_load","M_tch_load_bucket")
  u <- unified |>
    filter(C_teacher_load_exclude == "Include", C_course_subject_area != "Untracked") |>
    .ensure_cols(c(grp, "D_meeting"))
  if (all(is.na(u$D_meeting)))
    u <- u |> mutate(D_meeting = paste0(C_period_exploded, "@", C_rotation_exploded))
  u |>
    group_by(across(all_of(grp))) |>
    summarise(M_tch_num_preps = n_distinct(C_course_name),
              M_tch_meetings_in_s1 = paste0(unique(D_meeting[D_term != "3502"]), collapse = " | "),
              C_tch_course_names   = paste0(unique(C_course_name), collapse = " | "),
              M_tch_weight = 1, .groups = "drop") |>
    arrange(desc(C_tch_location_name), desc(M_tch_utilization))
}

# max() that returns NA (not -Inf + warning) when every value in the group is NA
.max_or_na <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

build_class_rollup <- function(unified, teacher_rollup) {
  # class-level grain: one row per class-slot with all the class fields
  grp <- c("C_cls_location_id","C_cls_location_name","C_class_id","D_term","D_period","D_rotation",
           "C_teacher_load_exclude","C_class_size_exclude","D_employee_id",
           "C_cls_course_subject","C_cls_course_subject_area","C_cls_course_name","C_cls_course_credit_type",
           "C_cls_grade","C_cls_course_rigor","C_cls_course_rigor_detail",
           "M_cls_num_ell","M_cls_num_swd","M_cls_pct_ell","M_cls_pct_swd","C_cls_ell_bucket","C_cls_swd_bucket",
           "M_cls_num_stu","M_cls_class_size","C_cls_class_size_bucket")
  base <- unified |>
    .ensure_cols(c(grp, "M_meeting_minutes", "M_cls_class_weight", "M_cls_class_weight_times_class_size")) |>
    group_by(across(all_of(grp))) |>
    summarise(M_cls_class_weight = .max_or_na(M_cls_class_weight),
              M_meeting_minutes = .max_or_na(M_meeting_minutes),
              M_cls_class_weight_times_class_size = .max_or_na(M_cls_class_weight_times_class_size),
              .groups = "drop")

  # 3.6: merge teacher-level flags (from the teacher rollup) into the class rollup
  tch_cols <- c("D_employee_id","C_tch_ell_swd_flag","C_tch_location_name","C_tch_course_subject",
                "C_tch_grade","D_employee_license_type","C_tch_novice_indicator","M_tch_num_periods",
                "M_tch_utilization","M_tch_load","M_tch_load_bucket","M_tch_num_preps")
  tch <- teacher_rollup |> ungroup() |> .ensure_cols(tch_cols) |>
    distinct(D_employee_id, .keep_all = TRUE) |>
    select(all_of(tch_cols))

  left_join(base, tch, by = "D_employee_id")
}