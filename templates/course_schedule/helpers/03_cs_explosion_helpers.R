# =============================================================================
# 00_explosion_helpers.R
# FOUNDATION for the faithful double-booking resolution port.
#
# Explosion the legacy way: build small CROSSWALKS once, then fan the data out
# with simple joins (04-db-explosions.Rmd pattern). Crosswalks are GENERATED
# from config + the parser — no external meeting file needed — but a district's
# section-meeting export could stand in for the meeting crosswalk if provided.
#
# Three crosswalks:
#   meeting  : time-key (D_expression OR D_period+D_rotation) -> C_period_exploded,
#              C_rotation_exploded   (structure; school-independent; via parser)
#   term     : D_term -> C_term_exploded, C_term_weight_exploded  (from term_expands)
#   weight   : (school, period, rotation) -> rotation/period weight  (per-school)
#
# Then M_class_weight_exploded = C_term_weight_exploded x rotation_wt x period_wt.
# INVARIANT: exploded weights sum back to the record's class weight.
#
# Legacy column names are kept so teams can trace 04/06 line-for-line.
# Depends on 00_expression_helpers.R, 00_weight_helpers.R, 00_db_helpers.R.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

# --- crosswalk builders (all small, all config/parser-driven) ----------------

# meeting crosswalk: distinct time-key -> its exploded (period, rotation) rows.
# Format-agnostic via record_meetings(): expression OR period+rotation.
build_meeting_crosswalk <- function(cs_data, config, spec) {
  tk     <- time_key_cols(config)
  combos <- unique(as.data.frame(cs_data)[, tk, drop = FALSE])
  rows <- lapply(seq_len(nrow(combos)), function(i) {
    m <- record_meetings(as.list(combos[i, tk, drop = FALSE]), config, spec)
    if (!length(m)) return(NULL)
    d <- data.frame(C_period_exploded   = sub("@.*$", "", m),
                    C_rotation_exploded = sub("^.*@", "", m),
                    stringsAsFactors = FALSE)
    for (k in tk) d[[k]] <- combos[[k]][i]
    d
  })
  bind_rows(rows)
}

# term crosswalk: D_term -> finest slots + each slot's year-share.
build_term_crosswalk <- function(config) {
  W  <- weight_lookups(config)
  te <- term_expands_spec(config)
  terms <- names(config$weights$term)
  rows <- lapply(terms, function(t) {
    slots <- term_slots(t, te)
    data.frame(D_term = as.character(t),
               C_term_exploded = as.character(slots),
               C_term_weight_exploded = .term_w(t, W) / length(slots),
               stringsAsFactors = FALSE)
  })
  bind_rows(rows)
}

# weight crosswalk: (school, period, rotation) -> rotation & period weights.
# Built from the distinct meetings that actually occur (tiny), via the config
# weight lookups (this is where per-school overrides like advisory live).
build_weight_crosswalk <- function(school_meeting_df, config) {
  W <- weight_lookups(config)
  d <- unique(as.data.frame(school_meeting_df))
  names(d)[1:3] <- c("D_location_name", "C_period_exploded", "C_rotation_exploded")
  mtg <- paste0(d$C_period_exploded, "@", d$C_rotation_exploded)
  d$C_rotation_weight_exploded <- mapply(function(m, s) .rot_w(m, s, W), mtg, d$D_location_name)
  d$C_period_weight_exploded   <- mapply(function(m, s) .per_w(m, s, W), mtg, d$D_location_name)
  d
}

# convenience: all three crosswalks as a named list (for the inspection check).
inspect_crosswalks <- function(cs_data, config, spec) {
  tk   <- time_key_cols(config)
  mtg  <- build_meeting_crosswalk(cs_data, config, spec)
  term <- build_term_crosswalk(config)
  d  <- unique(as.data.frame(cs_data)[, c("D_location_name", tk), drop = FALSE])
  sm <- unique(left_join(d, mtg, by = tk)[, c("D_location_name", "C_period_exploded", "C_rotation_exploded")])
  wt <- build_weight_crosswalk(sm, config)
  list("Meeting crosswalk (time-key -> period x rotation)" = mtg,
       "Term crosswalk (term -> slots + weight)"           = term,
       "Weight crosswalk (school x meeting -> weights)"    = wt)
}

# --- the explosion: fan out via the small crosswalks -------------------------
explode_for_resolution <- function(cs_data, config, spec) {
  df <- as.data.frame(cs_data)
  df$H_row_id_before_explosions <- seq_len(nrow(df))
  df$D_term <- as.character(df$D_term)
  tk <- time_key_cols(config)

  mtg_xwalk  <- build_meeting_crosswalk(df, config, spec)   # tiny
  term_xwalk <- build_term_crosswalk(config)                # tiny

  out <- df |>
    left_join(mtg_xwalk,  by = tk,        relationship = "many-to-many") |>
    left_join(term_xwalk, by = "D_term",  relationship = "many-to-many")

  wt_xwalk <- build_weight_crosswalk(
    unique(out[, c("D_location_name", "C_period_exploded", "C_rotation_exploded")]), config)
  out <- left_join(out, wt_xwalk,
                   by = c("D_location_name", "C_period_exploded", "C_rotation_exploded"))

  out$M_class_weight_exploded <- round(out$C_term_weight_exploded *
                                       out$C_rotation_weight_exploded *
                                       out$C_period_weight_exploded, 4)
  out$H_new_record_id <- seq_len(nrow(out))
  out
}

# --- checks / transparency ---------------------------------------------------

# INVARIANT: exploded weights sum back to class weight, per record.
check_explosion_invariant <- function(exploded, cs_data_with_weight) {
  by_rec <- exploded |>
    group_by(H_row_id_before_explosions) |>
    summarise(exploded_sum = round(sum(M_class_weight_exploded), 4), .groups = "drop")
  base <- data.frame(H_row_id_before_explosions = seq_len(nrow(cs_data_with_weight)),
                     class_weight = round(cs_data_with_weight$M_cls_class_weight, 4))
  merged <- left_join(by_rec, base, by = "H_row_id_before_explosions")
  merged$match <- abs(merged$exploded_sum - merged$class_weight) < 1e-6
  list(all_match = all(merged$match, na.rm = TRUE),
       n_mismatch = sum(!merged$match, na.rm = TRUE),
       sample_mismatch = head(merged[!merged$match & !is.na(merged$match), ], 10))
}

# TRANSPARENCY: trace ONE record/student through the exploded rows + every stage
# weight column present so far. Grows as resolution stages add their columns.
trace_record <- function(exploded, stu_id = NULL, class_id = NULL) {
  d <- exploded
  if (!is.null(stu_id))   d <- d[d$D_stu_id == stu_id, , drop = FALSE]
  if (!is.null(class_id)) d <- d[d$C_class_id == class_id, , drop = FALSE]
  stage_cols <- intersect(
    c("H_student_adjuster_same_course", "M_cls_class_weight", "M_class_weight_exploded",
      "M_class_weight_adj_same_course", "H_db_student_row", "H_db_teacher_row",
      "H_class_with_db", "M_class_weight_post_class",
      "M_class_weight_post_teachers", "M_class_weight_post_student",
      "M_student_count_adjusted", "M_tch_class_weight", "M_stu_class_weight"),
    names(d))
  keep <- c("D_stu_id", "D_employee_id", "C_class_id",
            intersect(c("C_class_id_consolidated", "C_class_id_post_teachers"), names(d)),
            "D_course_name", "C_term_exploded", "C_period_exploded", "C_rotation_exploded",
            stage_cols)
  d[, intersect(keep, names(d)), drop = FALSE] |>
    arrange(across(any_of(c("C_term_exploded", "C_period_exploded", "C_rotation_exploded"))))
}
