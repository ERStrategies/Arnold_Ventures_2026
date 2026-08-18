# =============================================================================
# 00_weight_helpers.R
# Derive class WEIGHT (a dimensionless load-share, for double-booking
# resolution) and class MINUTES (real clock time, for instructional hours) —
# kept rigorously separate, because weight is unitless and minutes are the unit.
#
#   class_weight  = SUM over meetings ( term_wt x rotation_wt x period_wt )
#   class_minutes = SUM over meetings ( bell_minutes )
#
#   term     : fraction of the year (D_term -> config$weights$term)
#   rotation : per A/B day; per-SCHOOL per-meeting overrides (e.g. weekly advisory)
#   period   : normal slot = 1.0, length-INDEPENDENT; per-school block overrides
#   minutes  : real minutes from the bell schedule, per school / meeting
#
# Depends on 00_expression_helpers.R (parse_expression / expr_spec).
# =============================================================================

suppressPackageStartupMessages({ library(dplyr) })

`%||%` <- function(a, b) if (is.null(a)) b else a

# Build fast lookups from config (one place, so a bad config surfaces here).
weight_lookups <- function(config) {
  list(
    term       = config$weights$term,
    rot_def    = config$weights$rotation$default %||% 0.5,
    rot_school = config$weights$rotation$by_school,
    per_def    = config$weights$period$default %||% 1.0,
    per_period = config$weights$period$by_period,
    per_school = config$weights$period$by_school,
    bell_def   = config$bell_minutes$default %||% NA_real_,
    bell       = config$bell_minutes$by_school
  )
}

.rot_w <- function(meeting, school, W) {
  s <- W$rot_school[[school]]
  if (!is.null(s) && !is.null(s[[meeting]])) return(as.numeric(s[[meeting]]))
  as.numeric(W$rot_def)
}
.per_w <- function(meeting, school, W) {
  # Extract just the period token (before "@" for period_rotation format)
  period_token <- sub("@.*$", "", meeting)
  # by_period takes highest precedence (applies across all schools)
  if (!is.null(W$per_period) && !is.null(W$per_period[[period_token]])) {
    return(as.numeric(W$per_period[[period_token]]))
  }
  # by_school next
  s <- W$per_school[[school]]
  if (!is.null(s) && !is.null(s[[meeting]])) return(as.numeric(s[[meeting]]))
  as.numeric(W$per_def)
}
.bell_min <- function(meeting, school, W) {
  s <- W$bell[[school]]
  if (!is.null(s)) {
    if (!is.null(s$overrides) && !is.null(s$overrides[[meeting]])) return(as.numeric(s$overrides[[meeting]]))
    if (!is.null(s$default)) return(as.numeric(s$default))
  }
  if (!is.na(W$bell_def)) return(as.numeric(W$bell_def))
  NA_real_
}

.term_w <- function(term, W) {
  v <- W$term[[as.character(term)]]
  if (is.null(v)) NA_real_ else as.numeric(v)
}

# Per-meeting breakdown for one expression + school (for the review's worked
# examples): meeting, rotation_wt, period_wt, minutes.
meeting_breakdown <- function(expr, school, config, spec) {
  W <- weight_lookups(config)
  m <- parse_expression(expr, spec)
  if (!length(m)) return(data.frame())
  data.frame(
    meeting     = m,
    rotation_wt = vapply(m, .rot_w, numeric(1), school = school, W = W),
    period_wt   = vapply(m, .per_w, numeric(1), school = school, W = W),
    minutes     = vapply(m, .bell_min, numeric(1), school = school, W = W),
    row.names = NULL, stringsAsFactors = FALSE)
}

# Add M_cls_class_weight and M_cls_class_minutes to a data frame. Constant within
# a class id (expression/term/school are constant within a class id). Computed
# once per distinct (time cols, school) then multiplied by the per-row term wt.
derive_class_weight_minutes <- function(df, config, spec,
                                        school_col = "D_location_name",
                                        term_col   = "D_term") {
  W         <- weight_lookups(config)
  time_cols <- time_key_cols(config)
  df        <- as.data.frame(df)

  combos <- unique(df[, c(time_cols, school_col), drop = FALSE])
  combos$.rp  <- NA_real_
  combos$.min <- NA_real_

  for (i in seq_len(nrow(combos))) {
    school <- combos[[school_col]][i]
    m <- record_meetings(as.list(combos[i, time_cols]), config, spec)
    if (!length(m)) { combos$.rp[i] <- 0; combos$.min[i] <- 0; next }
    combos$.rp[i]  <- sum(vapply(m, function(x) .rot_w(x, school, W) * .per_w(x, school, W), numeric(1)))
    combos$.min[i] <- sum(vapply(m, function(x) .bell_min(x, school, W), numeric(1)))
  }

  df <- left_join(df, combos, by = c(time_cols, school_col))
  tw <- vapply(df[[term_col]], .term_w, numeric(1), W = W)
  df$M_cls_class_weight  <- round(tw * df$.rp, 4)
  df$M_cls_class_minutes <- df$.min
  df$.rp <- NULL; df$.min <- NULL
  df
}
