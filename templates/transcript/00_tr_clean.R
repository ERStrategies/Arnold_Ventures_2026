# =============================================================================
# 00_tr_clean.R — Cleaning primitives, all config-driven
# =============================================================================
# DISTRICT-AGNOSTIC. Every value these functions need comes from the config;
# no Nashville strings belong in this file.
# =============================================================================


# --- Dates -------------------------------------------------------------------

#' Parse a character date column, trying each supplied format in order.
#'
#' Unlike as.Date(), this reports how many values failed rather than quietly
#' returning NA. That silent NA is what broke M_days_enrolled in the old
#' pipeline: the exit dates were %m/%d/%Y and the code asked for %Y-%m-%d, so
#' every single date became NA and every downstream duration was NA too.
#'
#' @param x       Character vector.
#' @param formats One or more strptime formats, tried in order.
#' @param label   Name used in the check log.
#' @param allow_na Values already NA in the input are permitted. Values that
#'                 are present but unparseable are always a failure.
parse_date_strict <- function(x, formats, label, allow_na = TRUE) {

  raw <- as.character(x)
  raw[trimws(raw) == ""] <- NA_character_
  out <- as.Date(rep(NA_real_, length(raw)), origin = "1970-01-01")

  for (fmt in formats) {
    todo <- is.na(out) & !is.na(raw)
    if (!any(todo)) break
    out[todo] <- as.Date(raw[todo], format = fmt)
  }

  unparsed <- !is.na(raw) & is.na(out)

  chk_stop(paste0("Dates parse: ", label),
           !any(unparsed),
           value  = paste0(format(sum(!is.na(out)), big.mark = ","), " parsed"),
           detail = if (any(unparsed))
             paste0(format(sum(unparsed), big.mark = ","),
                    " value(s) present but unparseable with format(s) ",
                    paste(formats, collapse = " / ")),
           offenders = tibble::tibble(raw_value = utils::head(unique(raw[unparsed]), 50)))

  if (!allow_na) {
    chk_stop(paste0("Dates present: ", label), !any(is.na(out)),
             detail = paste0(format(sum(is.na(out)), big.mark = ","), " missing"))
  } else if (any(is.na(raw))) {
    chk_info(paste0("Dates missing: ", label),
             paste0(round(100 * mean(is.na(raw)), 1), "% (",
                    format(sum(is.na(raw)), big.mark = ","), " rows)"))
  }

  out
}

#' Replace missing dates with a fallback, keeping the Date class.
#'
#' dplyr::coalesce() rather than ifelse(). ifelse() strips the Date class and
#' turns the fallback into its numeric representation, which then fails to
#' re-parse. That is the exact bug this replaces.
fill_missing_date <- function(x, fallback, label = NULL) {
  n_filled <- sum(is.na(x) & !is.na(fallback))
  out <- dplyr::coalesce(x, fallback)
  if (!is.null(label)) {
    chk_info(paste0("Dates filled from fallback: ", label),
             paste0(format(n_filled, big.mark = ","), " (",
                    round(100 * n_filled / length(x), 1), "%)"))
  }
  out
}


# --- Location names ----------------------------------------------------------

#' Strip decoration from a location name without destroying real punctuation.
#'
#' @param x            Character vector of raw names.
#' @param strip_prefix Regex removed from the front (e.g. a year stamp).
#' @param strip_suffix Regex removed from the end (e.g. a grade span).
#' @param aliases      Named list, alias = canonical.
clean_location_name <- function(x, strip_prefix = NULL, strip_suffix = NULL,
                                aliases = NULL) {
  out <- as.character(x)
  if (!is.null(strip_prefix)) out <- sub(strip_prefix, "", out)
  if (!is.null(strip_suffix)) out <- sub(strip_suffix, "", out)
  out <- trimws(gsub("\\s+", " ", out))
  if (!is.null(aliases) && length(aliases) > 0) {
    hit <- match(out, names(aliases))
    out[!is.na(hit)] <- unlist(aliases)[hit[!is.na(hit)]]
  }
  out
}

#' Build a location_id -> canonical_name crosswalk and check it is coherent.
#'
#' School names carry a year stamp and are truncated, so they are a poor key.
#' The numeric code is stable. This resolves the config's human-readable list
#' to codes once, and stops if a listed school cannot be found — which is what
#' catches a misspelling like "Hillwood High School" that silently drops a
#' whole school from the cohort.
#'
#' @param data        Data with id_col and name_col (already cleaned).
#' @param include     Character vector of canonical names to resolve.
build_location_crosswalk <- function(data, id_col, name_col, include = NULL) {

  # Not every district supplies a stable location code. If it is absent, fall
  # back to the cleaned name as the key and say so, rather than failing here.
  if (!id_col %in% names(data)) {
    chk_info("No location code column; using cleaned name as the location key",
             id_col, detail = "string matching is fragile - request a code from the district")
    data[[id_col]] <- data[[name_col]]
  }

  xwalk <- data |>
    dplyr::count(.data[[id_col]], .data[[name_col]], name = "n_rows") |>
    dplyr::rename(location_id = 1, location_name = 2) |>
    dplyr::arrange(location_id, dplyr::desc(n_rows))

  chk_info("Location crosswalk built",
           paste0(dplyr::n_distinct(xwalk$location_id), " code(s), ",
                  dplyr::n_distinct(xwalk$location_name), " cleaned name(s)"))

  ambiguous <- xwalk |>
    dplyr::count(location_id, name = "n_names") |>
    dplyr::filter(n_names > 1)
  if (nrow(ambiguous) > 0) {
    chk_info("Location codes mapping to more than one cleaned name",
             nrow(ambiguous),
             data = xwalk |> dplyr::semi_join(ambiguous, by = "location_id"))
  }

  if (!is.null(include)) {
    resolved <- xwalk |>
      dplyr::filter(location_name %in% include) |>
      dplyr::distinct(location_id, location_name)

    unresolved <- setdiff(include, resolved$location_name)
    chk_stop("Every school in schools_to_include exists in the data",
             length(unresolved) == 0,
             value  = paste0(nrow(resolved), " of ", length(include), " resolved"),
             detail = if (length(unresolved) > 0)
               paste("Not found:", paste(unresolved, collapse = " | ")),
             offenders = tibble::tibble(school_not_found = unresolved))

    multi <- resolved |> dplyr::count(location_name, name = "n_codes") |>
      dplyr::filter(n_codes > 1)
    if (nrow(multi) > 0) {
      chk_info("Included schools with more than one location code",
               nrow(multi),
               data = resolved |> dplyr::semi_join(multi, by = "location_name"))
    }
    attr(xwalk, "included_ids") <- unique(resolved$location_id)
  }

  xwalk
}


# --- Value maps ---------------------------------------------------------------

#' Recode a column from a named list in the config, stopping on any value the
#' map does not cover. Silent pass-through of an unmapped code is how a new
#' race category or flag encoding slips into an analysis unnoticed.
apply_value_map <- function(x, map, label, allow_na = TRUE) {
  if (is.null(map) || length(map) == 0) return(x)
  observed <- unique(as.character(x))
  observed <- observed[!is.na(observed)]
  unmapped <- setdiff(observed, names(map))

  chk_stop(paste0("Value map covers all codes: ", label),
           length(unmapped) == 0,
           value  = paste0(length(observed), " distinct code(s)"),
           detail = if (length(unmapped) > 0)
             paste("Unmapped:", paste(unmapped, collapse = " | ")),
           offenders = tibble::tibble(unmapped_code = unmapped))

  out <- unlist(map)[as.character(x)]
  out <- unname(out)
  if (!allow_na) {
    chk_stop(paste0("No missing after recode: ", label), !any(is.na(out)))
  }
  out
}


# --- Numeric plausibility ------------------------------------------------------

#' Report, and optionally null out, values outside a plausible range.
#'
#' @param action "flag" records an info check and leaves values alone.
#'               "na"   replaces out-of-range values with NA.
#'               "stop" treats any out-of-range value as structural failure.
range_guard <- function(x, min = NULL, max = NULL, label, action = c("flag", "na", "stop")) {
  action <- match.arg(action)
  bad <- rep(FALSE, length(x))
  if (!is.null(min)) bad <- bad | (!is.na(x) & x < min)
  if (!is.null(max)) bad <- bad | (!is.na(x) & x > max)

  detail <- paste0("range allowed [", min %||% "-Inf", ", ", max %||% "Inf", "]")

  if (action == "stop") {
    chk_stop(paste0("Values in range: ", label), !any(bad), detail = detail,
             offenders = tibble::tibble(value = sort(unique(x[bad]))))
    return(x)
  }

  if (any(bad)) {
    chk_info(paste0("Out-of-range values: ", label),
             paste0(format(sum(bad), big.mark = ","), " row(s)"),
             detail = detail,
             data = tibble::tibble(value = sort(unique(x[bad]))) |>
               dplyr::mutate(n_rows = purrr::map_int(value, ~ sum(x == .x, na.rm = TRUE))))
  } else {
    chk_info(paste0("Out-of-range values: ", label), 0, detail = detail)
  }

  if (action == "na") x[bad] <- NA
  x
}


# --- Row filters ---------------------------------------------------------------

#' Apply the config's scope rules, reporting what each one costs.
#'
#' The old scripts filtered without saying how many rows went, which makes an
#' unintended filter invisible. Every rule here reports its own attrition.
apply_scope <- function(data, scope, id_col = NULL, sum_cols = NULL) {
  n0 <- nrow(data)
  ids0 <- if (!is.null(id_col)) dplyr::n_distinct(data[[id_col]]) else NA
  sums0 <- if (!is.null(sum_cols))
    vapply(sum_cols, function(c) sum(data[[c]], na.rm = TRUE), numeric(1)) else NULL

  for (col in scope$drop_when_null %||% character()) {
    if (!col %in% names(data)) {
      chk_stop(paste0("Scope column exists: ", col), FALSE)
    }
    before <- nrow(data)
    data <- data |> dplyr::filter(!is.na(.data[[col]]),
                                  trimws(as.character(.data[[col]])) != "")
    chk_info(paste0("Scope — drop null ", col),
             paste0(format(before - nrow(data), big.mark = ","), " row(s) dropped"))
  }

  for (expr in scope$filters %||% character()) {
    before <- nrow(data)
    data <- data |> dplyr::filter(!!rlang::parse_expr(expr))
    chk_info(paste0("Scope — filter ", expr),
             paste0(format(before - nrow(data), big.mark = ","), " row(s) dropped"))
  }

  chk_info("Scope — total retained",
           paste0(format(nrow(data), big.mark = ","), " of ",
                  format(n0, big.mark = ","), " rows (",
                  round(100 * nrow(data) / n0, 1), "%)"))
  if (!is.null(id_col)) {
    chk_info("Scope — distinct ids retained",
             paste0(format(dplyr::n_distinct(data[[id_col]]), big.mark = ","),
                    " of ", format(ids0, big.mark = ",")))
  }
  # Dropping a row removes whatever it was carrying. At scale that silently
  # changes totals, so report what each summed column lost.
  if (!is.null(sum_cols)) {
    sums1 <- vapply(sum_cols, function(c) sum(data[[c]], na.rm = TRUE), numeric(1))
    chk_info("Scope — analytic weight removed", "see table",
             data = tibble::tibble(column = sum_cols,
                                   before = sums0,
                                   after  = sums1,
                                   removed = sums0 - sums1))
  }
  data
}


# --- Canonical names -----------------------------------------------------------

#' Give every location code one display name: its most frequent cleaned name.
#'
#' School names drift between years and carry site variants (credit recovery,
#' truncations, abbreviations). Matching is done on the stable code; this is
#' purely so output reads sensibly.
resolve_canonical_names <- function(data, id_col, name_col, out_col = "C_location_name_canonical") {
  canon <- data |>
    dplyr::count(.data[[id_col]], .data[[name_col]], name = "n_rows") |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::slice_max(n_rows, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(c(id_col, name_col)))
  names(canon)[2] <- out_col

  n_renamed <- data |>
    dplyr::left_join(canon, by = id_col) |>
    dplyr::filter(.data[[name_col]] != .data[[out_col]]) |>
    nrow()

  chk_info("Location names normalised to canonical",
           paste0(format(n_renamed, big.mark = ","), " row(s) renamed"),
           detail = paste0(nrow(canon), " code(s) each given one display name"))

  dplyr::left_join(data, canon, by = id_col)
}


# --- Pattern-driven derivation --------------------------------------------------

#' Return the value of the first matching rule, scanning sources in order.
#'
#' Used for term detection: the S1/S2 marker sits on the course number for
#' 98.9% of rows and on the course name for 96.8%, so the number is tried
#' first and the name is the fallback.
#'
#' @param sources Named list of character vectors, tried in order.
#' @param rules   List of list(pattern=, value=).
#' @param label   Name used in the check log.
match_first_pattern <- function(sources, rules, label, ignore_case = TRUE) {
  n <- length(sources[[1]])
  out <- rep(NA_character_, n)
  found_in <- rep(NA_character_, n)

  for (src_name in names(sources)) {
    x <- as.character(sources[[src_name]])
    for (rule in rules) {
      todo <- is.na(out)
      if (!any(todo)) break
      hit <- todo & grepl(rule$pattern, x, ignore.case = ignore_case, perl = TRUE)
      out[hit] <- rule$value
      found_in[hit] <- src_name
    }
  }

  chk_info(paste0("Derived ", label),
           paste0(round(100 * mean(!is.na(out)), 1), "% resolved"),
           data = tibble::tibble(value = out, source = found_in) |>
             dplyr::count(value, source, name = "n_rows") |>
             dplyr::arrange(dplyr::desc(n_rows)))
  out
}


# --- Range guard with tolerance --------------------------------------------------

#' Report out-of-range values, and stop if they exceed a tolerated share.
#'
#' "flag" on its own records a note and passes the value through, which means
#' an impossible value still lands in any downstream mean. tolerance_pct is
#' what turns a flag into something that demands attention: a handful of typos
#' is a note, a systematic problem halts the run.
#'
#' Returns a list(values = , flag = ) so the caller can keep both.
range_guard_flagged <- function(x, min = NULL, max = NULL, label,
                                action = c("flag", "na", "stop"),
                                tolerance_pct = NULL) {
  action <- match.arg(action)
  bad <- rep(FALSE, length(x))
  if (!is.null(min)) bad <- bad | (!is.na(x) & x < min)
  if (!is.null(max)) bad <- bad | (!is.na(x) & x > max)
  pct <- 100 * mean(bad)

  offenders <- tibble::tibble(value = sort(unique(x[bad]))) |>
    dplyr::mutate(n_rows = purrr::map_int(value, ~ sum(x == .x, na.rm = TRUE)))

  chk_info(paste0("Out-of-range: ", label),
           paste0(format(sum(bad), big.mark = ","), " row(s), ", round(pct, 3), "%"),
           detail = paste0("allowed [", min %||% "-Inf", ", ", max %||% "Inf", "]"),
           data = if (any(bad)) offenders else NULL)

  if (action == "stop") {
    chk_stop(paste0("Values in range: ", label), !any(bad), offenders = offenders)
  } else if (!is.null(tolerance_pct)) {
    chk_stop(paste0("Out-of-range within tolerance: ", label),
             pct <= tolerance_pct,
             value  = paste0(round(pct, 3), "%"),
             detail = paste0("tolerance is ", tolerance_pct, "%"),
             offenders = offenders)
  }

  if (action == "na") x[bad] <- NA
  list(values = x, flag = bad)
}


# --- School presence across years -----------------------------------------------

#' Check that in-scope locations appear in the years they are expected in.
#'
#' Schools open, close, merge and get renamed. A missing school-year is
#' sometimes a data problem and sometimes a fact about the district, and the
#' pipeline cannot tell the difference. So the expected years are declared in
#' config: an undeclared gap stops the run, a declared one is reported with
#' its stated reason.
#'
#' @param data        Rows carrying location, name and year.
#' @param id_col,name_col,year_col Column names.
#' @param include_ids Location codes in scope.
#' @param expectations Config block: list(default=, overrides=list(<name>=list(years=, reason=))).
check_school_year_coverage <- function(data, id_col, name_col, year_col,
                                       include_ids, expectations = NULL) {

  all_years <- sort(unique(data[[year_col]]))

  observed <- data |>
    dplyr::filter(.data[[id_col]] %in% include_ids) |>
    dplyr::count(.data[[id_col]], .data[[name_col]], .data[[year_col]], name = "n_rows") |>
    dplyr::rename(location_id = 1, location_name = 2, year = 3)

  schools <- observed |> dplyr::distinct(location_id, location_name)

  overrides <- expectations$overrides %||% list()

  gaps <- purrr::pmap_dfr(schools, function(location_id, location_name) {
    expected <- if (!is.null(overrides[[location_name]]$years)) {
      as.integer(unlist(overrides[[location_name]]$years))
    } else {
      all_years
    }
    seen <- observed$year[observed$location_id == location_id]
    tibble::tibble(
      location_id   = location_id,
      location_name = location_name,
      missing_year  = setdiff(expected, seen),
      declared      = FALSE
    ) |>
      dplyr::bind_rows(tibble::tibble(
        location_id   = location_id,
        location_name = location_name,
        missing_year  = setdiff(all_years, union(expected, seen)),
        declared      = TRUE))
  })

  undeclared <- gaps |> dplyr::filter(!declared)
  declared   <- gaps |> dplyr::filter(declared)

  presence <- observed |>
    dplyr::select(location_id, location_name, year, n_rows) |>
    tidyr::pivot_wider(names_from = year, values_from = n_rows,
                       names_prefix = "y", values_fill = 0) |>
    dplyr::arrange(location_name)

  chk_info("School presence by year", paste0(nrow(schools), " in-scope location(s)"),
           data = presence, n_show = 50)

  if (nrow(declared) > 0) {
    reasons <- purrr::map_chr(declared$location_name,
                              ~ overrides[[.x]]$reason %||% "declared in config")
    chk_info("Declared school-year absences", nrow(declared),
             data = declared |> dplyr::mutate(reason = reasons) |>
               dplyr::select(location_name, missing_year, reason))
  }

  chk_stop("No undeclared school-year gaps",
           nrow(undeclared) == 0,
           value  = paste0(nrow(schools), " school(s) x ", length(all_years), " year(s)"),
           detail = if (nrow(undeclared) > 0)
             paste0(nrow(undeclared), " school-year(s) missing with no entry in ",
                    "cohort$school_year_expectations$overrides"),
           offenders = undeclared |> dplyr::select(-declared))

  presence
}

#' Confirm that locations declared as related are genuinely distinct codes.
#'
#' A successor school is not the same school. This guards against a future
#' extract reusing a code, which would silently merge two institutions.
check_locations_distinct <- function(xwalk, related) {
  if (is.null(related) || length(related) == 0) return(invisible(NULL))

  pairs <- purrr::map_dfr(related, function(r) {
    ids <- function(nm) unique(xwalk$location_id[xwalk$location_name == nm])
    tibble::tibble(from = r$from, to = r$to,
                   relationship = r$relationship %||% "related",
                   merge = isTRUE(r$merge),
                   from_ids = paste(ids(r$from), collapse = "/"),
                   to_ids   = paste(ids(r$to),   collapse = "/"))
  })

  chk_info("Declared location relationships", nrow(pairs), data = pairs)

  shared <- pairs |> dplyr::filter(!merge, from_ids == to_ids, from_ids != "")
  chk_stop("Related locations that should stay separate have distinct codes",
           nrow(shared) == 0,
           detail = if (nrow(shared) > 0)
             "a pair marked merge: false shares one location code",
           offenders = shared)
  pairs
}


# --- Location merges -------------------------------------------------------------

#' Combine locations that are one institution across time.
#'
#' Schools relocate, consolidate and get renamed. When a district moves a whole
#' campus - students and staff together - the old and new codes describe one
#' continuous institution, and treating them separately would make every
#' enrolled student look like a transfer.
#'
#' This remaps absorbed location codes onto a surviving one, leaving the raw
#' code untouched so nothing is lost. Downstream logic uses the merged code.
#'
#' Two things are verified rather than assumed:
#'   - each named location resolves to exactly one code, so a typo cannot
#'     silently merge nothing
#'   - merged locations do not overlap in time, because a genuine relocation
#'     means the old site stops as the new one starts. Overlap suggests two
#'     coexisting schools, which is a different situation and probably should
#'     not be merged. Set allow_overlap: true in config to permit it.
#'
#' @param merges Config list of list(name=, absorbs=, effective_year=, reason=,
#'   allow_overlap=).
#' @return data with C_location_id and C_location_name_merged added.
apply_location_merges <- function(data, id_col, name_col, year_col, merges = NULL) {

  data[["C_location_id"]]          <- data[[id_col]]
  data[["C_location_name_merged"]] <- data[[name_col]]

  if (is.null(merges) || length(merges) == 0) {
    chk_info("Location merges applied", 0, detail = "none configured")
    return(data)
  }

  ids_for <- function(nm) unique(data[[id_col]][data[[name_col]] == nm])

  summary_rows <- list()

  for (m in merges) {
    survivor_ids <- ids_for(m$name)
    chk_stop(paste0("Merge target resolves to one location code: ", m$name),
             length(survivor_ids) == 1,
             value  = paste(survivor_ids, collapse = ", "),
             detail = if (length(survivor_ids) != 1)
               paste0("found ", length(survivor_ids), " code(s); expected exactly 1"))
    survivor_id <- survivor_ids[[1]]

    for (absorbed in unlist(m$absorbs)) {
      absorbed_ids <- ids_for(absorbed)
      chk_stop(paste0("Absorbed location exists: ", absorbed),
               length(absorbed_ids) >= 1,
               detail = "name not found in the data; check spelling against the crosswalk")

      # A relocation means the sites do not run at the same time.
      yrs_survivor <- unique(data[[year_col]][data[[id_col]] == survivor_id])
      yrs_absorbed <- unique(data[[year_col]][data[[id_col]] %in% absorbed_ids])
      overlap <- intersect(yrs_survivor, yrs_absorbed)

      if (isTRUE(m$allow_overlap)) {
        chk_info(paste0("Merge overlap permitted: ", absorbed, " -> ", m$name),
                 paste(sort(overlap), collapse = ", "))
      } else {
        chk_stop(paste0("Merged locations do not overlap in time: ",
                        absorbed, " -> ", m$name),
                 length(overlap) == 0,
                 value  = paste0(absorbed, ": ", paste(sort(yrs_absorbed), collapse = ", "),
                                 " | ", m$name, ": ", paste(sort(yrs_survivor), collapse = ", ")),
                 detail = if (length(overlap) > 0)
                   paste0("both operate in ", paste(sort(overlap), collapse = ", "),
                          "; if they genuinely coexisted they are probably two ",
                          "schools, not one. Set allow_overlap: true to proceed."))
      }

      hit <- data[[id_col]] %in% absorbed_ids
      data[["C_location_id"]][hit]          <- survivor_id
      data[["C_location_name_merged"]][hit] <- m$name

      summary_rows[[length(summary_rows) + 1]] <- tibble::tibble(
        absorbed       = absorbed,
        absorbed_id    = paste(absorbed_ids, collapse = "/"),
        absorbed_years = paste(sort(yrs_absorbed), collapse = ", "),
        into           = m$name,
        into_id        = survivor_id,
        into_years     = paste(sort(yrs_survivor), collapse = ", "),
        rows_remapped  = sum(hit),
        reason         = m$reason %||% NA_character_
      )
    }
  }

  merge_summary <- dplyr::bind_rows(summary_rows)
  chk_info("Location merges applied", nrow(merge_summary),
           value = paste0(format(sum(merge_summary$rows_remapped), big.mark = ","),
                          " row(s) remapped"),
           data  = merge_summary |> dplyr::select(-reason))

  attr(data, "merge_summary") <- merge_summary
  data
}


# --- Two-tier range description ---------------------------------------------------

#' Describe a numeric column against plausible and hard bounds.
#'
#' Two tiers, because "outside what I expected" and "cannot be a real
#' measurement" are different questions and deserve different consequences.
#' A grade of 100.05 is extra credit; a grade of 981 is not a grade. Collapsing
#' both into one bound forces a false choice between halting on the former and
#' ignoring the latter.
#'
#' The plausible tier only ever reports. The hard tier reports, and gates if
#' hard_tolerance_pct is set.
#'
#' @param spec Config entry: list(plausible=, hard=, hard_action=, hard_tolerance_pct=).
#' @return list(values=, flag_plausible=, flag_hard=)
range_two_tier <- function(x, spec, label) {

  out_of <- function(v, b) {
    bad <- rep(FALSE, length(v))
    if (!is.null(b$min)) bad <- bad | (!is.na(v) & v < b$min)
    if (!is.null(b$max)) bad <- bad | (!is.na(v) & v > b$max)
    bad
  }

  flag_plausible <- out_of(x, spec$plausible)
  flag_hard      <- out_of(x, spec$hard)

  chk_info(paste0("Outside plausible range: ", label),
           paste0(format(sum(flag_plausible), big.mark = ","), " row(s), ",
                  round(100 * mean(flag_plausible), 3), "%"),
           detail = paste0("plausible [", spec$plausible$min, ", ",
                           spec$plausible$max, "] — reported, never halts"))

  chk_info(paste0("Outside hard range: ", label),
           paste0(format(sum(flag_hard), big.mark = ","), " row(s), ",
                  round(100 * mean(flag_hard), 3), "%"),
           detail = paste0("hard [", spec$hard$min, ", ", spec$hard$max, "]"),
           data = if (any(flag_hard))
             tibble::tibble(value = sort(unique(x[flag_hard]))) |>
               dplyr::mutate(n_rows = purrr::map_int(value, ~ sum(x == .x, na.rm = TRUE)))
           else NULL)

  action <- spec$hard_action %||% "flag"
  tol    <- spec$hard_tolerance_pct

  if (action == "stop") {
    chk_stop(paste0("No values outside hard range: ", label), !any(flag_hard))
  } else if (!is.null(tol)) {
    pct <- 100 * mean(flag_hard)
    chk_stop(paste0("Hard-range breaches within tolerance: ", label),
             pct <= tol,
             value  = paste0(round(pct, 3), "%"),
             detail = paste0("tolerance is ", tol, "%; set ranges$", label,
                             "$hard or $hard_tolerance_pct in config"),
             offenders = tibble::tibble(value = sort(unique(x[flag_hard]))))
  }

  if (action == "na") x[flag_hard] <- NA

  list(values = x, flag_plausible = flag_plausible, flag_hard = flag_hard)
}

#' Bucket the tail of a column above a threshold, for reading its shape.
describe_tail <- function(x, breaks, label) {
  tail_vals <- x[!is.na(x) & x > breaks[1]]
  if (length(tail_vals) == 0) {
    chk_info(paste0("Tail above ", breaks[1], ": ", label), 0)
    return(invisible(NULL))
  }
  out <- tibble::tibble(value = tail_vals) |>
    dplyr::mutate(bucket = cut(value, breaks = breaks, right = FALSE,
                               dig.lab = 6, include.lowest = TRUE)) |>
    dplyr::count(bucket, name = "n_rows") |>
    dplyr::mutate(pct_of_all = round(100 * n_rows / length(x), 3))

  chk_info(paste0("Tail above ", breaks[1], ": ", label),
           paste0(format(length(tail_vals), big.mark = ","), " row(s)"),
           data = out, n_show = 30)
  out
}


# --- Cohort selection machinery ---------------------------------------------------

#' Reduce a per-year condition to one verdict per student.
#'
#' Cohort rules are almost always "in every year" or "in the first and last
#' year", and which one is a methodology decision rather than a fact. Keeping
#' the reduction in one place means the choice is declared in config once and
#' applied identically to duration, progression and school.
#'
#' @param by_year Data with id_col, year_col and a logical `ok` column.
#' @param rule    all_years | bookends | none.
#' @param years   The full set of years the cohort must span.
#' @return tibble of id_col and `passes`.
summarise_year_rule <- function(by_year, id_col, year_col, rule, years) {
  years <- sort(unique(years))
  required <- switch(rule,
    all_years = years,
    bookends  = c(min(years), max(years)),
    none      = integer(0),
    stop("Unknown year rule: ", rule, call. = FALSE))

  if (length(required) == 0) {
    return(by_year |> dplyr::distinct(dplyr::across(dplyr::all_of(id_col))) |>
             dplyr::mutate(passes = TRUE))
  }

  by_year |>
    dplyr::filter(.data[[year_col]] %in% required) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_col))) |>
    dplyr::summarise(
      years_present = dplyr::n_distinct(.data[[year_col]]),
      years_ok      = sum(ok, na.rm = TRUE),
      .groups = "drop") |>
    dplyr::mutate(passes = years_present == length(required) &
                    years_ok == length(required)) |>
    dplyr::select(dplyr::all_of(id_col), passes)
}


#' Start an attrition funnel.
#'
#' Every cohort rule removes students, and a rule that removes far more than
#' expected is the single easiest way to publish a cohort that quietly
#' describes a narrower population than it claims to. The funnel makes each
#' step's cost visible, in sequence, against the starting universe.
funnel_new <- function(ids, label = "Starting universe") {
  structure(list(
    ids   = unique(ids),
    start = length(unique(ids)),
    log   = tibble::tibble(
      step             = 1L,
      criterion        = label,
      students         = length(unique(ids)),
      dropped          = NA_integer_,
      pct_of_previous  = NA_real_,
      pct_of_start     = 100)
  ), class = "cohort_funnel")
}

#' Apply one criterion and record what it cost.
#'
#' @param passing Vector of ids that pass this criterion.
funnel_add <- function(funnel, label, passing) {
  before <- length(funnel$ids)
  funnel$ids <- intersect(funnel$ids, unique(passing))
  after  <- length(funnel$ids)

  funnel$log <- dplyr::bind_rows(funnel$log, tibble::tibble(
    step            = nrow(funnel$log) + 1L,
    criterion       = label,
    students        = after,
    dropped         = before - after,
    pct_of_previous = round(100 * after / before, 1),
    pct_of_start    = round(100 * after / funnel$start, 1)))

  chk_info(paste0("Cohort — ", label),
           paste0(format(after, big.mark = ","), " remain (",
                  format(before - after, big.mark = ","), " dropped, ",
                  round(100 * after / before, 1), "% retained)"))
  funnel
}

#' Compare each step's retention against a benchmark.
funnel_report <- function(funnel, benchmark_pct = NULL) {
  out <- funnel$log
  if (!is.null(benchmark_pct)) {
    out <- out |>
      dplyr::mutate(vs_benchmark = dplyr::if_else(
        is.na(pct_of_previous), NA_real_,
        round(pct_of_previous - benchmark_pct, 1)))
  }
  out
}