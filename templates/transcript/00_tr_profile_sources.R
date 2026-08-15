# =============================================================================
# profile_sources.R — One-time diagnostic before building Stage 2
# =============================================================================
# Run once, paste the console output back.
#
# Prints structure and value distributions. Deliberately does NOT print
# student IDs, student names, or any row-level record. Course names and IDs
# are printed, which is permitted (the course catalog is designated
# non-confidential in Appendix A of the DSA).
#
#   source(here::here("R", "setup.R"))
#   config <- load_config("nashville")
#   source(here::here("R", "profile_sources.R"))
# =============================================================================

stopifnot(exists("load_config"))

hdr <- function(x) cat("\n", strrep("=", 72), "\n", x, "\n", strrep("=", 72), "\n", sep = "")
sub <- function(x) cat("\n--- ", x, " ", strrep("-", max(0, 60 - nchar(x))), "\n", sep = "")

# Read raw, unrenamed, so we see the district's own columns and types.
tr <- read_source("transcript_raw",   config, refresh = FALSE)
dm <- read_source("stu_demographics", config, refresh = FALSE)

# ---------------------------------------------------------------------------
# 1. Shape and column inventory
# ---------------------------------------------------------------------------
inventory <- function(d, label) {
  sub(paste0(label, ": ", format(nrow(d), big.mark = ","), " rows x ", ncol(d), " cols"))
  purrr::imap_dfr(d, function(col, nm) {
    tibble::tibble(
      column      = nm,
      type        = paste(class(col), collapse = "/"),
      n_distinct  = dplyr::n_distinct(col),
      pct_missing = round(100 * mean(is.na(col) | trimws(as.character(col)) == ""), 1)
    )
  }) |> as.data.frame() |> print()
}

hdr("1. COLUMN INVENTORY")
inventory(tr, "TRANSCRIPT")
inventory(dm, "DEMOGRAPHICS")

# ---------------------------------------------------------------------------
# 2. Year encoding  — the join hinges on this
# ---------------------------------------------------------------------------
hdr("2. YEAR ENCODING")
for (nm in c("start_year", "end_year")) {
  if (nm %in% names(tr)) {
    sub(paste0("transcript$", nm, "  [", paste(class(tr[[nm]]), collapse = "/"), "]"))
    print(as.data.frame(dplyr::count(tr, .data[[nm]], name = "n_rows")))
  }
}
if ("year" %in% names(dm)) {
  sub(paste0("demographics$year  [", paste(class(dm$year), collapse = "/"), "]"))
  print(as.data.frame(dplyr::count(dm, year, name = "n_rows")))
}

# ---------------------------------------------------------------------------
# 3. Grade level  — numeric 9 or character "09"?
# ---------------------------------------------------------------------------
hdr("3. GRADE LEVEL ENCODING")
if ("gradelevel" %in% names(tr)) {
  sub(paste0("transcript$gradelevel  [", paste(class(tr$gradelevel), collapse = "/"), "]"))
  print(as.data.frame(dplyr::count(tr, gradelevel, name = "n_rows")))
}
if ("grade" %in% names(dm)) {
  sub(paste0("demographics$grade  [", paste(class(dm$grade), collapse = "/"), "]"))
  print(as.data.frame(dplyr::count(dm, grade, name = "n_rows")))
}

# ---------------------------------------------------------------------------
# 4. Demographic flags  — "1"/"0"? "Y"/"N"? numeric?
# ---------------------------------------------------------------------------
hdr("4. FLAG AND CATEGORY ENCODING")
for (nm in c("ell", "sped", "ed", "race")) {
  if (nm %in% names(dm)) {
    sub(paste0("demographics$", nm, "  [", paste(class(dm[[nm]]), collapse = "/"), "]"))
    print(as.data.frame(dplyr::count(dm, .data[[nm]], name = "n_rows")))
  }
}

# ---------------------------------------------------------------------------
# 5. Dates  — raw strings, before any parsing
# ---------------------------------------------------------------------------
hdr("5. DATE FORMATS (raw, unparsed)")
for (nm in c("edt", "wdt")) {
  if (nm %in% names(dm)) {
    v <- as.character(dm[[nm]])
    sub(paste0("demographics$", nm, "  [", paste(class(dm[[nm]]), collapse = "/"), "]"))
    cat("  pct missing : ", round(100 * mean(is.na(v) | trimws(v) == ""), 1), "%\n", sep = "")
    cat("  distinct    : ", dplyr::n_distinct(v), "\n", sep = "")
    cat("  sample      : ", paste(utils::head(unique(stats::na.omit(v)), 8), collapse = " | "), "\n", sep = "")
    cat("  char widths : ", paste(sort(unique(nchar(v))), collapse = ", "), "\n", sep = "")
  }
}

# ---------------------------------------------------------------------------
# 6. Grain  — how many rows per student per year in each file
# ---------------------------------------------------------------------------
hdr("6. FILE GRAIN")

sub("Demographics: rows per student per year")
dm |>
  dplyr::count(cid, year, name = "rows") |>
  dplyr::count(rows, name = "n_student_years") |>
  dplyr::arrange(rows) |> as.data.frame() |> print()

sub("Demographics: distinct schools per student per year")
if ("school_name" %in% names(dm)) {
  dm |>
    dplyr::group_by(cid, year) |>
    dplyr::summarise(n_schools = dplyr::n_distinct(school_name), .groups = "drop") |>
    dplyr::count(n_schools, name = "n_student_years") |>
    dplyr::arrange(n_schools) |> as.data.frame() |> print()
}

sub("Transcript: rows per student per end_year per course_number")
if (all(c("cid", "end_year", "course_number") %in% names(tr))) {
  tr |>
    dplyr::count(cid, end_year, course_number, name = "rows") |>
    dplyr::count(rows, name = "n_combinations") |>
    dplyr::arrange(rows) |> as.data.frame() |> print()
}

sub("Transcript: fully duplicated rows")
cat("  ", format(sum(duplicated(tr)), big.mark = ","), " of ",
    format(nrow(tr), big.mark = ","), "\n", sep = "")

# ---------------------------------------------------------------------------
# 7. Course identifiers  — where does the S1/S2 suffix actually live?
# ---------------------------------------------------------------------------
hdr("7. COURSE IDENTIFIERS")

sub("Suffix location")
cat("  course_number ending S1/S2 : ",
    sum(grepl("(S1|S2)\\s*$", as.character(tr$course_number), ignore.case = TRUE)), "\n", sep = "")
cat("  course_name   ending S1/S2 : ",
    sum(grepl("(S1|S2)\\s*$", as.character(tr$course_name),   ignore.case = TRUE)), "\n", sep = "")

sub("Last 2 characters of course_name")
tr |>
  dplyr::mutate(last2 = stringr::str_sub(course_name, -2)) |>
  dplyr::count(last2, sort = TRUE, name = "n_rows") |>
  utils::head(20) |> as.data.frame() |> print()

sub("Sample course_number values")
cat("  ", paste(utils::head(unique(as.character(tr$course_number)), 15), collapse = " | "), "\n", sep = "")
cat("  char widths: ", paste(sort(unique(nchar(as.character(tr$course_number)))), collapse = ", "), "\n", sep = "")

sub("Course ID to name: is the mapping one-to-one?")
tr |>
  dplyr::distinct(course_number, course_name) |>
  dplyr::count(course_number, name = "n_names") |>
  dplyr::count(n_names, name = "n_course_ids") |>
  dplyr::arrange(n_names) |> as.data.frame() |> print()

# ---------------------------------------------------------------------------
# 8. Credits  — sanity on the semester/credit relationship
# ---------------------------------------------------------------------------
hdr("8. CREDITS")
for (nm in c("credits_earned", "credits_attempted")) {
  if (nm %in% names(tr)) {
    sub(paste0("transcript$", nm, "  [", paste(class(tr[[nm]]), collapse = "/"), "]"))
    print(as.data.frame(dplyr::count(tr, .data[[nm]], sort = TRUE, name = "n_rows") |> utils::head(15)))
  }
}
if ("percent" %in% names(tr)) {
  sub("transcript$percent (D_stu_grade)")
  cat("  type    : ", paste(class(tr$percent), collapse = "/"), "\n", sep = "")
  cat("  distinct: ", dplyr::n_distinct(tr$percent), "\n", sep = "")
  print(summary(suppressWarnings(as.numeric(tr$percent))))
}

# ---------------------------------------------------------------------------
# 9. Student overlap between files  — counts only
# ---------------------------------------------------------------------------
hdr("9. STUDENT OVERLAP (counts only)")
tr_ids <- unique(as.character(tr$cid))
dm_ids <- unique(as.character(dm$cid))
cat("  transcript students        : ", format(length(tr_ids), big.mark = ","), "\n", sep = "")
cat("  demographics students      : ", format(length(dm_ids), big.mark = ","), "\n", sep = "")
cat("  in both                    : ", format(length(intersect(tr_ids, dm_ids)), big.mark = ","), "\n", sep = "")
cat("  transcript only            : ", format(length(setdiff(tr_ids, dm_ids)), big.mark = ","), "\n", sep = "")
cat("  demographics only          : ", format(length(setdiff(dm_ids, tr_ids)), big.mark = ","), "\n", sep = "")
cat("  id char widths, transcript : ", paste(sort(unique(nchar(tr_ids))), collapse = ", "), "\n", sep = "")
cat("  id char widths, demogs     : ", paste(sort(unique(nchar(dm_ids))), collapse = ", "), "\n", sep = "")

# ---------------------------------------------------------------------------
# 10. School names  — check against the config list
# ---------------------------------------------------------------------------
hdr("10. SCHOOL NAMES IN DATA vs CONFIG")
if ("school_name" %in% names(dm)) {
  in_data   <- sort(unique(as.character(dm$school_name)))
  cat("  distinct school_name values: ", length(in_data), "\n\n", sep = "")
  cleaned <- trimws(gsub(config$derived$location_name_strip_pattern, "", in_data))
  tibble::tibble(
    school_name_raw = in_data,
    cleaned         = cleaned,
    in_config       = cleaned %in% config$cohort$schools_to_include |
                      cleaned %in% names(config$cohort$school_name_aliases)
  ) |> as.data.frame() |> print()
}

hdr("PROFILE COMPLETE")
