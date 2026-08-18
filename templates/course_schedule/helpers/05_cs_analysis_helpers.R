# =============================================================================
# 05_cs_cohort_helpers.R
#
# Helpers for analyzing 9th-grade student cohorting.
#
# Population decisions (which course names count as 9th core / seminar)
# should be made upstream in the .qmd and passed into these functions.
#
# Required packages:
#   dplyr
#   tidyr
#   purrr
#   igraph
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(igraph)
})


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

.mean_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}


.class_roster <- function(cohort_schedule) {
  cohort_schedule |>
    distinct(
      D_location_name,
      C_class_id,
      D_stu_id
    )
}


# =============================================================================
# 1. BUILD 9TH-GRADE COHORTING SCHEDULE
# =============================================================================

build_ninth_cohort_schedule <- function(
    student_rollup,
    core_ninth_courses,
    seminar_ninth_courses,
    s1_term = "S1",
    cohort_subjects = c(
      "ELA",
      "Math",
      "Science",
      "Social Studies",
      "Advisory"
    )
) {

  ninth_cohort_courses <- union(
    core_ninth_courses,
    seminar_ninth_courses
  )

  student_rollup |>
    filter(
      C_course_name %in% ninth_cohort_courses,
      C_course_subject %in% cohort_subjects,
      D_term == s1_term
    ) |>
    transmute(
      D_location_name,
      D_stu_id = as.character(D_stu_id),
      C_class_id = as.character(C_class_id),
      D_term,
      C_course_subject,
      C_course_name
    ) |>
    distinct()
}


# =============================================================================
# 2. BUILD STUDENT-PAIR NETWORK
# =============================================================================

build_student_pairs <- function(cohort_schedule) {

  class_roster <- .class_roster(cohort_schedule)

  # Every pair of students who share a class
  student_pairs_by_class <- class_roster |>
    rename(D_stu_id_1 = D_stu_id) |>
    inner_join(
      class_roster |>
        rename(D_stu_id_2 = D_stu_id),
      by = c(
        "D_location_name",
        "C_class_id"
      ),
      relationship = "many-to-many"
    ) |>
    filter(D_stu_id_1 < D_stu_id_2) |>
    distinct(
      D_location_name,
      C_class_id,
      D_stu_id_1,
      D_stu_id_2
    )

  # Strength of relationship = number of distinct classes shared
  student_pairs_by_class |>
    count(
      D_location_name,
      D_stu_id_1,
      D_stu_id_2,
      name = "M_shared_classes"
    )
}


# =============================================================================
# 3. INFER STUDENT COHORTS
# =============================================================================

infer_ninth_cohorts <- function(
    cohort_schedule,
    student_pairs,
    seed = 1234
) {

  set.seed(seed)

  class_roster <- .class_roster(cohort_schedule)

  infer_school <- function(school_name) {

    vertices <- class_roster |>
      filter(D_location_name == school_name) |>
      distinct(D_stu_id) |>
      transmute(name = D_stu_id)

    edges <- student_pairs |>
      filter(D_location_name == school_name) |>
      transmute(
        from = D_stu_id_1,
        to = D_stu_id_2,
        weight = M_shared_classes
      )

    g <- igraph::graph_from_data_frame(
      edges,
      directed = FALSE,
      vertices = vertices
    )

    # If the school has no shared-class relationships,
    # treat each student as their own community.
    if (igraph::ecount(g) == 0) {

      raw_cohort <- seq_len(igraph::vcount(g))
      school_modularity <- NA_real_

    } else {

      communities <- igraph::cluster_louvain(
        g,
        weights = igraph::E(g)$weight
      )

      raw_cohort <- as.integer(
        igraph::membership(communities)[igraph::V(g)$name]
      )

      school_modularity <- igraph::modularity(
        communities
      )
    }

    tibble(
      D_location_name = school_name,
      D_stu_id = igraph::V(g)$name,
      raw_cohort = raw_cohort,
      M_school_modularity = school_modularity
    )
  }


  cohort_membership_raw <- purrr::map_dfr(
    sort(unique(class_roster$D_location_name)),
    infer_school
  )


  # Relabel communities so Cohort 1 = largest cohort,
  # Cohort 2 = second-largest, etc.
  cohort_lookup <- cohort_membership_raw |>
    count(
      D_location_name,
      raw_cohort,
      name = "M_inferred_cohort_size"
    ) |>
    group_by(D_location_name) |>
    arrange(
      desc(M_inferred_cohort_size),
      raw_cohort,
      .by_group = TRUE
    ) |>
    mutate(
      C_inferred_cohort = paste0(
        "Cohort ",
        row_number()
      )
    ) |>
    ungroup()


  # Number of cohorting classes each student has
  course_coverage <- class_roster |>
    count(
      D_location_name,
      D_stu_id,
      name = "M_stu_num_ninth_cohort_classes"
    )


  cohort_membership_raw |>
    left_join(
      cohort_lookup,
      by = c(
        "D_location_name",
        "raw_cohort"
      )
    ) |>
    left_join(
      course_coverage,
      by = c(
        "D_location_name",
        "D_stu_id"
      )
    ) |>
    select(
      -raw_cohort
    )
}


# =============================================================================
# 4. COHORT SUMMARY
# =============================================================================

build_cohort_summary <- function(cohort_membership) {

  cohort_membership |>
    distinct(
      D_location_name,
      C_inferred_cohort,
      M_inferred_cohort_size,
      M_school_modularity
    ) |>
    arrange(
      D_location_name,
      desc(M_inferred_cohort_size)
    )
}


# =============================================================================
# 5. SECTION PURITY
#
# For each class:
#   What share of students belong to its dominant inferred cohort?
# =============================================================================

build_section_purity <- function(
    cohort_schedule,
    cohort_membership
) {

  class_roster <- .class_roster(cohort_schedule)


  section_meta <- cohort_schedule |>
    group_by(
      D_location_name,
      C_class_id
    ) |>
    summarise(
      C_course_subject = paste(
        sort(unique(na.omit(C_course_subject))),
        collapse = " | "
      ),
      C_course_name = paste(
        sort(unique(na.omit(C_course_name))),
        collapse = " | "
      ),
      .groups = "drop"
    )


  section_cohort_counts <- class_roster |>
    left_join(
      cohort_membership |>
        select(
          D_location_name,
          D_stu_id,
          C_inferred_cohort
        ),
      by = c(
        "D_location_name",
        "D_stu_id"
      )
    ) |>
    count(
      D_location_name,
      C_class_id,
      C_inferred_cohort,
      name = "n_students"
    )


  section_cohort_counts |>
    group_by(
      D_location_name,
      C_class_id
    ) |>
    summarise(
      M_cls_num_students =
        sum(n_students),

      M_cls_num_inferred_cohorts =
        n_distinct(C_inferred_cohort),

      C_cls_dominant_cohort =
        C_inferred_cohort[which.max(n_students)],

      M_cls_dominant_cohort_students =
        max(n_students),

      M_cls_cohort_purity =
        max(n_students) / sum(n_students),

      .groups = "drop"
    ) |>
    left_join(
      section_meta,
      by = c(
        "D_location_name",
        "C_class_id"
      )
    ) |>
    arrange(
      D_location_name,
      M_cls_cohort_purity
    )
}


# =============================================================================
# 6. STUDENT × CLASS PURITY
#
# For each student in each class:
#   What share of their classmates are members of their inferred cohort?
# =============================================================================

build_student_class_purity <- function(
    cohort_schedule,
    cohort_membership
) {

  roster <- .class_roster(cohort_schedule) |>
    left_join(
      cohort_membership |>
        select(
          D_location_name,
          D_stu_id,
          C_inferred_cohort
        ),
      by = c(
        "D_location_name",
        "D_stu_id"
      )
    )


  class_sizes <- roster |>
    count(
      D_location_name,
      C_class_id,
      name = "n_students_in_class"
    )


  class_cohort_sizes <- roster |>
    count(
      D_location_name,
      C_class_id,
      C_inferred_cohort,
      name = "n_students_from_cohort"
    )


  roster |>
    left_join(
      class_sizes,
      by = c(
        "D_location_name",
        "C_class_id"
      )
    ) |>
    left_join(
      class_cohort_sizes,
      by = c(
        "D_location_name",
        "C_class_id",
        "C_inferred_cohort"
      )
    ) |>
    mutate(
      M_stu_num_classmates =
        n_students_in_class - 1,

      M_stu_num_same_cohort_classmates =
        n_students_from_cohort - 1,

      M_stu_class_cohort_purity =
        if_else(
          M_stu_num_classmates > 0,
          M_stu_num_same_cohort_classmates /
            M_stu_num_classmates,
          NA_real_
        )
    ) |>
    select(
      D_location_name,
      D_stu_id,
      C_inferred_cohort,
      C_class_id,
      M_stu_num_classmates,
      M_stu_num_same_cohort_classmates,
      M_stu_class_cohort_purity
    )
}


# =============================================================================
# 7. STUDENT PURITY
#
# Produces:
#   - average class purity
#   - % of classes >= 90% pure
#   - overall classmate purity
#   - number of unique peers outside the student's cohort
# =============================================================================

build_student_purity <- function(
    student_class_purity,
    student_pairs,
    cohort_membership,
    purity_threshold = 0.90
) {

  student_summary <- student_class_purity |>
    group_by(
      D_location_name,
      D_stu_id,
      C_inferred_cohort
    ) |>
    summarise(
      M_stu_num_cohort_classes =
        n(),

      M_stu_avg_class_cohort_purity =
        .mean_or_na(M_stu_class_cohort_purity),

      M_stu_pct_classes_90pct_pure =
        if (
          all(is.na(M_stu_class_cohort_purity))
        ) {
          NA_real_
        } else {
          mean(
            M_stu_class_cohort_purity >= purity_threshold,
            na.rm = TRUE
          )
        },

      M_stu_overall_classmate_purity =
        if (
          sum(M_stu_num_classmates, na.rm = TRUE) == 0
        ) {
          NA_real_
        } else {
          sum(
            M_stu_num_same_cohort_classmates,
            na.rm = TRUE
          ) /
            sum(
              M_stu_num_classmates,
              na.rm = TRUE
            )
        },

      .groups = "drop"
    )


  pair_cohorts <- student_pairs |>
    left_join(
      cohort_membership |>
        select(
          D_location_name,
          D_stu_id,
          C_inferred_cohort
        ) |>
        rename(
          D_stu_id_1 = D_stu_id,
          C_cohort_1 = C_inferred_cohort
        ),
      by = c(
        "D_location_name",
        "D_stu_id_1"
      )
    ) |>
    left_join(
      cohort_membership |>
        select(
          D_location_name,
          D_stu_id,
          C_inferred_cohort
        ) |>
        rename(
          D_stu_id_2 = D_stu_id,
          C_cohort_2 = C_inferred_cohort
        ),
      by = c(
        "D_location_name",
        "D_stu_id_2"
      )
    )


  # Mirror outside-cohort pairs so each student
  # gets a count from their own perspective.
  outside_cohort_peers <- bind_rows(

    pair_cohorts |>
      filter(C_cohort_1 != C_cohort_2) |>
      transmute(
        D_location_name,
        D_stu_id = D_stu_id_1,
        D_peer_id = D_stu_id_2
      ),

    pair_cohorts |>
      filter(C_cohort_1 != C_cohort_2) |>
      transmute(
        D_location_name,
        D_stu_id = D_stu_id_2,
        D_peer_id = D_stu_id_1
      )

  ) |>
    distinct(
      D_location_name,
      D_stu_id,
      D_peer_id
    ) |>
    count(
      D_location_name,
      D_stu_id,
      name = "M_stu_unique_outside_cohort_peers"
    )


  student_summary |>
    left_join(
      outside_cohort_peers,
      by = c(
        "D_location_name",
        "D_stu_id"
      )
    ) |>
    mutate(
      M_stu_unique_outside_cohort_peers =
        replace_na(
          M_stu_unique_outside_cohort_peers,
          0L
        )
    )
}


# =============================================================================
# 8. PEER CONTINUITY
#
# For each student:
#   How many peers do they share 1, 2, 3, 4, etc. classes with?
# =============================================================================

build_peer_continuity <- function(student_pairs) {

  peer_long <- bind_rows(

    student_pairs |>
      transmute(
        D_location_name,
        D_stu_id = D_stu_id_1,
        D_peer_id = D_stu_id_2,
        M_shared_classes
      ),

    student_pairs |>
      transmute(
        D_location_name,
        D_stu_id = D_stu_id_2,
        D_peer_id = D_stu_id_1,
        M_shared_classes
      )
  )


  peer_summary <- peer_long |>
    group_by(
      D_location_name,
      D_stu_id
    ) |>
    summarise(
      M_stu_unique_peers =
        n_distinct(D_peer_id),

      M_stu_peers_shared_2plus =
        sum(M_shared_classes >= 2),

      M_stu_peers_shared_3plus =
        sum(M_shared_classes >= 3),

      M_stu_peers_shared_4plus =
        sum(M_shared_classes >= 4),

      M_stu_max_shared_classes =
        max(M_shared_classes),

      .groups = "drop"
    )


  peer_distribution <- peer_long |>
    count(
      D_location_name,
      D_stu_id,
      M_shared_classes,
      name = "n_peers"
    ) |>
    mutate(
      metric = paste0(
        "M_stu_peers_shared_",
        M_shared_classes,
        "_classes"
      )
    ) |>
    select(
      -M_shared_classes
    ) |>
    pivot_wider(
      names_from = metric,
      values_from = n_peers,
      values_fill = 0
    )


  peer_summary |>
    left_join(
      peer_distribution,
      by = c(
        "D_location_name",
        "D_stu_id"
      )
    )
}


# =============================================================================
# 9. WRAPPER — RUN FULL COHORT ANALYSIS
# =============================================================================

run_ninth_cohort_analysis <- function(
    student_rollup,
    core_ninth_courses,
    seminar_ninth_courses,
    s1_term = "S1",
    purity_threshold = 0.90,
    seed = 1234
) {

  # ---------------------------------------------------------------------------
  # Cohorting population
  # ---------------------------------------------------------------------------

  cohort_schedule <- build_ninth_cohort_schedule(
    student_rollup = student_rollup,
    core_ninth_courses = core_ninth_courses,
    seminar_ninth_courses = seminar_ninth_courses,
    s1_term = s1_term
  )


  # ---------------------------------------------------------------------------
  # Student network
  # ---------------------------------------------------------------------------

  student_pairs <- build_student_pairs(
    cohort_schedule
  )


  # ---------------------------------------------------------------------------
  # Infer cohorts
  # ---------------------------------------------------------------------------

  cohort_membership <- infer_ninth_cohorts(
    cohort_schedule = cohort_schedule,
    student_pairs = student_pairs,
    seed = seed
  )


  cohort_summary <- build_cohort_summary(
    cohort_membership
  )


  # ---------------------------------------------------------------------------
  # Section purity
  # ---------------------------------------------------------------------------

  section_purity <- build_section_purity(
    cohort_schedule = cohort_schedule,
    cohort_membership = cohort_membership
  )


  # ---------------------------------------------------------------------------
  # Student purity
  # ---------------------------------------------------------------------------

  student_class_purity <- build_student_class_purity(
    cohort_schedule = cohort_schedule,
    cohort_membership = cohort_membership
  )


  student_purity <- build_student_purity(
    student_class_purity = student_class_purity,
    student_pairs = student_pairs,
    cohort_membership = cohort_membership,
    purity_threshold = purity_threshold
  )


  # ---------------------------------------------------------------------------
  # Peer continuity
  # ---------------------------------------------------------------------------

  peer_continuity <- build_peer_continuity(
    student_pairs
  )


  # ---------------------------------------------------------------------------
  # Return all useful diagnostic outputs
  # ---------------------------------------------------------------------------

  list(
    cohort_schedule = cohort_schedule,
    student_pairs = student_pairs,
    cohort_membership = cohort_membership,
    cohort_summary = cohort_summary,
    section_purity = section_purity,
    student_class_purity = student_class_purity,
    student_purity = student_purity,
    peer_continuity = peer_continuity
  )
}