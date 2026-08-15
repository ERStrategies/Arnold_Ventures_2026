# =============================================================================
# 00_tr_setup.R — One line at the top of the .qmd pulls in everything
# =============================================================================
# source(here::here("templates", "transcript", "00_tr_setup.R"))
# =============================================================================

erstools::ers_load_packages()
options(scipen = 99)

# Not always exported by the loaded packages; define defensively.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

.helpers <- c("00_tr_checks.R", "00_tr_config.R", "00_tr_io.R",
              "00_tr_joins.R", "00_tr_clean.R")

for (f in .helpers) {
  p <- here::here("templates", "transcript", f)
  if (!file.exists(p)) stop("Helper not found: ", p, call. = FALSE)
  source(p, local = FALSE)
}

message("Helpers loaded: ", paste(.helpers, collapse = ", "))
rm(.helpers)