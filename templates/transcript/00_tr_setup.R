# =============================================================================
# setup.R — One line at the top of the .qmd pulls in everything
# =============================================================================
# source(here::here("R", "setup.R"))
# =============================================================================

erstools::ers_load_packages()
options(scipen = 99)

# Not always exported by the loaded packages; define defensively.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

.helpers <- c("checks.R", "config.R", "io.R", "joins.R")

for (f in .helpers) {
  p <- here::here("R", f)
  if (!file.exists(p)) stop("Helper not found: ", p, call. = FALSE)
  source(p, local = FALSE)
}

message("Helpers loaded: ", paste(.helpers, collapse = ", "))
rm(.helpers)
