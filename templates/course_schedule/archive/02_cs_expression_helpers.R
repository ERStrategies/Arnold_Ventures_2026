# =============================================================================
# 00_expression_helpers.R
# Parse a course-schedule "expression" into its set of atomic MEETINGS
# "<period-atom>@<rotation>", so double-booking sizing can compare meeting sets
# for overlap without exploding the data.
#
# District-agnostic. The spec comes from config$expression via expr_spec():
#   atoms     : ordered slots used to resolve ranges that cross half/block
#               periods (e.g. Detroit's 4a/4b). Leave EMPTY for plain integer
#               periods — ranges then expand numerically.
#   expands   : whole tokens covering several atoms (e.g. "4" -> 4a,4b).
#   rotations : rotation token -> atomic days ("A-B" -> both A and B).
#
# Verified in Python on Springfield (integer ranges, A/B/A-B, comma segments)
# and Detroit (half-period atoms) before translation.
# =============================================================================

suppressPackageStartupMessages({
  library(stringr)
})

expr_spec <- function(config) {
  e <- config$expression
  list(
    atoms     = unlist(e$atoms, use.names = FALSE),
    expands   = lapply(e$expands,   function(x) unlist(x, use.names = FALSE)),
    rotations = lapply(e$rotations, function(x) unlist(x, use.names = FALSE))
  )
}

.expr_token_atoms <- function(tok, spec) {
  tok <- str_trim(tok)
  if (!is.null(spec$expands[[tok]])) spec$expands[[tok]] else tok
}

# atoms for a period part: single token or a range "X-Y".
# 1) if the configured atoms list covers both endpoints -> resolve over it
#    (handles half/block periods like 4a/4b);
# 2) else if both endpoints are integers -> numeric range;
# 3) else unresolved -> empty.
.expr_period_atoms <- function(pp, spec) {
  pp <- str_trim(pp)
  if (str_detect(pp, "-")) {
    ends <- str_split_fixed(pp, "-", 2)
    lo <- str_trim(ends[1]); hi <- str_trim(ends[2])
    both <- c(.expr_token_atoms(lo, spec), .expr_token_atoms(hi, spec))
    idx  <- match(both, spec$atoms)
    if (length(idx) && !any(is.na(idx))) return(spec$atoms[min(idx):max(idx)])
    if (grepl("^[0-9]+$", lo) && grepl("^[0-9]+$", hi)) {
      a <- as.integer(lo); b <- as.integer(hi)
      return(as.character(seq(min(a, b), max(a, b))))
    }
    return(character(0))
  }
  .expr_token_atoms(pp, spec)
}

.expr_rotation_set <- function(rp, spec) {
  rp <- str_trim(rp)
  if (!is.null(spec$rotations[[rp]])) return(spec$rotations[[rp]])
  parts <- str_split(rp, "[,\\s]+")[[1]]; parts <- parts[nzchar(parts)]
  out <- character(0)
  for (p in parts) {
    if (!is.null(spec$rotations[[p]])) out <- c(out, spec$rotations[[p]])
    else out <- c(out, str_split(p, "")[[1]])
  }
  sort(unique(out))
}

# one expression -> sorted character vector of "<atom>@<rotation>" meetings.
# segments are comma- OR space-separated.
parse_expression <- function(expr, spec) {
  if (length(expr) != 1 || is.na(expr) || !nzchar(str_trim(expr))) return(character(0))
  blocks <- str_split(str_trim(expr), "[,\\s]+")[[1]]
  meetings <- character(0)
  for (b in blocks) {
    m <- str_match(str_trim(b), "^(.+)\\(([^)]*)\\)$")
    if (is.na(m[1, 1])) next
    atoms <- .expr_period_atoms(m[1, 2], spec)
    rots  <- .expr_rotation_set(m[1, 3], spec)
    if (length(atoms) && length(rots)) {
      grid <- expand.grid(a = atoms, r = rots, stringsAsFactors = FALSE)
      meetings <- c(meetings, paste0(grid$a, "@", grid$r))
    }
  }
  meetings <- unique(meetings)
  atoms_of <- sub("@.*$", "", meetings)
  meetings[order(match(atoms_of, spec$atoms), suppressWarnings(as.numeric(atoms_of)), atoms_of)]
}

# add a list-column of meetings to a whole data frame
add_meetings <- function(df, spec, expr_col = "D_expression", into = "M_meetings") {
  df[[into]] <- lapply(df[[expr_col]], parse_expression, spec = spec)
  df
}

# quick eyeball: parse a vector of expressions -> a table (expression -> meetings)
expr_demo <- function(exprs, spec) {
  data.frame(
    expression = exprs,
    meetings   = vapply(exprs, function(e) paste(parse_expression(e, spec), collapse = " | "),
                        character(1)),
    row.names = NULL, stringsAsFactors = FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- format-agnostic time -> meetings adapter --------------------------------
# Districts encode time as an expression (D_expression) OR pre-split period +
# rotation (D_period + D_rotation). Both resolve to the same meeting set, so
# nothing downstream cares which format the district used. Set config$time_format.
time_key_cols <- function(config) {
  fmt <- config$time_format %||% "expression"
  cols <- config$time_cols[[fmt]]
  if (is.null(cols)) if (identical(fmt, "period_rotation")) c("D_period","D_rotation") else "D_expression" else unlist(cols)
}

# One record's time value(s) -> its meeting set.
record_meetings <- function(vals, config, spec) {
  fmt <- config$time_format %||% "expression"
  if (identical(fmt, "period_rotation")) {
    period <- vals[[1]]; rotation <- as.character(vals[[2]])
    if (is.na(period) || is.na(rotation)) return(character(0))
    rots <- spec$rotations[[rotation]]; if (is.null(rots)) rots <- rotation
    paste0(period, "@", unlist(rots))     # period+rotation -> meeting(s); A-B fans out
  } else {
    parse_expression(vals[[1]], spec)     # expression -> meetings
  }
}

# inverse of parse_expression: a meeting set -> a compact expression string.
# (integer periods: group by rotation-set, collapse consecutive periods into ranges)
serialize_meetings <- function(meetings) {
  if (!length(meetings)) return(NA_character_)
  atom <- sub("@.*$", "", meetings); rot <- sub("^.*@", "", meetings)
  by_atom <- tapply(rot, atom, function(rs) paste(sort(unique(rs)), collapse = "-"))
  df <- data.frame(atom = names(by_atom), label = as.vector(by_atom),
                   p = suppressWarnings(as.integer(names(by_atom))), stringsAsFactors = FALSE)
  segs <- character(0); keys <- numeric(0)
  for (lab in unique(df$label)) {
    ps <- sort(df$p[df$label == lab])
    if (any(is.na(ps))) {                                  # non-integer atoms: emit individually
      for (a in df$atom[df$label == lab]) { segs <- c(segs, paste0(a, "(", lab, ")")); keys <- c(keys, Inf) }
      next
    }
    brk <- c(0, which(diff(ps) != 1), length(ps))          # consecutive runs
    for (i in seq_len(length(brk) - 1)) {
      run <- ps[(brk[i] + 1):brk[i + 1]]; a <- run[1]; b <- run[length(run)]
      segs <- c(segs, paste0(if (a == b) a else paste0(a, "-", b), "(", lab, ")")); keys <- c(keys, a)
    }
  }
  paste(segs[order(keys)], collapse = ", ")
}