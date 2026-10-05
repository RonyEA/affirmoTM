#' Create a transition matrix
#'
#' Reads and checks a transition matrix in wide format: one row per current
#' state (`from`), one column per next state, holding the one-cycle transition
#' probabilities. This is the format every arm of the model uses.
#'
#' Every state has its own rows, death states included. A death state is a state
#' whose rows always go back to itself with probability 1 (nobody leaves death).
#' The probabilities themselves are the user's responsibility; the checks below
#' only make sure the matrix is complete and consistent.
#'
#' @param x A `data.frame`, or the path to a CSV file, with
#'   * a `from` column naming the current state;
#'   * optional stratifier columns (see `strata`), so that each stratum has its
#'     own matrix. A matrix without stratifiers applies to everyone;
#'   * an optional `sim` column numbering probabilistic draws, as produced by
#'     [tm_matrix_from_model()];
#'   * an optional `year` column: years since a patient's entry, 1 being the
#'     first year after entry. The rows for year `k` drive the move into year
#'     `k` after entry, so the probabilities can change over time. Every year
#'     needs a full matrix, with the same strata. Without it, the same rows
#'     apply in every year;
#'   * one numeric column per state.
#' @param strata Names of the stratifier columns. Defaults to whichever of
#'   `agegrp`, `sex` and `mm_cluster` are present. The age-group column must be
#'   called `agegrp`: it is the only stratifier patients move through as they
#'   age; any other stratifier is fixed for each patient.
#'
#' @details
#' The checks stop with an error that names the offending rows or states:
#' * every row sums to 1 (tolerance 1e-8); rows are never rescaled;
#' * probabilities are between 0 and 1, with no missing values;
#' * every state has both a column and its own rows;
#' * no row is duplicated, and every stratum has a row for every state;
#' * `year`, if present, holds whole numbers, 1 or more, and every year has the
#'   same strata. [tm_read_matrices()] also checks that every year of the
#'   horizon is listed.
#'
#' The death states are the states whose probability of staying is 1 (within
#' 1e-8) in every row. A state that stays put with probability 1 in only some
#' strata is a living state.
#'
#' @return An object of class `tm_matrix`: a list with `probs` (the matrix as a
#'   `data.table`), `states` (in column order), `death`, `strata`, `years` (the
#'   years listed in the `year` column, or `NULL`), `n_sim` (0 when there are no
#'   draws) and `source`.
#' @export
#' @examples
#' toy <- data.frame(
#'   from = c("Well", "Sick", "Dead"),
#'   Well = c(0.90, 0.20, 0),
#'   Sick = c(0.08, 0.70, 0),
#'   Dead = c(0.02, 0.10, 1)
#' )
#' tm_matrix(toy)
tm_matrix <- function(x, strata = NULL) {
  if (is.character(x) && length(x) == 1) {
    if (!file.exists(x)) {
      stop("Transition matrix file not found: ", x, call. = FALSE)
    }
    x <- data.table::fread(x)
  }
  if (!is.data.frame(x)) {
    stop("`x` must be a data.frame or the path to a CSV file.", call. = FALSE)
  }
  x <- data.table::as.data.table(x)
  if (!"from" %in% names(x)) {
    stop("The transition matrix needs a `from` column naming the current state.", call. = FALSE)
  }

  if (is.null(strata)) {
    strata <- intersect(c("agegrp", "sex", "mm_cluster"), names(x))
  }
  missing_strata <- setdiff(strata, names(x))
  if (length(missing_strata) > 0) {
    stop("Stratifier column(s) not found: ", paste(missing_strata, collapse = ", "), call. = FALSE)
  }
  if ("year" %in% strata) {
    stop("`year` is the years-since-entry column, not a stratifier; remove it from `strata`.", call. = FALSE)
  }
  has_sim <- "sim" %in% names(x)
  has_year <- "year" %in% names(x)
  if (has_year) check_year_values(x$year)
  id_cols <- c(if (has_sim) "sim", if (has_year) "year", strata, "from")
  states <- setdiff(names(x), id_cols)
  if (length(states) == 0) {
    stop("The transition matrix has no state columns.", call. = FALSE)
  }
  non_numeric <- states[!vapply(x[, states, with = FALSE], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    stop("State column(s) are not numeric: ", paste(non_numeric, collapse = ", "),
         ". If they are stratifiers, add them to `strata` (the `strata` entry of inputs/settings.yaml ",
         "when reading from inputs/).", call. = FALSE)
  }

  data.table::set(x, j = "from", value = as.character(x[["from"]]))
  from_states <- unique(x$from)
  no_column <- setdiff(from_states, states)
  if (length(no_column) > 0) {
    stop("State(s) with a row but no column: ", paste(no_column, collapse = ", "), call. = FALSE)
  }
  no_rows <- setdiff(states, from_states)
  if (length(no_rows) > 0) {
    stop("State(s) with a column but no rows: ", paste(no_rows, collapse = ", "),
         ". Every state needs its own rows; a death state's rows go back to itself with probability 1.",
         call. = FALSE)
  }

  probs <- as.matrix(x[, states, with = FALSE])
  check_probability_rows(probs)

  if (anyDuplicated(x[, id_cols, with = FALSE]) > 0) {
    dup <- which(duplicated(x[, id_cols, with = FALSE]))
    stop("Duplicated rows (same ", paste(id_cols, collapse = ", "), "): rows ",
         format_rows(dup), call. = FALSE)
  }
  group_cols <- setdiff(id_cols, "from")
  if (length(group_cols) > 0) {
    n_from <- x[, .N, by = group_cols]
    incomplete <- n_from[n_from[["N"]] != length(states)]
    if (nrow(incomplete) > 0) {
      first <- incomplete[1, group_cols, with = FALSE]
      stop(nrow(incomplete), " strata do not have a row for every state (",
           length(states), " expected). First: ",
           paste(names(first), unlist(first), sep = " = ", collapse = ", "), call. = FALSE)
    }
  }
  if (has_year && length(strata) > 0) {
    # Every year (and draw) needs the same strata, so a patient finds a row whatever their year since entry.
    n_strata <- nrow(unique(x[, strata, with = FALSE]))
    per_year <- unique(x[, c(if (has_sim) "sim", "year", strata), with = FALSE])[, .N, by = c(if (has_sim) "sim", "year")]
    short <- per_year[per_year[["N"]] != n_strata]
    if (nrow(short) > 0) {
      stop("Every year needs a row for every stratum (", n_strata, " strata); year(s) ",
           format_values(unique(short$year)), " have fewer.", call. = FALSE)
    }
  }

  # Death states: every one of their rows goes back to the same state with probability 1.
  stay <- probs[cbind(seq_len(nrow(probs)), match(x$from, states))]
  always_stays <- tapply(stay > 1 - 1e-8, x$from, all)
  death <- states[states %in% names(always_stays)[always_stays]]

  data.table::setcolorder(x, c(id_cols, states))
  structure(
    list(
      probs = x,
      states = states,
      death = death,
      strata = strata,
      years = if (has_year) sort(unique(x$year)),
      n_sim = if (has_sim) data.table::uniqueN(x$sim) else 0L,
      source = "user"
    ),
    class = "tm_matrix"
  )
}

check_probability_rows <- function(probs) {
  na_rows <- which(rowSums(is.na(probs)) > 0)
  if (length(na_rows) > 0) {
    stop("Missing probabilities in rows ", format_rows(na_rows), call. = FALSE)
  }
  out_of_range <- which(rowSums(probs < 0 | probs > 1) > 0)
  if (length(out_of_range) > 0) {
    stop("Probabilities outside [0, 1] in rows ", format_rows(out_of_range), call. = FALSE)
  }
  row_sums <- rowSums(probs)
  off <- which(abs(row_sums - 1) > 1e-8)
  if (length(off) > 0) {
    shown <- utils::head(off, 10)
    stop("Rows do not sum to 1 (tolerance 1e-8): ",
         paste0("row ", shown, " sums to ", format(row_sums[shown], digits = 10), collapse = "; "),
         if (length(off) > 10) paste0("; and ", length(off) - 10, " more rows"),
         call. = FALSE)
  }
  invisible(TRUE)
}

# Stops unless a `year` column (years since entry) holds whole numbers, 1 or more.
check_year_values <- function(year) {
  if (!is.numeric(year) || anyNA(year) || any(year != round(year)) || any(year < 1)) {
    stop("`year` must hold whole numbers, 1 or more (years since entry; 1 = the first year after entry).",
         call. = FALSE)
  }
  invisible(TRUE)
}

format_rows <- function(rows) {
  shown <- paste(utils::head(rows, 10), collapse = ", ")
  if (length(rows) > 10) paste0(shown, " and ", length(rows) - 10, " more") else shown
}

#' @export
print.tm_matrix <- function(x, ...) {
  cat("<tm_matrix> transition matrix (source: ", x$source, ")\n", sep = "")
  if (length(x$strata) > 0) {
    sizes <- vapply(x$strata, function(s) data.table::uniqueN(x$probs[[s]]), integer(1))
    n_strata <- nrow(unique(x$probs[, x$strata, with = FALSE]))
    cat("  Strata:        ", paste0(x$strata, " (", sizes, ")", collapse = ", "), "->", n_strata, "strata\n")
  } else {
    cat("  Strata:         none (one matrix for everyone)\n")
  }
  living <- setdiff(x$states, x$death)
  cat("  Living states: ", length(living), "-", paste(living, collapse = ", "), "\n")
  cat("  Death states:  ", length(x$death), "-", paste(x$death, collapse = ", "), "\n")
  cat("  Years:         ", if (is.null(x$years)) "the same rows every year" else
        paste0("by year since entry, ", paste(range(x$years), collapse = " to "), " (year column)"), "\n")
  cat("  Draws:         ", if (x$n_sim > 0) paste(x$n_sim, "(sim column)") else "none (point values)", "\n")
  if (!is.null(x$model_info)) {
    info <- x$model_info
    cat("  Allowed-transitions mask:", if (info$mask_applied) "applied" else "not applied", "\n")
    if (!is.null(info$vcov_repair)) {
      r <- info$vcov_repair
      cat("  vcov repair:    largest asymmetry", format(r$max_asymmetry, digits = 3),
          "symmetrised;", r$n_negative_eigen, "negative eigenvalues (smallest",
          format(r$min_eigen, digits = 3), ") set to 0\n")
    }
  }
  invisible(x)
}
