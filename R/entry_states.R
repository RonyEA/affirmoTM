#' Read and check the entry-state table
#'
#' Reads `inputs/entry_states.csv`: one row per stratum (the stratifiers in
#' `settings.yaml`) and one column per state, giving the probability of each
#' state in a patient's entry year. Every state must be a state of the transition
#' matrices, and every row must sum to 1. Any problem stops with an error.
#'
#' @param settings Run settings, from [tm_read_settings()].
#' @param matrices The two transition matrices, from [tm_read_matrices()].
#' @return A `data.table` with one row per stratum.
#' @export
tm_read_entry_states <- function(settings = tm_read_settings(), matrices = tm_read_matrices(settings)) {
  force(settings)
  path <- input_path("entry_states")
  entry <- data.table::fread(path)
  fail <- function(...) stop(path, ": ", ..., call. = FALSE)

  missing <- setdiff(settings$strata, names(entry))
  if (length(missing) > 0) fail("missing stratifier column(s): ", paste(missing, collapse = ", "))
  states <- setdiff(names(entry), settings$strata)
  unknown <- setdiff(states, matrices$usual_care$states)
  if (length(unknown) > 0) fail("states not in the transition matrices: ", paste(unknown, collapse = ", "))
  if (anyDuplicated(entry[, settings$strata, with = FALSE]) > 0) fail("duplicated strata.")
  tryCatch(check_probability_rows(as.matrix(entry[, states, with = FALSE])),
           error = function(e) fail(conditionMessage(e)))
  entry
}

#' Assign each patient an entry state
#'
#' Draws each patient's state in their entry year from the entry-state table row
#' for their stratum. Both arms share these entry states. The draws are the first
#' numbers of the run's random stream, started from `seed`, so the same inputs and
#' seed always give the same entry states. Your own random-number state is
#' restored afterwards.
#'
#' @param cohort The cohort, from [tm_read_cohort()].
#' @param entry_states The entry-state table, from [tm_read_entry_states()].
#' @param settings Run settings, from [tm_read_settings()].
#' @param seed Seed of the run. Defaults to `seed` in `settings.yaml`.
#' @return The cohort with an added `entry_state` column.
#' @export
tm_assign_entry_states <- function(cohort, entry_states, settings = tm_read_settings(), seed = settings$seed) {
  strata <- settings$strata
  states <- setdiff(names(entry_states), strata)
  probs <- as.matrix(entry_states[, states, with = FALSE])

  row <- entry_rows(cohort, entry_states, strata)

  if (!is.null(seed)) withr::local_seed(seed)
  cumulative <- t(apply(probs[row, , drop = FALSE], 1, cumsum))
  cumulative[, ncol(cumulative)] <- 1  # guards against rows summing to 0.9999999999
  drawn <- rowSums(stats::runif(nrow(cohort)) > cumulative) + 1L

  out <- data.table::copy(cohort)
  out[, entry_state := states[drawn]]
  out[]
}

# Each patient's row of the entry-state table; stops if any patient's entry stratum has no row.
entry_rows <- function(cohort, entry_states, strata) {
  key <- function(d) if (length(strata) > 0) do.call(paste, c(d[, strata, with = FALSE], sep = " / ")) else rep("", nrow(d))
  row <- match(key(cohort), key(entry_states))
  if (anyNA(row)) {
    stop("inputs/entry_states.csv has no row for the entry stratum of ", sum(is.na(row)), " patients (ids ",
         format_values(cohort$id[is.na(row)]), "). First missing stratum: ",
         key(cohort)[which(is.na(row))[1]], call. = FALSE)
  }
  row
}
