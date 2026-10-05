#' Simulate both arms year by year
#'
#' Moves every patient through the transition matrix of each arm, one cycle
#' (year) at a time, from their entry cycle until they die or the horizon ends.
#'
#' * A patient's state in their entry cycle is their entry state (from
#'   [tm_assign_entry_states()]), the same in both arms.
#' * In each later cycle, the next state is drawn from the matrix row for the
#'   patient's stratum, their age group in the previous cycle and their previous
#'   state. If the matrix has a `year` column (see [tm_matrix()]), the row is
#'   also the one for the patient's year since entry: the move into the first
#'   year after entry uses the year 1 rows.
#' * Age goes up by one each cycle. Patients move through the age groups only if
#'   the age-group stratifier is called `agegrp`; any other stratifier is fixed.
#'   A patient who would pass the oldest age group dies that year, in the
#'   `age_limit_death_state` from `settings.yaml`.
#' * Both arms use the same random number for the same patient and cycle, so
#'   differences between the arms come from the matrices. Both arms read the
#'   states in the usual-care matrix's column order, so a random number means
#'   the same thing in each. The numbers continue the run's random stream after
#'   the entry-state draws (see [tm_assign_entry_states()]), so one seed fixes
#'   the whole run. Your own random-number state is restored afterwards.
#'
#' @param cohort The cohort with entry states, from [tm_assign_entry_states()].
#' @param matrices The two transition matrices, from [tm_read_matrices()].
#' @param settings Run settings, from [tm_read_settings()].
#' @param seed Seed of the run, the same one used for the entry states. Defaults
#'   to `seed` in `settings.yaml`.
#' @return A `data.table` with one row per patient, arm and cycle: `id`, `arm`,
#'   `cycle`, `age`, the stratifiers (`agegrp`, if used, is the age group in
#'   that cycle; the others are fixed) and `state`, from the entry cycle up to
#'   and including the cycle of death.
#' @export
tm_simulate <- function(cohort, matrices, settings = tm_read_settings(), seed = settings$seed) {
  horizon <- check_horizon(settings$horizon)
  if (!"entry_state" %in% names(cohort)) {
    stop("The cohort has no `entry_state` column; run tm_assign_entry_states() first.", call. = FALSE)
  }
  uses_age <- "agegrp" %in% settings$strata
  if (uses_age) {
    if (!isTRUE(settings$age_limit_death_state %in% matrices$usual_care$death)) {
      stop("inputs/settings.yaml needs `age_limit_death_state`, one of the death states: ",
           paste(matrices$usual_care$death, collapse = ", "), call. = FALSE)
    }
    bands <- age_bands(matrices$usual_care$probs$agegrp)
  }

  n <- nrow(cohort)
  if (!is.null(seed)) {
    withr::local_seed(seed)
    stats::runif(n)  # skip the numbers used for the entry states
  }
  random <- matrix(stats::runif(n * horizon), nrow = n)  # one number per patient and cycle, shared by both arms
  # Both arms walk the states in the same order, so the shared random number means the same thing in each.
  states <- matrices[[1]]$states

  arms <- lapply(names(matrices), function(arm) {
    simulate_arm(cohort, matrices[[arm]], states, random, horizon, settings, if (uses_age) bands)[, arm := arm]
  })
  out <- data.table::rbindlist(arms)
  data.table::setcolorder(out, c("id", "arm", "cycle", "age", matrices$usual_care$strata, "state"))
  out[]
}

# Stops unless `horizon` is a whole number of cycles, 1 or more; returns it.
check_horizon <- function(horizon) {
  if (!is.numeric(horizon) || length(horizon) != 1 || is.na(horizon) || horizon < 1 || horizon != round(horizon)) {
    stop("inputs/settings.yaml needs `horizon`: a whole number of cycles, 1 or more.", call. = FALSE)
  }
  horizon
}

simulate_arm <- function(cohort, m, states, random, horizon, settings, bands) {
  strata_fixed <- setdiff(m$strata, "agegrp")
  key_cols <- c(if (!is.null(m$years)) "year", m$strata, "from")
  probs <- as.matrix(m$probs[, states, with = FALSE])
  cumulative <- t(apply(probs, 1, cumsum))
  cumulative[, ncol(cumulative)] <- 1  # guards against rows summing to 0.9999999999
  row_key <- do.call(paste, c(m$probs[, key_cols, with = FALSE], sep = "\r"))

  age_group <- function(age) {
    if (is.null(bands)) return(NULL)
    bands$agegrp[pmin(findInterval(age, bands$lower), nrow(bands))]
  }
  max_age <- if (!is.null(bands)) max(bands$upper)

  state <- rep(NA_character_, nrow(cohort))  # state in the previous cycle; NA = not entered yet
  records <- vector("list", horizon)
  for (cycle in seq_len(horizon) - 1L) {
    was_dead <- !is.na(state) & state %in% m$death
    movers <- which(!is.na(state) & !was_dead)
    if (length(movers) > 0) {
      age_before <- cohort$age[movers] + (cycle - 1L - cohort$entry_cycle[movers])
      lookup <- cohort[movers, strata_fixed, with = FALSE]
      if (!is.null(bands)) lookup[, agegrp := age_group(age_before)]
      if (!is.null(m$years)) lookup[, year := cycle - cohort$entry_cycle[movers]]  # the year since entry moved into
      lookup[, from := state[movers]]
      row <- match(do.call(paste, c(lookup[, key_cols, with = FALSE], sep = "\r")), row_key)
      if (anyNA(row)) {
        stop("No matrix row for some patients in cycle ", cycle, " (ids ",
             format_values(cohort$id[movers[is.na(row)]]), ").", call. = FALSE)
      }
      drawn <- rowSums(random[movers, cycle + 1L] > cumulative[row, , drop = FALSE]) + 1L
      next_state <- states[drawn]
      if (!is.null(bands)) {
        aged_out <- age_before + 1L > max_age & !(next_state %in% m$death)
        next_state[aged_out] <- settings$age_limit_death_state
      }
      state[movers] <- next_state
    }
    entering <- which(cohort$entry_cycle == cycle)
    state[entering] <- cohort$entry_state[entering]

    present <- which(cohort$entry_cycle <= cycle & !was_dead)
    age_now <- cohort$age[present] + (cycle - cohort$entry_cycle[present])
    record <- data.table::data.table(
      id = cohort$id[present],
      cycle = rep(cycle, length(present)),
      age = age_now,
      agegrp = age_group(age_now)
    )
    # The fixed stratifiers too, so that costs and utilities can be matched by stratum (see tm_add_outcomes()).
    for (s in strata_fixed) data.table::set(record, j = s, value = cohort[[s]][present])
    data.table::set(record, j = "state", value = state[present])
    records[[cycle + 1L]] <- record
  }
  data.table::rbindlist(records)
}
