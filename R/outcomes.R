#' Read and check the costs
#'
#' Reads `inputs/costs.csv`: columns `state` and one per arm (`usual_care`,
#' `iabc`), giving the cost of a year spent in each state. Every state of the
#' transition matrices, death states included, needs exactly one row. Costs must
#' be 0 or more. Any problem stops with an error.
#'
#' Optional columns let costs differ between patients and over time:
#' * stratifier columns, any of the `strata` in `settings.yaml` (for example
#'   `mm_cluster`), so that each stratum has its own costs. Each patient-year
#'   uses the patient's stratum in that year: their current age group, as they
#'   age, and their sex and cluster. Every stratum of the matrices needs a row
#'   for every state;
#' * a `year` column (years since entry, 1 being the first year after entry).
#'   Every state then needs a row for every year from 1 to `horizon - 1`
#'   (`settings.yaml`); later years are allowed and not used.
#'
#' In a patient's entry year both arms are charged the `usual_care` cost (year
#' 1's, with a `year` column); the `iabc` cost applies from the year after entry
#' (see [tm_add_outcomes()]).
#'
#' @param settings Run settings, from [tm_read_settings()].
#' @param matrices The two transition matrices, from [tm_read_matrices()].
#' @return A `data.table` with one row per state (and stratum and year, with
#'   those columns).
#' @export
tm_read_costs <- function(settings = tm_read_settings(), matrices = tm_read_matrices(settings)) {
  read_state_table("costs", matrices, settings, required = matrices$usual_care$states,
                   check = function(v) all(v >= 0), rule = "0 or more")
}

#' Read and check the utilities
#'
#' Reads `inputs/utilities.csv`: columns `state` and one per arm (`usual_care`,
#' `iabc`), giving the QALY for a year spent in each state. Every living state
#' needs exactly one row. Death states (see [tm_matrix()]) count 0 QALYs in the
#' year of death; they may be left out, and if listed they must be 0. Utilities
#' must be at most 1. Any problem stops with an error.
#'
#' Optional stratifier and `year` columns work as in [tm_read_costs()], for
#' example utilities by age group and sex: every living state then needs a row
#' for every stratum (and year).
#'
#' In a patient's entry year both arms take the `usual_care` utility (year 1's,
#' with a `year` column); the `iabc` utility applies from the year after entry
#' (see [tm_add_outcomes()]).
#'
#' @inheritParams tm_read_costs
#' @return A `data.table` with one row per state (and stratum and year, with
#'   those columns), death states included (as 0).
#' @export
tm_read_utilities <- function(settings = tm_read_settings(), matrices = tm_read_matrices(settings)) {
  death <- matrices$usual_care$death
  utilities <- read_state_table("utilities", matrices, settings,
                                required = setdiff(matrices$usual_care$states, death),
                                check = function(v) all(v <= 1), rule = "at most 1")
  arms <- names(matrices)
  listed_death <- utilities[state %in% death]
  not_zero <- unique(listed_death$state[rowSums(as.matrix(listed_death[, arms, with = FALSE]) != 0) > 0])
  if (length(not_zero) > 0) {
    stop("inputs/utilities.csv: ", paste(not_zero, collapse = ", "), " is a death state (its rows always go back ",
         "to itself with probability 1), so it counts 0 QALYs in the year of death; set it to 0 or leave it out. ",
         "If it is a living state, its rows in the transition matrices need a chance of leaving it.",
         call. = FALSE)
  }
  # Death states that are not listed (in some or all strata and years) count 0.
  other_keys <- intersect(c("year", matrices$usual_care$strata), names(utilities))
  zeros <- cross_join(data.table::data.table(state = death), unique(utilities[, other_keys, with = FALSE]))
  zeros <- zeros[!utilities, on = c("state", other_keys)]
  if (nrow(zeros) > 0) {
    for (arm in arms) zeros[, (arm) := 0]
    utilities <- rbind(utilities, zeros, use.names = TRUE)
  }
  utilities
}

#' Read and check the intervention cost
#'
#' Reads `inputs/intervention_cost.csv`: columns `year` and `cost`, giving the
#' cost of delivering the iABC programme to one patient in each year after
#' their entry (`year` 1 is the first year after entry). Every year from 1 to
#' `horizon - 1` (`settings.yaml`) needs exactly one row; later years are
#' allowed and not used. Costs must be 0 or more. Any problem stops with an
#' error.
#'
#' The intervention cost is the same whatever the patient's state. It is
#' charged to iABC patients only, in each year after entry that they spend in a
#' living state: never in the entry year, the year of death, or usual care (see
#' [tm_add_outcomes()]). Differences in care between the arms that depend on
#' the state, such as medication, belong in `costs.csv`.
#'
#' @inheritParams tm_read_costs
#' @return A `data.table` with columns `year` and `cost`, one row per year.
#' @export
tm_read_intervention_cost <- function(settings = tm_read_settings()) {
  path <- input_path("intervention_cost")
  x <- data.table::fread(path)
  fail <- function(...) stop(path, ": ", ..., call. = FALSE)

  missing_cols <- setdiff(c("year", "cost"), names(x))
  if (length(missing_cols) > 0) fail("missing column(s): ", paste(missing_cols, collapse = ", "))
  extra_cols <- setdiff(names(x), c("year", "cost"))
  if (length(extra_cols) > 0) fail("unexpected column(s): ", paste(extra_cols, collapse = ", "))
  tryCatch({
    check_year_values(x$year)
    check_year_coverage(x$year, check_horizon(settings$horizon))
  }, error = function(e) fail(conditionMessage(e)))
  if (anyDuplicated(x$year) > 0) fail("duplicated years: ", format_values(unique(x$year[duplicated(x$year)])))
  if (!is.numeric(x$cost) || anyNA(x$cost)) fail("`cost` must be numbers with no missing values.")
  if (any(x$cost < 0)) fail("`cost` values must be 0 or more.")
  data.table::set(x, j = "cost", value = as.numeric(x$cost))
  x[order(year)]
}

# Reads a state x arm table from inputs/, with optional stratifier and `year` columns, checking states, strata, years,
# arms and values.
read_state_table <- function(name, matrices, settings, required, check, rule) {
  path <- input_path(name)
  x <- data.table::fread(path)
  fail <- function(...) stop(path, ": ", ..., call. = FALSE)
  arms <- names(matrices)
  probs <- matrices$usual_care$probs

  missing_cols <- setdiff(c("state", arms), names(x))
  if (length(missing_cols) > 0) fail("missing column(s): ", paste(missing_cols, collapse = ", "))
  strata <- intersect(matrices$usual_care$strata, names(x))
  extra_cols <- setdiff(names(x), c("state", "year", strata, arms))
  if (length(extra_cols) > 0) {
    fail("unexpected column(s): ", paste(extra_cols, collapse = ", "),
         ". Stratifier columns must be listed in `strata` in inputs/settings.yaml.")
  }
  has_year <- "year" %in% names(x)
  if (has_year) {
    tryCatch({
      check_year_values(x$year)
      check_year_coverage(unique(x$year), check_horizon(settings$horizon))
    }, error = function(e) fail(conditionMessage(e)))
  }
  for (s in strata) {
    if (anyNA(x[[s]])) fail("missing values in `", s, "`.")
    if (is.character(probs[[s]])) data.table::set(x, j = s, value = as.character(x[[s]]))
    unknown <- setdiff(unique(x[[s]]), unique(probs[[s]]))
    if (length(unknown) > 0) fail("`", s, "` values not in the transition matrices: ", format_values(unknown))
  }
  key_cols <- c("state", if (has_year) "year", strata)
  dup <- duplicated(x[, key_cols, with = FALSE])
  if (any(dup)) {
    first <- x[which(dup)[1], key_cols, with = FALSE]
    if (length(key_cols) == 1) fail("duplicated states: ", format_values(unique(x$state[dup])))
    fail(sum(dup), " duplicated rows (same ", paste(key_cols, collapse = ", "), "). First: ",
         paste(names(first), unlist(first), sep = " = ", collapse = ", "))
  }
  unknown <- setdiff(x$state, matrices$usual_care$states)
  if (length(unknown) > 0) fail("states not in the transition matrices: ", format_values(unknown))

  # Every required state needs a row for every year listed and every stratum of the matrices.
  grid <- data.table::data.table(state = required)
  if (has_year) grid <- cross_join(grid, data.table::data.table(year = sort(unique(x$year))))
  if (length(strata) > 0) grid <- cross_join(grid, unique(probs[, strata, with = FALSE]))
  missing <- grid[!x, on = key_cols]
  if (nrow(missing) > 0) {
    if (length(strata) == 0 && !has_year) fail("missing states: ", format_values(unique(missing$state)))
    if (length(strata) == 0) {
      yr <- min(missing$year)
      fail("missing states in year ", yr, ": ", format_values(missing[missing$year == yr]$state))
    }
    first <- missing[1]
    fail(nrow(missing), " missing rows: every state needs a row for every ",
         paste(setdiff(key_cols, "state"), collapse = ", "), " of the matrices. First: ",
         paste(names(first), unlist(first), sep = " = ", collapse = ", "))
  }
  for (arm in arms) {
    v <- x[[arm]]
    if (!is.numeric(v) || anyNA(v)) fail("`", arm, "` must be numbers with no missing values.")
    if (!check(v)) fail("`", arm, "` values must be ", rule, ".")
    data.table::set(x, j = arm, value = as.numeric(v))  # whole-number and decimal columns combine without a warning
  }
  data.table::setcolorder(x, c(key_cols, arms))
  x
}

# Every row of `a` paired with every row of `b`.
cross_join <- function(a, b) {
  if (ncol(b) == 0) return(data.table::copy(a))
  cbind(a[rep(seq_len(nrow(a)), each = nrow(b))], b[rep(seq_len(nrow(b)), times = nrow(a))])
}

#' Add costs and QALYs to the simulated years
#'
#' Gives every simulated patient-year the cost and QALY of its state in its arm,
#' and the discounted values. Discounting runs from cycle 0 for everyone: a year
#' in cycle `t` is multiplied by `1 / (1 + discount_rate)^t`, with `discount_rate`
#' from `settings.yaml`. The year of death carries its cost and 0 QALYs.
#'
#' A patient's entry year (their first simulated year) is the same in both arms:
#' the entry state is shared and the arms' matrices act only from the next year.
#' So in the entry year both arms take the `usual_care` cost and QALY of the
#' entry state, and each arm's own values apply from the year after entry, the
#' same year its matrix first applies.
#'
#' Costs or utilities with stratifier columns are matched on the patient's
#' stratum in that year (their current age group, sex and cluster); with a
#' `year` column, on the patient's year since entry, the entry year using year
#' 1's `usual_care` values.
#'
#' The intervention cost of year `k` after entry is added to the cost of every
#' iABC patient-year spent in a living state in year `k`. It is not charged in
#' the entry year, in the year of death, or in usual care.
#'
#' @param sim Simulated years, from [tm_simulate()].
#' @param costs Costs, from [tm_read_costs()].
#' @param utilities Utilities, from [tm_read_utilities()].
#' @param intervention_cost Intervention cost by year since entry, from
#'   [tm_read_intervention_cost()].
#' @param death Names of the death states.
#' @param settings Run settings, from [tm_read_settings()].
#' @return `sim` with added columns `cost` (the year's whole cost, intervention
#'   cost included), `intervention_cost` (the intervention cost within `cost`),
#'   `qaly`, `discount`, `cost_disc` and `qaly_disc`.
#' @export
tm_add_outcomes <- function(sim, costs, utilities, intervention_cost, death, settings = tm_read_settings()) {
  rate <- settings$discount_rate
  if (is.null(rate) || length(rate) != 1 || !is.numeric(rate) || rate < 0) {
    stop("inputs/settings.yaml needs `discount_rate`: a number, 0 or more (0.03 for 3%).", call. = FALSE)
  }
  out <- data.table::copy(sim)
  # Each year's values come from the patient's arm and year since entry. In the entry year both arms are in the
  # shared entry state and no arm's matrix has acted yet, so both take the usual-care values (of year 1, when the
  # values change by year); arm-specific values start the year after entry, with the arm's matrix.
  out[, since_entry := cycle - min(cycle), by = id]
  out[, `:=`(value_arm = data.table::fifelse(since_entry == 0L, "usual_care", arm),
             value_year = pmax(since_entry, 1L))]
  out <- attach_values(out, costs, "cost")
  out <- attach_values(out, utilities, "qaly")
  if (anyNA(out$cost) || anyNA(out$qaly)) {
    stop("Some simulated states have no cost or utility.", call. = FALSE)
  }
  # The intervention cost: iABC patients, years after entry, living states only.
  per_year <- stats::setNames(intervention_cost$cost, intervention_cost$year)
  out[, intervention_cost := 0]
  out[arm == "iabc" & since_entry >= 1L & !state %in% death,
      intervention_cost := unname(per_year[as.character(since_entry)])]
  if (anyNA(out$intervention_cost)) {
    stop("The intervention cost has no value for years since entry: ",
         format_values(sort(unique(out$since_entry[is.na(out$intervention_cost)]))), call. = FALSE)
  }
  out[, cost := cost + intervention_cost]
  out[, c("since_entry", "value_arm", "value_year") := NULL]
  out[, discount := 1 / (1 + rate)^cycle]
  out[, `:=`(cost_disc = cost * discount, qaly_disc = qaly * discount)]
  data.table::setcolorder(out, c(names(sim), "cost", "intervention_cost", "qaly"))
  data.table::setorderv(out, c("arm", "id", "cycle"))
  out[]
}

# Adds one value per simulated year from a state table (columns `state`, optional `year` and stratifier columns, and
# one per arm), matched on the state, `value_arm`, `value_year` (if the table has a `year` column) and the stratifiers.
attach_values <- function(out, table, value) {
  arm_cols <- intersect(c("usual_care", "iabc"), names(table))
  id_cols <- setdiff(names(table), arm_cols)
  strata <- setdiff(id_cols, c("state", "year"))
  missing <- setdiff(strata, names(out))
  if (length(missing) > 0) {
    stop("The simulated years have no ", paste0("`", missing, "`", collapse = ", "), " column to match the ", value,
         " values on; simulate with tm_simulate().", call. = FALSE)
  }
  long <- data.table::melt(table, id.vars = id_cols, measure.vars = arm_cols, variable.name = "value_arm",
                           value.name = value, variable.factor = FALSE)
  if ("year" %in% id_cols) data.table::setnames(long, "year", "value_year")
  for (s in strata) if (is.character(out[[s]])) data.table::set(long, j = s, value = as.character(long[[s]]))
  merge(out, long, by = c("state", "value_arm", if ("year" %in% id_cols) "value_year", strata), all.x = TRUE,
        sort = FALSE)
}
