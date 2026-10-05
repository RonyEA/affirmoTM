#' Summarise each arm of one run
#'
#' Totals and per-patient means of the discounted costs and QALYs, plus event
#' counts, for each arm. The intervention cost, part of the cost, is also shown
#' on its own (`intervention_cost_total`, `intervention_cost_per_patient`;
#' discounted). Every patient who enters within the horizon counts,
#' including late entrants and deaths in the entry year. Each year a patient
#' spends in a state listed under an event in `settings.yaml` counts as one
#' event; deaths are the years spent in a death state.
#'
#' @param out Simulated years with costs and QALYs, from [tm_add_outcomes()].
#' @param death Names of the death states.
#' @param settings Run settings, from [tm_read_settings()].
#' @return A `data.table` with one row per arm.
#' @export
tm_summarise <- function(out, death, settings = tm_read_settings()) {
  summary <- out[, .(
    patients = data.table::uniqueN(id),
    cost_total = sum(cost_disc),
    qaly_total = sum(qaly_disc),
    intervention_cost_total = sum(intervention_cost * discount)
  ), by = arm]
  summary[, `:=`(cost_per_patient = cost_total / patients, qaly_per_patient = qaly_total / patients,
                 intervention_cost_per_patient = intervention_cost_total / patients)]
  for (event in names(settings$events)) {
    counts <- out[, .(n = sum(state %in% settings$events[[event]])), by = arm]
    summary[counts, (event) := i.n, on = "arm"]
  }
  deaths <- out[, .(n = sum(state %in% death)), by = arm]
  summary[deaths, deaths := i.n, on = "arm"]
  summary[]
}

# The summary columns of each arm, apart from event counts and deaths (see tm_summarise()).
summary_columns <- c("patients", "cost_total", "qaly_total", "intervention_cost_total", "cost_per_patient",
                     "qaly_per_patient", "intervention_cost_per_patient")

#' Compare the arms
#'
#' Differences between iABC and usual care (iABC minus usual care) for each run:
#' incremental discounted cost and QALYs per patient, the part of the
#' incremental cost that is intervention cost (`delta_intervention_cost`), and
#' the difference in each event count and in deaths. Overall, the mean of each difference across runs,
#' and one ICER: mean incremental cost divided by mean incremental QALYs.
#'
#' The ICER alone can mislead: a negative ICER can mean iABC is cheaper and
#' better, or costlier and worse. `quadrant` says where the mean lies on the
#' cost-effectiveness plane:
#' * `"costlier, more QALYs"`: the ICER is the cost per QALY gained;
#' * `"dominant"`: iABC is cheaper (or no costlier) and gives more QALYs;
#' * `"dominated"`: iABC is costlier (or no cheaper) and gives fewer QALYs;
#' * `"cheaper, fewer QALYs"`: the ICER is the saving per QALY lost;
#' * `"no QALY difference"`: the ICER is undefined.
#'
#' @param by_run Per-run summaries, as in `tm_run()$by_run`.
#' @param intervention,comparator Arm names.
#' @return A list with `by_run` (one row per run) and `overall` (one row, with
#'   `icer` and `quadrant`).
#' @export
tm_compare <- function(by_run, intervention = "iabc", comparator = "usual_care") {
  counts <- setdiff(names(by_run), c("run", "seed", "arm", summary_columns))
  value <- function(arm_name, col) by_run[arm == arm_name][order(run)][[col]]
  runs <- by_run[arm == comparator][order(run)]
  diff <- data.table::data.table(
    run = runs$run,
    seed = runs$seed,
    delta_cost = value(intervention, "cost_per_patient") - value(comparator, "cost_per_patient"),
    delta_intervention_cost = value(intervention, "intervention_cost_per_patient") -
      value(comparator, "intervention_cost_per_patient"),
    delta_qaly = value(intervention, "qaly_per_patient") - value(comparator, "qaly_per_patient")
  )
  for (col in counts) diff[, (paste0("delta_", col)) := value(intervention, col) - value(comparator, col)]

  measures <- setdiff(names(diff), c("run", "seed"))
  overall <- diff[, lapply(.SD, mean), .SDcols = measures]
  overall[, icer := delta_cost / delta_qaly]
  overall[, quadrant := icer_quadrant(delta_cost, delta_qaly)]
  data.table::setcolorder(overall, c("delta_cost", "delta_intervention_cost", "delta_qaly", "icer", "quadrant"))
  list(by_run = diff, overall = overall[])
}

# Where incremental cost and QALYs lie on the cost-effectiveness plane; see tm_compare().
icer_quadrant <- function(delta_cost, delta_qaly) {
  data.table::fcase(
    delta_qaly > 0 & delta_cost > 0, "costlier, more QALYs",
    delta_qaly > 0, "dominant",
    delta_qaly < 0 & delta_cost < 0, "cheaper, fewer QALYs",
    delta_qaly < 0, "dominated",
    default = "no QALY difference"
  )
}

# The ICER in words for its quadrant, e.g. "€11,617 per QALY gained" or "iABC dominated (costlier, fewer QALYs)".
icer_text <- function(icer, quadrant, currency) {
  amount <- paste0(currency, format(round(abs(icer)), big.mark = ",", trim = TRUE))
  switch(quadrant,
    "costlier, more QALYs" = paste(amount, "per QALY gained"),
    "dominant" = "iABC dominant (cheaper, more QALYs)",
    "dominated" = "iABC dominated (costlier, fewer QALYs)",
    "cheaper, fewer QALYs" = paste(amount, "saved per QALY lost"),
    "undefined (no difference in QALYs)"
  )
}

#' Run the model
#'
#' Reads every input from `inputs/` (including the intervention cost, see
#' [tm_read_intervention_cost()]), then runs the whole simulation `n_runs`
#' times (from `settings.yaml`). Run `r` uses seed `seed + r - 1`. Each run
#' assigns entry states, simulates both arms, adds costs and QALYs and
#' summarises each arm. The results are returned and written to `outputs/`:
#'
#' * `outputs/summary_by_run.csv`: one row per run and arm;
#' * `outputs/summary.csv`: the mean across runs, one row per arm;
#' * `outputs/comparison_by_run.csv`: iABC minus usual care, one row per run;
#' * `outputs/comparison.csv`: the mean differences and the ICER;
#' * `outputs/ce_plane.png`: the cost-effectiveness plane (see [plot.tm_results()]).
#'
#' The ICER is mean incremental cost divided by mean incremental QALYs across
#' runs; see [tm_compare()].
#'
#' The files in `outputs/` are overwritten on every call. Every input, the
#' settings included (see [tm_read_settings()]), is checked before the first
#' run, so a problem stops `tm_run()` before anything is simulated or written.
#' A message then lists the death states found in the matrices (see
#' [tm_matrix()]), so that a living state taken for death would stand out.
#'
#' @param settings Run settings, from [tm_read_settings()]. Settings changed in
#'   R are checked in the same way.
#' @return An object of class `tm_results`: a list with `by_run`, `summary`,
#'   `comparison_by_run` and `comparison`.
#' @export
tm_run <- function(settings = tm_read_settings()) {
  if (!missing(settings)) check_settings(settings)  # tm_read_settings() has already checked the default
  n_runs <- settings$n_runs

  matrices <- tm_read_matrices(settings)
  check_events(settings$events, matrices$usual_care$states)
  cohort <- tm_read_cohort(settings, matrices)
  entry_states <- tm_read_entry_states(settings, matrices)
  entry_rows(cohort, entry_states, settings$strata)  # every patient's entry stratum has a row, before any run
  costs <- tm_read_costs(settings, matrices)
  utilities <- tm_read_utilities(settings, matrices)
  intervention_cost <- tm_read_intervention_cost(settings)
  death <- matrices$usual_care$death
  message("Death states: ", paste(death, collapse = ", "), ".")

  by_run <- vector("list", n_runs)
  for (r in seq_len(n_runs)) {
    seed <- settings$seed + r - 1
    patients <- tm_assign_entry_states(cohort, entry_states, settings, seed = seed)
    sim <- tm_simulate(patients, matrices, settings, seed = seed)
    out <- tm_add_outcomes(sim, costs, utilities, intervention_cost, death, settings)
    by_run[[r]] <- tm_summarise(out, death, settings)[, `:=`(run = r, seed = seed)]
    if (r %% 10 == 0 || r == n_runs) message("Run ", r, " of ", n_runs, " done.")
  }
  by_run <- data.table::rbindlist(by_run)
  data.table::setcolorder(by_run, c("run", "seed", "arm"))

  measures <- setdiff(names(by_run), c("run", "seed", "arm"))
  summary <- by_run[, lapply(.SD, mean), by = arm, .SDcols = measures]

  comparison <- tm_compare(by_run)

  dir.create("outputs", showWarnings = FALSE)
  data.table::fwrite(by_run, file.path("outputs", "summary_by_run.csv"))
  data.table::fwrite(summary, file.path("outputs", "summary.csv"))
  data.table::fwrite(comparison$by_run, file.path("outputs", "comparison_by_run.csv"))
  data.table::fwrite(comparison$overall, file.path("outputs", "comparison.csv"))

  results <- structure(list(by_run = by_run, summary = summary, comparison_by_run = comparison$by_run,
                            comparison = comparison$overall, n_runs = n_runs,
                            currency = if (is.null(settings$currency)) "\u20ac" else settings$currency,
                            wtp = check_wtp(settings$wtp)),
                       class = "tm_results")
  ggplot2::ggsave(file.path("outputs", "ce_plane.png"), ce_plane(results), width = 7, height = 5, dpi = 150)
  results
}

check_events <- function(events, states) {
  if (is.null(events)) return(invisible(TRUE))
  # Each event becomes a column of the summary, next to these.
  reserved <- c("run", "seed", "arm", summary_columns, "deaths")
  clash <- intersect(names(events), reserved)
  if (length(clash) > 0) {
    stop("inputs/settings.yaml: event names cannot be ", paste(reserved, collapse = ", "), "; rename ",
         paste(clash, collapse = ", "), ".", call. = FALSE)
  }
  for (event in names(events)) {
    unknown <- setdiff(unlist(events[[event]]), states)
    if (length(unknown) > 0) {
      stop("inputs/settings.yaml: event `", event, "` lists states not in the transition matrices: ",
           paste(unknown, collapse = ", "), call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' @export
print.tm_results <- function(x, ...) {
  cat("<tm_results> mean per arm over", x$n_runs, "runs (discounted costs and QALYs)\n\n")
  shown <- data.table::copy(x$summary)
  numeric_cols <- names(shown)[vapply(shown, is.numeric, logical(1))]
  shown[, (numeric_cols) := lapply(.SD, function(v) round(v, 3)), .SDcols = numeric_cols]
  print(shown[])
  cmp <- x$comparison
  cat("\niABC minus usual care (mean over runs, per patient):\n")
  cat("  Incremental cost:  ", paste0(x$currency, format(round(cmp$delta_cost, 2), big.mark = ",")),
      paste0("(intervention cost: ", x$currency, format(round(cmp$delta_intervention_cost, 2), big.mark = ","), ")"), "\n")
  cat("  Incremental QALYs: ", format(round(cmp$delta_qaly, 5)), "\n")
  cat("  ICER:              ", icer_text(cmp$icer, cmp$quadrant, x$currency), "\n")
  events <- grep("^delta_", setdiff(names(cmp), c("delta_cost", "delta_intervention_cost", "delta_qaly")), value = TRUE)
  if (length(events) > 0) {
    cat("  Difference in events (whole cohort):",
        paste0(sub("^delta_", "", events), " ", sprintf("%+.1f", unlist(cmp[, events, with = FALSE])), collapse = ", "), "\n")
  }
  cat("\nWritten to outputs/: summary.csv, summary_by_run.csv, comparison.csv, comparison_by_run.csv, ce_plane.png\n")
  invisible(x)
}

check_wtp <- function(wtp) {
  wtp <- unlist(wtp)
  if (is.null(wtp)) return(numeric(0))
  if (!is.numeric(wtp) || anyNA(wtp) || any(wtp <= 0)) {
    stop("inputs/settings.yaml: `wtp` must be a list of positive numbers (or [] for none).", call. = FALSE)
  }
  sort(wtp)
}
