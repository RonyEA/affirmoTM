#' Read, check and build the cohort
#'
#' Reads `inputs/cohort.csv`: one row per patient, with `id`, `age`, `entry_cycle`
#' and a column for every stratifier in `settings.yaml`. `age` is the patient's
#' age in whole years in their entry cycle (not at cycle 0): a patient aged 70
#' with `entry_cycle` 3 is 70 in cycle 3, 71 in cycle 4, and so on. `agegrp` is optional: if
#' it is missing it is worked out from `age` using the matrices' age groups, and
#' if it is present it must agree with `age`. These columns must have no missing
#' values; any other column is kept as it is and not checked. Any problem stops
#' with an error.
#'
#' Patients who enter in cycle `horizon` or later (from `settings.yaml`) are
#' never simulated: they are left out, with a warning listing them.
#'
#' The matrices must have rows for every stratum each patient can reach within
#' the horizon: their sex and cluster (or other fixed stratifiers) together with
#' every age group they pass through. A missing stratum stops with an error
#' naming it and the patients who need it.
#'
#' @param settings Run settings, from [tm_read_settings()].
#' @param matrices The two transition matrices, from [tm_read_matrices()].
#' @return A `data.table` with one row per patient.
#' @export
tm_read_cohort <- function(settings = tm_read_settings(), matrices = tm_read_matrices(settings)) {
  force(settings)
  path <- input_path("cohort")
  cohort <- data.table::fread(path)
  fail <- function(...) stop(path, ": ", ..., call. = FALSE)

  required <- c("id", "age", "entry_cycle", setdiff(settings$strata, "agegrp"))
  missing <- setdiff(required, names(cohort))
  if (length(missing) > 0) fail("missing column(s): ", paste(missing, collapse = ", "))
  # Only the columns the model uses must be complete; any other column is carried along as it is.
  for (col in intersect(c(required, "agegrp"), names(cohort))) {
    empty <- is.na(cohort[[col]]) | (is.character(cohort[[col]]) & cohort[[col]] == "")
    if (any(empty)) fail("missing values in `", col, "`, rows ", format_values(which(empty)))
  }
  if (anyDuplicated(cohort$id) > 0) {
    fail("duplicated ids: ", format_values(unique(cohort$id[duplicated(cohort$id)])))
  }
  whole <- function(x) is.numeric(x) && all(x == round(x))
  if (!whole(cohort$age)) fail("`age` must be whole years.")
  if (!whole(cohort$entry_cycle) || any(cohort$entry_cycle < 0)) {
    fail("`entry_cycle` must be a whole number, 0 or more.")
  }

  probs <- matrices$usual_care$probs
  bands <- NULL
  if ("agegrp" %in% settings$strata) {
    bands <- age_bands(probs$agegrp)
    outside <- cohort$age < min(bands$lower) | cohort$age > max(bands$upper)
    if (any(outside)) {
      fail("ages outside the matrices' age groups (", min(bands$lower), "-", max(bands$upper), ") for ids ",
           format_values(cohort$id[outside]))
    }
    derived <- bands$agegrp[findInterval(cohort$age, bands$lower)]
    if ("agegrp" %in% names(cohort)) {
      mismatch <- cohort$agegrp != derived
      if (any(mismatch)) fail("`agegrp` does not match `age` for ids ", format_values(cohort$id[mismatch]))
    } else {
      cohort[, agegrp := derived]
    }
  }
  for (s in setdiff(settings$strata, "agegrp")) {
    unknown <- setdiff(unique(cohort[[s]]), unique(probs[[s]]))
    if (length(unknown) > 0) {
      fail("`", s, "` values not in the transition matrices: ", format_values(unknown))
    }
  }

  horizon <- check_horizon(settings$horizon)
  late <- cohort$entry_cycle >= horizon
  if (any(late)) {
    warning(path, ": ", sum(late), " patients enter in cycle ", horizon, " or later, after the horizon of ", horizon,
            " cycles, and are left out (ids ", format_values(cohort$id[late]), ").", call. = FALSE)
    cohort <- cohort[!late]
  }
  check_strata_coverage(cohort, probs, settings$strata, horizon, bands, fail)

  data.table::setcolorder(cohort, intersect(c("id", "entry_cycle", "age", settings$strata), names(cohort)))
  cohort[]
}

# Stops unless the matrices have rows for every stratum a patient can use within the horizon: their fixed stratifiers
# with each age group they are in before a move. A move into cycle c uses the age group of cycle c - 1, so a patient
# entering in cycle e uses their ages at entry up to entry + horizon - 2 - e (at most the oldest age); patients who
# enter in the last cycle never move.
check_strata_coverage <- function(cohort, probs, strata, horizon, bands, fail) {
  if (length(strata) == 0) return(invisible(TRUE))
  moves <- horizon - 1L - cohort$entry_cycle
  movers <- which(moves > 0)
  fixed <- setdiff(strata, "agegrp")
  if (is.null(bands)) {
    reach <- cohort[movers, c("id", fixed), with = FALSE]
  } else {
    first <- findInterval(cohort$age[movers], bands$lower)
    last <- findInterval(pmin(cohort$age[movers] + moves[movers] - 1L, max(bands$upper)), bands$lower)
    reach <- cohort[rep(movers, last - first + 1L), c("id", fixed), with = FALSE]
    reach[, agegrp := bands$agegrp[unlist(Map(seq, first, last))]]
  }
  key <- function(d) do.call(paste, c(d[, strata, with = FALSE], sep = "\r"))
  gaps <- reach[!key(reach) %in% key(unique(probs[, strata, with = FALSE]))]
  if (nrow(gaps) > 0) {
    first_gap <- gaps[1, strata, with = FALSE]
    ids <- unique(gaps$id[key(gaps) == key(first_gap)])
    n_gaps <- data.table::uniqueN(gaps[, strata, with = FALSE])
    fail("the transition matrices have no rows for ", paste(names(first_gap), unlist(first_gap), sep = " = ",
         collapse = ", "), ", which ", length(ids), " patients reach within the horizon (ids ", format_values(ids), ")",
         if (n_gaps > 1) paste0("; ", n_gaps - 1, " more strata are missing"), ".")
  }
  invisible(TRUE)
}
