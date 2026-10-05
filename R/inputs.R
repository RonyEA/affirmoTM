# Fixed names of the files in inputs/. The working directory must be the top of the project.
input_files <- c(
  transition_matrix_usual_care = "transition_matrix_usual_care.csv",
  transition_matrix_iabc       = "transition_matrix_iabc.csv",
  entry_states                 = "entry_states.csv",
  cohort                       = "cohort.csv",
  costs                        = "costs.csv",
  intervention_cost            = "intervention_cost.csv",
  utilities                    = "utilities.csv",
  settings                     = "settings.yaml"
)

# Path to one input file; stops if inputs/ or the file is missing.
input_path <- function(name) {
  if (!dir.exists("inputs")) {
    stop("No inputs/ folder in the working directory (", getwd(), "). ",
         "Set the working directory to the top of the project.", call. = FALSE)
  }
  path <- file.path("inputs", input_files[[name]])
  if (!file.exists(path)) {
    stop(path, " not found.", call. = FALSE)
  }
  path
}

#' Read the run settings
#'
#' Reads `inputs/settings.yaml` and checks every setting's type and range (see
#' "Settings" below). Any problem stops with an error naming the setting; a
#' name that is not a known setting, such as a typo, gives a warning and is
#' ignored. Settings that depend on the other inputs (the states listed under
#' `events`, and `age_limit_death_state` being a death state) are checked once
#' the matrices are read.
#'
#' @section Settings:
#' * `strata`: the stratifier columns, `[]` for none (required);
#' * `seed`: a whole number (required);
#' * `horizon`: a whole number of cycles, 1 or more (required);
#' * `age_limit_death_state`: a death state's name (required when `agegrp` is a
#'   stratifier);
#' * `discount_rate`: a number, 0 or more (required);
#' * `n_runs`: a whole number, 1 or more (required);
#' * `events`: each event's name with the states it counts (optional);
#' * `currency`: text (optional; `€` by default);
#' * `wtp`: positive numbers, `[]` for none (optional).
#'
#' @return A list of settings.
#' @export
tm_read_settings <- function() {
  settings <- yaml::read_yaml(input_path("settings"))
  if (is.null(settings$strata)) {
    stop("inputs/settings.yaml needs a `strata` entry (use `strata: []` for none).", call. = FALSE)
  }
  settings$strata <- as.character(unlist(settings$strata))
  check_settings(settings)
  settings
}

# The settings the package uses; any other name in settings.yaml is reported as unknown.
known_settings <- c("strata", "seed", "horizon", "age_limit_death_state", "discount_rate", "n_runs", "events",
                    "currency", "wtp")

# Checks the type and range of every setting, so that a problem stops a run before anything is simulated or written.
# Warns about unknown names. Checks that need the matrices (event states, the age-limit death state) come later.
check_settings <- function(settings) {
  needs <- function(...) stop("inputs/settings.yaml needs ", ..., call. = FALSE)
  bad <- function(...) stop("inputs/settings.yaml: ", ..., call. = FALSE)
  whole <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x) && x == round(x)
  yaml_hint <- " Quote names that YAML reads as true or false, such as \"No\" or \"Yes\"."

  for (name in setdiff(names(settings), known_settings)) {
    distance <- utils::adist(name, known_settings)[1, ]
    warning("inputs/settings.yaml: `", name, "` is not a known setting and is ignored",
            if (min(distance) <= 3) paste0("; did you mean `", known_settings[which.min(distance)], "`?") else ".",
            call. = FALSE)
  }
  if (!is.character(settings$strata) || anyNA(settings$strata)) {
    needs("a `strata` entry: the stratifier column names (use `strata: []` for none).")
  }
  if (!whole(settings$n_runs) || settings$n_runs < 1) needs("`n_runs`: a whole number, 1 or more.")
  if (!whole(settings$seed) || abs(settings$seed) + settings$n_runs > .Machine$integer.max) {
    needs("`seed`: a whole number (at most ", .Machine$integer.max - settings$n_runs, " with ", settings$n_runs, " runs).")
  }
  check_horizon(settings$horizon)
  rate <- settings$discount_rate
  if (!is.numeric(rate) || length(rate) != 1 || is.na(rate) || rate < 0) {
    needs("`discount_rate`: a number, 0 or more (0.03 for 3%).")
  }
  limit <- settings$age_limit_death_state
  if ("agegrp" %in% settings$strata && !(is.character(limit) && length(limit) == 1 && !is.na(limit) && nzchar(limit))) {
    needs("`age_limit_death_state`: the death state of patients who would pass the oldest age group.", yaml_hint)
  }
  events <- settings$events
  if (length(events) > 0) {
    if (!is.list(events) || is.null(names(events)) || any(!nzchar(names(events)))) {
      bad("`events` must give each event a name and its states, for example `stroke: [AIS, AIS_Death]`.")
    }
    for (event in names(events)) {
      states <- unlist(events[[event]])
      if (length(states) == 0 || !is.character(states) || anyNA(states)) {
        bad("event `", event, "` must list one or more state names.", yaml_hint)
      }
    }
  }
  currency <- settings$currency
  if (!is.null(currency) && !(is.character(currency) && length(currency) == 1 && !is.na(currency))) {
    bad("`currency` must be text, for example \"\u20ac\".")
  }
  check_wtp(settings$wtp)
  invisible(TRUE)
}

#' Read and check the two transition matrices
#'
#' Reads `inputs/transition_matrix_usual_care.csv` and
#' `inputs/transition_matrix_iabc.csv`, checks each one with [tm_matrix()], and
#' checks that they match: the same states, stratifiers, strata and death
#' states. Any problem stops with an error.
#'
#' A matrix may have a `year` column (years since entry; see [tm_matrix()]) so
#' that its probabilities change over time. It must then list every year from 1
#' to `horizon - 1` (`settings.yaml`); later years are allowed and not used.
#' Either matrix, or both, may have one.
#'
#' @param settings Run settings, from [tm_read_settings()].
#' @return A list with `usual_care` and `iabc`, each a `tm_matrix`.
#' @export
tm_read_matrices <- function(settings = tm_read_settings()) {
  force(settings)
  arms <- c(usual_care = "transition_matrix_usual_care", iabc = "transition_matrix_iabc")
  matrices <- lapply(arms, function(name) {
    path <- input_path(name)
    x <- data.table::fread(path)
    if ("sim" %in% names(x)) {
      stop(path, " has a `sim` column. Matrices in inputs/ hold point values only.", call. = FALSE)
    }
    m <- tryCatch(
      tm_matrix(x, strata = settings$strata),
      error = function(e) stop(path, ": ", conditionMessage(e), call. = FALSE)
    )
    if ("agegrp" %in% m$strata) {
      tryCatch(age_bands(m$probs$agegrp),
               error = function(e) stop(path, ": ", conditionMessage(e), call. = FALSE))
    }
    if (!is.null(m$years)) {
      tryCatch(check_year_coverage(m$years, check_horizon(settings$horizon)),
               error = function(e) stop(path, ": ", conditionMessage(e), call. = FALSE))
    }
    m
  })
  check_matrices_match(matrices$usual_care, matrices$iabc)
  matrices
}

# Both arms must have the same states, death states, stratifiers and strata.
check_matrices_match <- function(uc, iabc) {
  differ <- function(what, a, b) {
    only_uc <- setdiff(a, b)
    only_iabc <- setdiff(b, a)
    if (length(only_uc) + length(only_iabc) > 0) {
      stop("The usual-care and iABC matrices have different ", what, ".",
           if (length(only_uc) > 0) paste0(" Only in usual care: ", format_values(only_uc), "."),
           if (length(only_iabc) > 0) paste0(" Only in iABC: ", format_values(only_iabc), "."),
           call. = FALSE)
    }
  }
  differ("states", uc$states, iabc$states)
  differ("death states", uc$death, iabc$death)
  differ("stratifiers", uc$strata, iabc$strata)
  if (length(uc$strata) > 0) {
    key <- function(m) do.call(paste, c(unique(m$probs[, m$strata, with = FALSE]), sep = " / "))
    differ(paste0("strata (", paste(uc$strata, collapse = " / "), ")"), key(uc), key(iabc))
  }
  invisible(TRUE)
}

# Reads age-group labels such as "65-69" as ranges and checks they follow on without gaps
# or overlaps. Returns the bands in age order.
age_bands <- function(labels) {
  labels <- unique(as.character(labels))
  parts <- regmatches(labels, regexec("^([0-9]+)-([0-9]+)$", labels))
  bad <- labels[lengths(parts) != 3]
  if (length(bad) > 0) {
    stop("Age groups must be written as ranges such as '65-69': ", paste(bad, collapse = ", "), call. = FALSE)
  }
  bands <- data.table::data.table(
    agegrp = labels,
    lower = as.integer(vapply(parts, `[`, "", 2)),
    upper = as.integer(vapply(parts, `[`, "", 3))
  )
  if (any(bands$upper < bands$lower)) {
    stop("Age groups with the upper age below the lower age: ",
         paste(bands$agegrp[bands$upper < bands$lower], collapse = ", "), call. = FALSE)
  }
  bands <- bands[order(bands$lower)]
  breaks <- which(bands$lower[-1] != bands$upper[-nrow(bands)] + 1)
  if (length(breaks) > 0) {
    stop("Age groups must follow on without gaps or overlaps: ",
         paste0(bands$agegrp[breaks], " is followed by ", bands$agegrp[breaks + 1], collapse = "; "),
         call. = FALSE)
  }
  bands
}

# Stops unless the years of a `year` column cover every year after entry within the horizon (1 to horizon - 1).
check_year_coverage <- function(years, horizon) {
  missing <- setdiff(seq_len(horizon - 1), years)
  if (length(missing) > 0) {
    stop("the `year` column must list every year from 1 to ", horizon - 1, " (horizon - 1); missing: ",
         format_values(missing), call. = FALSE)
  }
  invisible(TRUE)
}

format_values <- function(values, n = 10) {
  shown <- paste(utils::head(values, n), collapse = ", ")
  if (length(values) > n) paste0(shown, " and ", length(values) - n, " more") else shown
}
