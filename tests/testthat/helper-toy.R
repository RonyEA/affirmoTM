# A small toy project for tests: states Well, Sick and Dead, stratified by age group and sex, so that
# results can be worked out by hand.

toy_bands <- c("65-79", "80-109")

# One matrix row per age group, sex and current state; Dead always stays Dead. Probabilities can differ by age group.
toy_matrix <- function(p_sick = 0.1, p_dead_well = 0.05, p_dead_sick = 0.2, bands = toy_bands,
                       p_dead_well_by_band = NULL) {
  rows <- data.table::CJ(agegrp = bands, sex = c("men", "women"), from = c("Well", "Sick", "Dead"), sorted = FALSE)
  dead_well <- if (is.null(p_dead_well_by_band)) rep(p_dead_well, nrow(rows)) else p_dead_well_by_band[rows$agegrp]
  rows[, `:=`(
    Well = data.table::fcase(from == "Well", 1 - p_sick - dead_well, default = 0),
    Sick = data.table::fcase(from == "Well", p_sick, from == "Sick", 1 - p_dead_sick, default = 0),
    Dead = data.table::fcase(from == "Well", dead_well, from == "Sick", p_dead_sick, default = 1)
  )]
  rows[]
}

toy_cohort <- function(n = 10, age = 70, sex = "men", entry_cycle = 0) {
  data.table::data.table(id = seq_len(n), age = age, sex = sex, entry_cycle = entry_cycle)
}

toy_entry_states <- function(bands = toy_bands, well = 1) {
  rows <- data.table::CJ(agegrp = bands, sex = c("men", "women"), sorted = FALSE)
  rows[, `:=`(Well = well, Sick = 1 - well, Dead = 0)]
  rows[]
}

toy_settings <- function(...) {
  settings <- list(
    strata = c("agegrp", "sex"),
    seed = 1,
    horizon = 5,
    age_limit_death_state = "Dead",
    discount_rate = 0,
    n_runs = 2,
    events = list(sick = "Sick"),
    currency = "€",
    wtp = numeric(0)
  )
  utils::modifyList(settings, list(...))
}

# Intervention cost by year since entry; years 1 to 20 cover every toy horizon (later years are not used).
toy_intervention_cost <- function(first_years = numeric(0)) {
  data.table::data.table(year = 1:20, cost = c(first_years, rep(0, 20 - length(first_years))))
}

# Writes a complete toy inputs/ folder into `dir`.
write_toy_inputs <- function(dir,
                             usual_care = toy_matrix(),
                             iabc = toy_matrix(),
                             cohort = toy_cohort(),
                             entry_states = toy_entry_states(),
                             costs = data.table::data.table(state = c("Well", "Sick", "Dead"),
                                                            usual_care = c(100, 1000, 500),
                                                            iabc = c(100, 1000, 500)),
                             utilities = data.table::data.table(state = c("Well", "Sick"),
                                                                usual_care = c(0.9, 0.5),
                                                                iabc = c(0.9, 0.5)),
                             intervention_cost = toy_intervention_cost(),
                             settings = toy_settings()) {
  inputs <- file.path(dir, "inputs")
  dir.create(inputs, recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(usual_care, file.path(inputs, "transition_matrix_usual_care.csv"))
  data.table::fwrite(iabc, file.path(inputs, "transition_matrix_iabc.csv"))
  data.table::fwrite(cohort, file.path(inputs, "cohort.csv"))
  data.table::fwrite(entry_states, file.path(inputs, "entry_states.csv"))
  data.table::fwrite(costs, file.path(inputs, "costs.csv"))
  data.table::fwrite(utilities, file.path(inputs, "utilities.csv"))
  data.table::fwrite(intervention_cost, file.path(inputs, "intervention_cost.csv"))
  yaml::write_yaml(settings, file.path(inputs, "settings.yaml"))
  invisible(dir)
}

# Creates a toy project in a temporary folder and makes it the working directory for the calling test.
local_toy_project <- function(..., env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  write_toy_inputs(dir, ...)
  withr::local_dir(dir, .local_envir = env)
  dir
}

# Reads every toy input and runs one simulation, returning all pieces.
toy_run <- function(seed = 1) {
  settings <- tm_read_settings()
  matrices <- tm_read_matrices(settings)
  cohort <- tm_read_cohort(settings, matrices)
  entry <- tm_read_entry_states(settings, matrices)
  patients <- tm_assign_entry_states(cohort, entry, settings, seed = seed)
  sim <- tm_simulate(patients, matrices, settings, seed = seed)
  list(settings = settings, matrices = matrices, patients = patients, sim = sim)
}

# Adds costs and QALYs to a toy_run(), reading the costs, utilities and intervention cost from inputs/.
toy_outcomes <- function(run) {
  tm_add_outcomes(run$sim, tm_read_costs(run$settings, run$matrices), tm_read_utilities(run$settings, run$matrices),
                  tm_read_intervention_cost(run$settings), run$matrices$usual_care$death, run$settings)
}
