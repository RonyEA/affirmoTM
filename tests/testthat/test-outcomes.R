# tm_read_costs(), tm_read_utilities(), tm_read_intervention_cost() and tm_add_outcomes().

read_toy_values <- function() {
  settings <- tm_read_settings()
  matrices <- tm_read_matrices(settings)
  list(costs = tm_read_costs(settings, matrices), utilities = tm_read_utilities(settings, matrices))
}

test_that("costs and utilities are read, with death states counting 0 QALYs", {
  local_toy_project()
  values <- read_toy_values()
  expect_equal(values$costs$state, c("Well", "Sick", "Dead"))
  expect_equal(values$utilities[state == "Dead", c(usual_care, iabc)], c(0, 0))
})

test_that("broken cost and utility files are rejected", {
  costs <- data.table::data.table(state = c("Well", "Sick", "Dead"), usual_care = c(100, 1000, 500),
                                  iabc = c(100, 1000, 500))
  local_toy_project(costs = costs[state != "Dead"])
  expect_error(read_toy_values(), "costs.csv: missing states: Dead")
  local_toy_project(costs = data.table::copy(costs)[1, iabc := -1])
  expect_error(read_toy_values(), "`iabc` values must be 0 or more")
  local_toy_project(costs = costs[, -"iabc"])
  expect_error(read_toy_values(), "missing column\\(s\\): iabc")
  local_toy_project(costs = data.table::copy(costs)[, extra := 1])
  expect_error(read_toy_values(), "unexpected column\\(s\\): extra")

  utilities <- data.table::data.table(state = c("Well", "Sick"), usual_care = c(0.9, 0.5), iabc = c(0.9, 0.5))
  local_toy_project(utilities = data.table::copy(utilities)[1, usual_care := 1.2])
  expect_error(read_toy_values(), "`usual_care` values must be at most 1")
  local_toy_project(utilities = rbind(utilities, data.table::data.table(state = "Dead", usual_care = 0.2, iabc = 0)))
  expect_error(read_toy_values(), "Dead is a death state .*counts 0 QALYs")
  local_toy_project(utilities = utilities[state != "Sick"])
  expect_error(read_toy_values(), "utilities.csv: missing states: Sick")
})

test_that("a living state that is never left is caught through its utility", {
  # Sick always stays Sick, so it reads as a death state; its utility of 0.5 shows it was meant to be living.
  never_leave <- toy_matrix(p_dead_sick = 0)
  local_toy_project(usual_care = never_leave, iabc = never_leave)
  expect_error(read_toy_values(), "Sick is a death state .*If it is a living state, its rows .*need a chance of leaving it")
})

test_that("costs and QALYs are attached and discounted from cycle 0", {
  certain_death <- toy_matrix(p_sick = 0, p_dead_well = 1)
  local_toy_project(usual_care = certain_death, iabc = certain_death, cohort = toy_cohort(n = 1),
                    settings = toy_settings(discount_rate = 0.03))
  out <- toy_outcomes(toy_run())[arm == "usual_care"]
  expect_equal(out$state, c("Well", "Dead"))
  expect_equal(out$cost, c(100, 500))
  expect_equal(out$qaly, c(0.9, 0))
  expect_equal(out$discount, c(1, 1 / 1.03))
  expect_equal(out$cost_disc, c(100, 500 / 1.03))
  expect_equal(out$qaly_disc, c(0.9, 0))
})

test_that("the entry year takes the usual-care values in both arms; each arm's own values start the year after", {
  # Patients aged 70 stay Well for the whole horizon. Patient 1 enters in cycle 0, patient 2 in cycle 1.
  stay_well <- toy_matrix(p_sick = 0, p_dead_well_by_band = c("65-79" = 0, "80-109" = 0.05))
  costs <- data.table::data.table(state = c("Well", "Sick", "Dead"), usual_care = c(100, 1000, 500),
                                  iabc = c(200, 1000, 600))
  utilities <- data.table::data.table(state = c("Well", "Sick"), usual_care = c(0.9, 0.5), iabc = c(1, 0.5))
  local_toy_project(usual_care = stay_well, iabc = stay_well, cohort = toy_cohort(n = 2, entry_cycle = c(0, 1)),
                    costs = costs, utilities = utilities, settings = toy_settings(horizon = 3))
  out <- toy_outcomes(toy_run())

  expect_equal(out[arm == "usual_care" & id == 1, cost], c(100, 100, 100))
  expect_equal(out[arm == "iabc" & id == 1, cost], c(100, 200, 200))
  expect_equal(out[arm == "iabc" & id == 1, qaly], c(0.9, 1, 1))
  # The entry year is each patient's own: cycle 1 for the late entrant.
  expect_equal(out[arm == "iabc" & id == 2, cycle], 1:2)
  expect_equal(out[arm == "iabc" & id == 2, cost], c(100, 200))
  expect_equal(out[arm == "iabc" & id == 2, qaly], c(0.9, 1))

  # A patient who enters in a death state is charged the usual-care cost of that state in both arms.
  local_toy_project(usual_care = stay_well, iabc = stay_well, cohort = toy_cohort(n = 1),
                    entry_states = toy_entry_states()[, `:=`(Well = 0, Dead = 1)], costs = costs,
                    utilities = utilities, settings = toy_settings(horizon = 3))
  out <- toy_outcomes(toy_run())
  expect_equal(out$state, c("Dead", "Dead"))
  expect_equal(out$cost, c(500, 500))
  expect_equal(out$qaly, c(0, 0))
})

test_that("costs and utilities with a year column follow each patient's year since entry", {
  stay_well <- toy_matrix(p_sick = 0, p_dead_well_by_band = c("65-79" = 0, "80-109" = 0.05))
  costs <- data.table::data.table(state = rep(c("Well", "Sick", "Dead"), each = 2), year = rep(1:2, 3),
                                  usual_care = c(100, 150, 1000, 1000, 500, 500),
                                  iabc = c(300, 150, 1000, 1000, 500, 500))
  utilities <- data.table::data.table(state = rep(c("Well", "Sick"), each = 2), year = rep(1:2, 2),
                                      usual_care = c(0.9, 0.8, 0.5, 0.5), iabc = c(1, 0.8, 0.5, 0.5))
  local_toy_project(usual_care = stay_well, iabc = stay_well, cohort = toy_cohort(n = 2, entry_cycle = c(0, 1)),
                    costs = costs, utilities = utilities, settings = toy_settings(horizon = 3))
  run <- toy_run()
  values <- read_toy_values()
  expect_equal(values$utilities[state == "Dead", year], 1:2)  # death states count 0 in every year
  out <- toy_outcomes(run)

  # Patient 1: the entry year (cycle 0) takes year 1's usual-care values in both arms, then years 1 and 2.
  expect_equal(out[arm == "usual_care" & id == 1, cost], c(100, 100, 150))
  expect_equal(out[arm == "iabc" & id == 1, cost], c(100, 300, 150))
  expect_equal(out[arm == "iabc" & id == 1, qaly], c(0.9, 1, 0.8))
  # Patient 2 enters in cycle 1, so cycle 2 is their year 1.
  expect_equal(out[arm == "iabc" & id == 2, cost], c(100, 300))
})

test_that("broken year columns in costs and utilities are rejected", {
  costs <- data.table::data.table(state = rep(c("Well", "Sick", "Dead"), each = 2), year = rep(1:2, 3),
                                  usual_care = 100, iabc = 100)
  local_toy_project(costs = costs[year == 1], settings = toy_settings(horizon = 3))
  expect_error(read_toy_values(),
               "costs.csv: the `year` column must list every year from 1 to 2 \\(horizon - 1\\); missing: 2")
  local_toy_project(costs = costs[!(state == "Sick" & year == 2)], settings = toy_settings(horizon = 3))
  expect_error(read_toy_values(), "costs.csv: missing states in year 2: Sick")
  local_toy_project(costs = rbind(costs, costs[1]), settings = toy_settings(horizon = 3))
  expect_error(read_toy_values(), "costs.csv: 1 duplicated rows \\(same state, year\\). First: state = Well, year = 1")
  local_toy_project(costs = data.table::copy(costs)[1, year := 0L], settings = toy_settings(horizon = 3))
  expect_error(read_toy_values(), "costs.csv: `year` must hold whole numbers, 1 or more")

  utilities <- data.table::data.table(state = rep(c("Well", "Sick"), each = 2), year = rep(1:2, 2),
                                      usual_care = 0.5, iabc = 0.5)
  local_toy_project(utilities = utilities[!(state == "Well" & year == 1)], settings = toy_settings(horizon = 3))
  expect_error(read_toy_values(), "utilities.csv: missing states in year 1: Well")
})

test_that("costs and utilities with stratifier columns follow each patient's stratum, as they age", {
  # Patients aged 78 stay Well for 3 cycles and move into the 80-109 age group in cycle 2.
  stay_well <- toy_matrix(p_sick = 0, p_dead_well_by_band = c("65-79" = 0, "80-109" = 0.05))
  costs <- data.table::data.table(state = rep(c("Well", "Sick", "Dead"), each = 2), sex = c("men", "women"),
                                  usual_care = c(100, 120, 1000, 1000, 500, 500),
                                  iabc = c(110, 130, 1000, 1000, 500, 500))
  utilities <- data.table::data.table(state = rep(c("Well", "Sick"), each = 2), agegrp = c("65-79", "80-109"),
                                      usual_care = c(0.9, 0.7, 0.5, 0.4), iabc = c(0.9, 0.7, 0.5, 0.4))
  local_toy_project(usual_care = stay_well, iabc = stay_well, costs = costs, utilities = utilities,
                    cohort = toy_cohort(n = 2, age = 78, sex = c("men", "women")), settings = toy_settings(horizon = 3))
  values <- read_toy_values()
  expect_setequal(values$utilities[state == "Dead", agegrp], c("65-79", "80-109"))  # death states count 0 everywhere
  out <- toy_outcomes(toy_run())

  expect_equal(out[arm == "usual_care" & id == 1, cost], c(100, 100, 100))
  expect_equal(out[arm == "usual_care" & id == 2, cost], c(120, 120, 120))
  # The entry year takes the usual-care cost of the patient's own stratum.
  expect_equal(out[arm == "iabc" & id == 2, cost], c(120, 130, 130))
  # Utility follows the age group in each cycle.
  expect_equal(out[arm == "iabc" & id == 1, agegrp], c("65-79", "65-79", "80-109"))
  expect_equal(out[arm == "iabc" & id == 1, qaly], c(0.9, 0.9, 0.7))
})

test_that("broken stratifier columns in costs and utilities are rejected", {
  costs <- data.table::data.table(state = rep(c("Well", "Sick", "Dead"), each = 2), sex = c("men", "women"),
                                  usual_care = 100, iabc = 100)
  local_toy_project(costs = costs[!(state == "Sick" & sex == "women")])
  expect_error(read_toy_values(), "costs.csv: 1 missing rows: every state needs a row for every sex of the matrices. First: state = Sick, sex = women")
  local_toy_project(costs = data.table::copy(costs)[1, sex := "other"])
  expect_error(read_toy_values(), "costs.csv: `sex` values not in the transition matrices: other")
  local_toy_project(costs = rbind(costs, costs[1]))
  expect_error(read_toy_values(), "costs.csv: 1 duplicated rows \\(same state, sex\\). First: state = Well, sex = men")
  local_toy_project(costs = data.table::copy(costs)[, region := "north"])
  expect_error(read_toy_values(), "costs.csv: unexpected column\\(s\\): region. Stratifier columns must be listed in `strata`")

  utilities <- data.table::data.table(state = rep(c("Well", "Sick"), each = 4), agegrp = rep(c("65-79", "80-109"), 4),
                                      sex = rep(c("men", "women"), each = 2), usual_care = 0.5, iabc = 0.5)
  local_toy_project(utilities = utilities[!(state == "Well" & agegrp == "80-109" & sex == "women")])
  expect_error(read_toy_values(), "utilities.csv: 1 missing rows: every state needs a row for every agegrp, sex of the matrices. First: state = Well, agegrp = 80-109, sex = women")
})

test_that("the intervention cost is read, one row per year after entry", {
  local_toy_project(intervention_cost = data.table::data.table(year = c(2, 1, 3, 4), cost = c(0, 50L, 0, 0)))
  ic <- tm_read_intervention_cost(tm_read_settings())
  expect_equal(ic$year, 1:4)
  expect_equal(ic$cost, c(50, 0, 0, 0))
  expect_type(ic$cost, "double")
})

test_that("broken intervention-cost files are rejected", {
  read_ic <- function() tm_read_intervention_cost(tm_read_settings())
  ic <- data.table::data.table(year = 1:4, cost = c(50, 0, 0, 0))
  local_toy_project(intervention_cost = ic[year != 3])
  expect_error(read_ic(), "intervention_cost.csv: the `year` column must list every year from 1 to 4 \\(horizon - 1\\); missing: 3")
  local_toy_project(intervention_cost = rbind(ic, ic[1]))
  expect_error(read_ic(), "intervention_cost.csv: duplicated years: 1")
  local_toy_project(intervention_cost = data.table::copy(ic)[1, cost := -5])
  expect_error(read_ic(), "intervention_cost.csv: `cost` values must be 0 or more")
  local_toy_project(intervention_cost = data.table::copy(ic)[2, cost := NA])
  expect_error(read_ic(), "intervention_cost.csv: `cost` must be numbers with no missing values")
  local_toy_project(intervention_cost = ic[, .(year)])
  expect_error(read_ic(), "intervention_cost.csv: missing column\\(s\\): cost")
  local_toy_project(intervention_cost = data.table::copy(ic)[, arm := "iabc"])
  expect_error(read_ic(), "intervention_cost.csv: unexpected column\\(s\\): arm")
  local_toy_project()
  file.remove("inputs/intervention_cost.csv")
  expect_error(read_ic(), "inputs/intervention_cost.csv not found")
})

test_that("the intervention cost is charged to iABC patients alive in the years after entry only", {
  # Patients aged 70 stay Well for the whole horizon. Patient 1 enters in cycle 0, patient 2 in cycle 1.
  stay_well <- toy_matrix(p_sick = 0, p_dead_well_by_band = c("65-79" = 0, "80-109" = 0.05))
  local_toy_project(usual_care = stay_well, iabc = stay_well, cohort = toy_cohort(n = 2, entry_cycle = c(0, 1)),
                    intervention_cost = toy_intervention_cost(c(50, 20)), settings = toy_settings(horizon = 4))
  out <- toy_outcomes(toy_run())
  # Not in the entry year; then year 1, year 2, and nothing in year 3.
  expect_equal(out[arm == "iabc" & id == 1, intervention_cost], c(0, 50, 20, 0))
  expect_equal(out[arm == "iabc" & id == 1, cost], 100 + c(0, 50, 20, 0))
  # The late entrant's years after entry are their own: cycle 2 is their year 1.
  expect_equal(out[arm == "iabc" & id == 2, intervention_cost], c(0, 50, 20))
  # Usual care never pays it.
  expect_true(all(out[arm == "usual_care", intervention_cost] == 0))
  expect_equal(out[arm == "usual_care" & id == 1, cost], c(100, 100, 100, 100))

  # Not in the year of death: everyone enters Well and dies in the next cycle (their year 1).
  certain_death <- toy_matrix(p_sick = 0, p_dead_well = 1)
  local_toy_project(usual_care = certain_death, iabc = certain_death, cohort = toy_cohort(n = 1),
                    intervention_cost = toy_intervention_cost(50))
  out <- toy_outcomes(toy_run())[arm == "iabc"]
  expect_equal(out$state, c("Well", "Dead"))
  expect_equal(out$intervention_cost, c(0, 0))
  expect_equal(out$cost, c(100, 500))
})

test_that("a missing discount rate is reported", {
  local_toy_project(settings = toy_settings(discount_rate = NULL))
  expect_error(toy_outcomes(toy_run()), "needs `discount_rate`")
})
