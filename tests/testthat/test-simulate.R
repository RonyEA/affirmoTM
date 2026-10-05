# tm_simulate(): moving patients through both arms year by year.

test_that("patients who must die do so in the cycle after entry, with nothing recorded afterwards", {
  certain_death <- toy_matrix(p_sick = 0, p_dead_well = 1)
  local_toy_project(usual_care = certain_death, iabc = certain_death)
  sim <- toy_run()$sim
  expect_equal(nrow(sim), 10 * 2 * 2)
  expect_equal(sim[cycle == 0, unique(state)], "Well")
  expect_equal(sim[cycle == 1, unique(state)], "Dead")
})

# Nobody leaves Well at ages 65-79, but Well can be left at 80-109, so it is a living state (a state never
# left in any stratum would be a death state).
stay_young <- toy_matrix(p_sick = 0, p_dead_well_by_band = c("65-79" = 0, "80-109" = 0.05))

test_that("patients who never move are followed to the end of the horizon", {
  stay <- stay_young
  local_toy_project(usual_care = stay, iabc = stay, settings = toy_settings(horizon = 5))
  sim <- toy_run()$sim
  expect_equal(sim[, .N, by = .(id, arm)]$N, rep(5, 20))
  expect_true(all(sim$state == "Well"))
  expect_equal(sim[id == 1 & arm == "usual_care", age], 70:74)
})

test_that("late entrants start in their entry cycle and stop at the horizon", {
  stay <- stay_young
  local_toy_project(usual_care = stay, iabc = stay, cohort = toy_cohort(n = 2, entry_cycle = c(0, 3)),
                    settings = toy_settings(horizon = 5))
  sim <- toy_run()$sim
  expect_equal(sim[id == 2 & arm == "usual_care", cycle], 3:4)
  expect_equal(sim[id == 2 & arm == "usual_care", age], 70:71)
})

test_that("a move uses the age group of the previous cycle", {
  # Death is certain in the 80-109 rows only. A patient aged 78 at entry is 80 in cycle 2,
  # so the move into cycle 3 is the first to use the 80-109 rows.
  by_age <- toy_matrix(p_sick = 0, p_dead_well_by_band = c("65-79" = 0, "80-109" = 1))
  local_toy_project(usual_care = by_age, iabc = by_age, cohort = toy_cohort(n = 1, age = 78))
  sim <- toy_run()$sim[arm == "usual_care"]
  expect_equal(sim$agegrp, c("65-79", "65-79", "80-109", "80-109"))
  expect_equal(sim$state, c("Well", "Well", "Well", "Dead"))
})

test_that("a patient who would pass the oldest age group dies that year", {
  # Nobody leaves Well at ages 80-109 (Well can be left at 65-79, so it is a living state).
  stay <- toy_matrix(p_sick = 0, p_dead_well_by_band = c("65-79" = 0.05, "80-109" = 0))
  local_toy_project(usual_care = stay, iabc = stay, cohort = toy_cohort(n = 1, age = 108))
  sim <- toy_run()$sim[arm == "usual_care"]
  expect_equal(sim$age, 108:110)
  expect_equal(sim$state, c("Well", "Well", "Dead"))
})

test_that("identical matrices give identical arms, and different matrices give different arms", {
  local_toy_project(cohort = toy_cohort(n = 500))
  sim <- toy_run()$sim
  expect_identical(sim[arm == "usual_care", -"arm"], sim[arm == "iabc", -"arm"])

  local_toy_project(cohort = toy_cohort(n = 500), iabc = toy_matrix(p_dead_well = 0.01, p_dead_sick = 0.01))
  sim <- toy_run()$sim
  expect_lt(sim[arm == "iabc", sum(state == "Dead")], sim[arm == "usual_care", sum(state == "Dead")] / 2)
})

test_that("the arms stay paired when the iABC matrix lists its columns in another order", {
  reordered <- data.table::setcolorder(toy_matrix(), c("agegrp", "sex", "from", "Dead", "Sick", "Well"))
  local_toy_project(cohort = toy_cohort(n = 500), iabc = reordered)
  sim <- toy_run()$sim
  expect_identical(sim[arm == "usual_care", -"arm"], sim[arm == "iabc", -"arm"])
})

test_that("with a year column, the move into year k after entry uses the year k rows", {
  # Patients stay Well, except that the iABC year 2 rows send Well to Dead. Patient 1 enters in cycle 0 and dies in
  # cycle 2; patient 2 enters in cycle 1 and dies in cycle 3.
  die <- toy_matrix(p_sick = 0, p_dead_well = 1)
  by_year <- data.table::rbindlist(list(data.table::copy(stay_young)[, year := 1L], data.table::copy(die)[, year := 2L],
                                        data.table::copy(stay_young)[, year := 3L],
                                        data.table::copy(stay_young)[, year := 4L]))
  local_toy_project(usual_care = stay_young, iabc = by_year, cohort = toy_cohort(n = 2, entry_cycle = c(0, 1)),
                    settings = toy_settings(horizon = 5))
  sim <- toy_run()$sim
  expect_equal(sim[arm == "iabc" & id == 1, state], c("Well", "Well", "Dead"))
  expect_equal(sim[arm == "iabc" & id == 2, cycle], 1:3)
  expect_equal(sim[arm == "iabc" & id == 2, state], c("Well", "Well", "Dead"))
  expect_true(all(sim[arm == "usual_care", state] == "Well"))
})

test_that("a year column with the same rows in every year gives exactly the same simulation", {
  m <- toy_matrix(p_sick = 0.2)
  by_year <- data.table::rbindlist(lapply(1:4, function(y) data.table::copy(m)[, year := y]))
  cohort <- toy_cohort(n = 300, entry_cycle = rep(0:2, 100))
  local_toy_project(usual_care = m, iabc = m, cohort = cohort)
  plain <- toy_run()$sim
  local_toy_project(usual_care = by_year, iabc = by_year, cohort = cohort)
  expect_identical(toy_run()$sim, plain)
})

test_that("each simulated year carries the patient's stratifiers", {
  local_toy_project(cohort = toy_cohort(n = 2, age = c(70, 85), sex = c("men", "women")))
  sim <- toy_run()$sim
  expect_equal(names(sim), c("id", "arm", "cycle", "age", "agegrp", "sex", "state"))
  expect_true(all(sim[id == 1, sex] == "men"))
  expect_true(all(sim[id == 2, sex] == "women"))
})

test_that("transition shares follow the matrix", {
  local_toy_project(cohort = toy_cohort(n = 5000), usual_care = toy_matrix(p_sick = 0.3, p_dead_well = 0.1))
  sim <- toy_run()$sim[arm == "usual_care" & cycle == 1]
  expect_lt(abs(mean(sim$state == "Sick") - 0.3), 0.03)
  expect_lt(abs(mean(sim$state == "Dead") - 0.1), 0.02)
})

test_that("the same seed gives the same simulation", {
  local_toy_project(cohort = toy_cohort(n = 200))
  a <- toy_run(seed = 5)$sim
  b <- toy_run(seed = 5)$sim
  c <- toy_run(seed = 6)$sim
  expect_identical(a, b)
  expect_false(isTRUE(all.equal(a, c)))
})

test_that("a run leaves the user's own random numbers alone", {
  local_toy_project(cohort = toy_cohort(n = 50))
  set.seed(42)
  expected <- runif(1)
  set.seed(42)
  toy_run(seed = 7)
  expect_identical(runif(1), expected)
})

test_that("missing settings and entry states are reported", {
  local_toy_project(settings = toy_settings(horizon = 0))
  expect_error(toy_run(), "needs `horizon`")
  local_toy_project(settings = toy_settings(age_limit_death_state = "Gone"))
  expect_error(toy_run(), "needs `age_limit_death_state`, one of the death states: Dead")
  local_toy_project()
  settings <- tm_read_settings()
  matrices <- tm_read_matrices(settings)
  expect_error(tm_simulate(tm_read_cohort(settings, matrices), matrices, settings), "no `entry_state` column")
})
