# tm_read_entry_states() and tm_assign_entry_states().

read_toy_entry <- function() {
  settings <- tm_read_settings()
  tm_read_entry_states(settings, tm_read_matrices(settings))
}

test_that("a valid entry-state table is read", {
  local_toy_project()
  expect_equal(nrow(read_toy_entry()), 4)
})

test_that("broken entry-state tables are rejected", {
  local_toy_project(entry_states = toy_entry_states()[, Other := 0])
  expect_error(read_toy_entry(), "states not in the transition matrices: Other")

  bad <- toy_entry_states()
  bad[1, Well := 0.9]
  local_toy_project(entry_states = bad)
  expect_error(read_toy_entry(), "entry_states.csv: Rows do not sum to 1")

  local_toy_project(entry_states = toy_entry_states()[, -"sex"])
  expect_error(read_toy_entry(), "missing stratifier column\\(s\\): sex")
})

test_that("entry states follow the table and are reproducible", {
  local_toy_project(cohort = toy_cohort(n = 5000), entry_states = toy_entry_states(well = 0.7))
  settings <- tm_read_settings()
  matrices <- tm_read_matrices(settings)
  cohort <- tm_read_cohort(settings, matrices)
  entry <- tm_read_entry_states(settings, matrices)
  a <- tm_assign_entry_states(cohort, entry, settings, seed = 1)
  expect_lt(abs(mean(a$entry_state == "Well") - 0.7), 0.03)
  expect_setequal(unique(a$entry_state), c("Well", "Sick"))
  expect_identical(a$entry_state, tm_assign_entry_states(cohort, entry, settings, seed = 1)$entry_state)
  expect_false(identical(a$entry_state, tm_assign_entry_states(cohort, entry, settings, seed = 2)$entry_state))
})

test_that("a patient whose stratum has no entry row is reported", {
  local_toy_project(cohort = toy_cohort(n = 2, sex = c("men", "women")),
                    entry_states = toy_entry_states()[sex == "men"])
  settings <- tm_read_settings()
  matrices <- tm_read_matrices(settings)
  expect_error(tm_assign_entry_states(tm_read_cohort(settings, matrices), tm_read_entry_states(settings, matrices),
                                      settings),
               "no row for the entry stratum of 1 patients \\(ids 2\\)")
})
