# tm_read_cohort(): reading, checking and completing the cohort.

read_toy_cohort <- function() {
  settings <- tm_read_settings()
  tm_read_cohort(settings, tm_read_matrices(settings))
}

test_that("a valid cohort is read and its age group worked out from age", {
  local_toy_project(cohort = toy_cohort(n = 3, age = c(65, 79, 80)))
  cohort <- read_toy_cohort()
  expect_equal(cohort$agegrp, c("65-79", "65-79", "80-109"))
  expect_equal(names(cohort)[1:3], c("id", "entry_cycle", "age"))
})

test_that("an agegrp column is accepted when it agrees with age, and rejected when it does not", {
  local_toy_project(cohort = toy_cohort(n = 2, age = c(70, 85))[, agegrp := c("65-79", "80-109")])
  expect_equal(read_toy_cohort()$agegrp, c("65-79", "80-109"))
  local_toy_project(cohort = toy_cohort(n = 2, age = c(70, 85))[, agegrp := "65-79"])
  expect_error(read_toy_cohort(), "`agegrp` does not match `age` for ids 2")
})

test_that("patients entering at or after the horizon are left out, with a warning", {
  local_toy_project(cohort = toy_cohort(n = 4, entry_cycle = c(0, 4, 5, 7)), settings = toy_settings(horizon = 5))
  expect_warning(cohort <- read_toy_cohort(),
                 "2 patients enter in cycle 5 or later, after the horizon of 5 cycles, and are left out \\(ids 3, 4\\)")
  expect_equal(cohort$id, 1:2)
})

test_that("every stratum a patient can reach within the horizon needs matrix rows", {
  # No rows for women aged 80-109. Patients entering at 77 are in that age group from cycle 3, and use it for the
  # move into cycle 4.
  gap <- toy_matrix()[!(agegrp == "80-109" & sex == "women")]
  cohort <- toy_cohort(n = 3, age = 77, sex = c("women", "women", "men"))
  local_toy_project(usual_care = gap, iabc = gap, cohort = cohort, settings = toy_settings(horizon = 6))
  expect_error(read_toy_cohort(), paste0("cohort.csv: the transition matrices have no rows for agegrp = 80-109, ",
                                         "sex = women, which 2 patients reach within the horizon \\(ids 1, 2\\)"))

  # With a 4-year horizon they never use the 80-109 rows.
  local_toy_project(usual_care = gap, iabc = gap, cohort = cohort, settings = toy_settings(horizon = 4))
  expect_equal(nrow(read_toy_cohort()), 3)

  # A patient entering in the last cycle never moves, so needs no rows.
  local_toy_project(usual_care = gap, iabc = gap, settings = toy_settings(horizon = 6),
                    cohort = toy_cohort(n = 1, age = 85, sex = "women", entry_cycle = 5))
  expect_equal(nrow(read_toy_cohort()), 1)
})

test_that("columns the model does not use may have blanks", {
  local_toy_project(cohort = toy_cohort(n = 3)[, notes := c("a", NA, "")])
  cohort <- read_toy_cohort()
  expect_equal(nrow(cohort), 3)
  expect_equal(cohort$notes, c("a", "", ""))  # blanks in a CSV text column are read as ""
})

test_that("broken cohorts are rejected with clear errors", {
  local_toy_project(cohort = toy_cohort()[, -"entry_cycle"])
  expect_error(read_toy_cohort(), "missing column\\(s\\): entry_cycle")

  local_toy_project(cohort = toy_cohort()[2, id := 1L])
  expect_error(read_toy_cohort(), "duplicated ids: 1")

  local_toy_project(cohort = toy_cohort(n = 2, age = c(70, 112)))
  expect_error(read_toy_cohort(), "ages outside the matrices' age groups \\(65-109\\) for ids 2")

  local_toy_project(cohort = toy_cohort(n = 2, age = c(70, 70.5)))
  expect_error(read_toy_cohort(), "`age` must be whole years")

  local_toy_project(cohort = toy_cohort(n = 2, entry_cycle = c(0, -1)))
  expect_error(read_toy_cohort(), "`entry_cycle` must be a whole number, 0 or more")

  local_toy_project(cohort = toy_cohort(n = 2, sex = c("men", "other")))
  expect_error(read_toy_cohort(), "`sex` values not in the transition matrices: other")

  local_toy_project(cohort = toy_cohort(n = 2, sex = c("men", NA)))
  expect_error(read_toy_cohort(), "missing values in `sex`, rows 2")
})
