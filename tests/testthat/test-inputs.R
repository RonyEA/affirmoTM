# Reading inputs/: file lookup, settings, the two matrices and age groups.

test_that("a missing inputs/ folder or file gives a clear error", {
  withr::local_dir(withr::local_tempdir())
  expect_error(tm_read_settings(), "No inputs/ folder in the working directory")
  dir.create("inputs")
  expect_error(tm_read_settings(), "inputs/settings.yaml not found")
})

test_that("settings need a strata entry, which may be empty", {
  local_toy_project(settings = toy_settings(strata = NULL))
  expect_error(tm_read_settings(), "needs a `strata` entry")
  yaml::write_yaml(toy_settings(strata = character(0)), "inputs/settings.yaml")
  expect_identical(tm_read_settings()$strata, character(0))
})

test_that("every setting is checked, with a message naming it", {
  read_with <- function(...) {
    local_toy_project(settings = toy_settings(...))
    tm_read_settings()
  }
  expect_silent(read_with())
  expect_error(read_with(horizon = "5"), "inputs/settings.yaml needs `horizon`: a whole number of cycles, 1 or more")
  expect_error(read_with(n_runs = "2"), "inputs/settings.yaml needs `n_runs`: a whole number, 1 or more")
  expect_error(read_with(seed = "abc"), "inputs/settings.yaml needs `seed`: a whole number")
  expect_error(read_with(seed = 1.5), "needs `seed`: a whole number")
  expect_error(read_with(seed = 3e9), "needs `seed`: a whole number \\(at most 2147483645 with 2 runs\\)")
  expect_error(read_with(discount_rate = "3%"), "needs `discount_rate`: a number, 0 or more")
  expect_error(read_with(currency = 123), "inputs/settings.yaml: `currency` must be text")
  expect_error(read_with(age_limit_death_state = FALSE), "needs `age_limit_death_state`.*Quote names that YAML reads")
  expect_error(read_with(events = list(sick = FALSE)), "event `sick` must list one or more state names")
  expect_error(read_with(wtp = list("20k")), "`wtp` must be a list of positive numbers")
})

test_that("unknown setting names give a warning, suggesting the closest known name", {
  s <- toy_settings()
  s$event <- s$events
  s$events <- NULL
  local_toy_project(settings = s)
  expect_warning(tm_read_settings(), "`event` is not a known setting and is ignored; did you mean `events`\\?")
  local_toy_project(settings = toy_settings(colour = "blue"))
  expect_warning(tm_read_settings(), "`colour` is not a known setting and is ignored\\.")
})

test_that("a bad setting stops tm_run() before anything is simulated or written", {
  local_toy_project(settings = toy_settings(wtp = list("20k")))
  expect_error(tm_run(), "`wtp` must be a list of positive numbers")
  expect_false(dir.exists("outputs"))

  # Settings changed in R are checked too.
  local_toy_project()
  settings <- tm_read_settings()
  settings$horizon <- "5"
  expect_error(tm_run(settings), "needs `horizon`")
  expect_false(dir.exists("outputs"))
})

test_that("valid matrices are read, with states, strata and death states", {
  local_toy_project()
  m <- tm_read_matrices(tm_read_settings())
  expect_named(m, c("usual_care", "iabc"))
  expect_equal(m$usual_care$states, c("Well", "Sick", "Dead"))
  expect_equal(m$usual_care$death, "Dead")
  expect_equal(m$usual_care$strata, c("agegrp", "sex"))
})

test_that("the two matrices must match", {
  renamed <- toy_matrix()[from == "Sick", from := "Ill"]
  data.table::setnames(renamed, "Sick", "Ill")
  local_toy_project(iabc = renamed)
  expect_error(tm_read_matrices(tm_read_settings()), "different states. Only in usual care: Sick. Only in iABC: Ill")

  local_toy_project(iabc = toy_matrix()[sex == "men"])
  expect_error(tm_read_matrices(tm_read_settings()), "different strata .*Only in usual care: .*women")
})

test_that("a matrix with a year column must list every year after entry within the horizon", {
  by_year <- function(years) data.table::rbindlist(lapply(years, function(y) toy_matrix()[, year := y]))
  local_toy_project(iabc = by_year(1:4), settings = toy_settings(horizon = 5))
  m <- tm_read_matrices(tm_read_settings())
  expect_equal(m$iabc$years, 1:4)
  expect_null(m$usual_care$years)

  local_toy_project(iabc = by_year(c(1, 2, 4)), settings = toy_settings(horizon = 5))
  expect_error(tm_read_matrices(tm_read_settings()),
               "transition_matrix_iabc.csv: the `year` column must list every year from 1 to 4 \\(horizon - 1\\); missing: 3")

  # Years beyond the horizon are allowed (and not used).
  local_toy_project(iabc = by_year(1:6), settings = toy_settings(horizon = 5))
  expect_equal(tm_read_matrices(tm_read_settings())$iabc$years, 1:6)
})

test_that("matrices in inputs/ hold point values only", {
  local_toy_project(iabc = toy_matrix()[, sim := 1L])
  expect_error(tm_read_matrices(tm_read_settings()), "has a `sim` column")
})

test_that("each matrix is checked, and errors name the file", {
  bad <- toy_matrix()
  bad[1, Well := Well - 0.1]
  local_toy_project(usual_care = bad)
  expect_error(tm_read_matrices(tm_read_settings()), "transition_matrix_usual_care.csv: Rows do not sum to 1")
})

test_that("a state whose rows are missing is reported, not taken for death", {
  local_toy_project(usual_care = toy_matrix()[from != "Sick"])
  expect_error(tm_read_matrices(tm_read_settings()),
               "transition_matrix_usual_care.csv: State\\(s\\) with a column but no rows: Sick")
})

test_that("a stratifier missing from settings is reported", {
  local_toy_project(settings = toy_settings(strata = "agegrp"))
  expect_error(tm_read_matrices(tm_read_settings()), "not numeric: sex.*add them to `strata`")
})

test_that("age groups must read as ranges without gaps or overlaps", {
  expect_equal(age_bands(c("70-74", "65-69"))$agegrp, c("65-69", "70-74"))
  expect_error(age_bands(c("65-69", "70_74")), "written as ranges such as '65-69': 70_74")
  expect_error(age_bands(c("65-69", "75-79")), "65-69 is followed by 75-79")
  expect_error(age_bands(c("65-69", "68-74")), "65-69 is followed by 68-74")
  expect_error(age_bands("69-65"), "upper age below the lower age")
})
