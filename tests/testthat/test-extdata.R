# The default model files and example inputs are found via system.file().

test_that("default input files are installed with the package", {
  files <- c(
    "Present_coef_model.rds",
    "Present_vcov_model.rds",
    "Present_design_matrix.rds",
    "Initial_Transition.csv"
  )
  paths <- vapply(files, function(f) system.file("extdata", f, package = "affirmoTM"), character(1))

  expect_true(all(nzchar(paths)), info = paste("missing:", paste(files[!nzchar(paths)], collapse = ", ")))
  expect_true(all(file.size(paths) > 0))
})

test_that("the example inputs are installed with the package", {
  files <- c("transition_matrix_usual_care.csv", "transition_matrix_iabc.csv", "cohort.csv", "entry_states.csv",
             "costs.csv", "utilities.csv", "intervention_cost.csv", "settings.yaml", "README.md")
  paths <- system.file("extdata", "example_inputs", files, package = "affirmoTM")
  expect_length(paths, length(files))
  expect_true(all(file.size(paths) > 0))
})
