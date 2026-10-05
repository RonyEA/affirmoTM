# tm_example_inputs(): copies the example files into an analysis folder without overwriting by accident.

test_that("the example files are copied into inputs/", {
  dir <- withr::local_tempdir()
  copied <- suppressMessages(tm_example_inputs(dir))
  expect_setequal(basename(copied), list.files(system.file("extdata", "example_inputs", package = "affirmoTM")))
  expect_true(all(file.exists(file.path(dir, "inputs", basename(copied)))))
})

test_that("existing files are not overwritten unless asked", {
  dir <- withr::local_tempdir()
  suppressMessages(tm_example_inputs(dir))
  writeLines("my own settings", file.path(dir, "inputs", "settings.yaml"))
  expect_error(tm_example_inputs(dir), "already exist .*settings.yaml.*overwrite = TRUE")
  expect_identical(readLines(file.path(dir, "inputs", "settings.yaml")), "my own settings")
  suppressMessages(tm_example_inputs(dir, overwrite = TRUE))
  expect_false(identical(readLines(file.path(dir, "inputs", "settings.yaml")), "my own settings"))
})

test_that("the example inputs run end to end", {
  dir <- withr::local_tempdir()
  suppressMessages(tm_example_inputs(dir))
  withr::local_dir(dir)
  settings <- tm_read_settings()
  settings$n_runs <- 2
  results <- suppressMessages(tm_run(settings))
  expect_s3_class(results, "tm_results")
  expect_equal(nrow(results$by_run), 4)
  expect_true(all(results$by_run$patients == 5000))
  expect_true(all(file.exists(file.path("outputs", c("summary.csv", "summary_by_run.csv", "comparison.csv",
                                                      "comparison_by_run.csv", "ce_plane.png")))))
})
