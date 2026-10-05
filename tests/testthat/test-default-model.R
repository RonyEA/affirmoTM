# tm_default_model(): the default model files load, have the expected shape, and agree with each other.

inputs <- tm_default_model()

test_that("inputs have the expected dimensions", {
  expect_equal(dim(inputs$coef), c(18, 28))
  expect_equal(dim(inputs$vcov), c(504, 504))
  expect_equal(dim(inputs$design), c(1638, 28))
  expect_equal(nrow(inputs$initial), 98)
  expect_equal(ncol(inputs$initial), 3 + 15)
})

test_that("coefficients, vcov and design matrix line up by name", {
  expect_identical(colnames(inputs$design), colnames(inputs$coef))
  expect_identical(colnames(inputs$vcov)[1:2], c("AB:(Intercept)", "AB:CurrentEventAB"))
  expect_true(isSymmetric(unname(inputs$vcov), tol = 1e-8))
})

test_that("design rows decode into a complete grid of unique strata", {
  s <- inputs$strata
  expect_equal(nrow(s), 1638)
  expect_equal(nrow(unique(s)), 1638)
  expect_equal(length(unique(s$from)), 13)
  expect_setequal(unique(s$sex), c("men", "women"))
  expect_equal(length(unique(s$agegrp)), 9)
  expect_equal(length(unique(s$mm_cluster)), 7)
  expect_equal(nrow(s), 13 * 2 * 9 * 7)
})

test_that("design decoding matches the dummy columns", {
  # Row 1 has the intercept, sexM and age_group70-74 set, nothing else.
  expect_equal(unname(inputs$design[1, c("sexM", "age_group70-74")]), c(1, 1))
  expect_equal(
    as.list(inputs$strata[1]),
    list(from = "NoEvent", sex = "men", agegrp = "70-74", mm_cluster = "Unspecified")
  )
  # Every decoded row maps back to its dummies.
  s <- inputs$strata
  d <- inputs$design
  expect_true(all(d[s$from != "NoEvent", ][cbind(seq_len(sum(s$from != "NoEvent")),
    match(paste0("CurrentEvent", s$from[s$from != "NoEvent"]), colnames(d)))] == 1))
  expect_true(all(d[, "sexM"] == as.numeric(s$sex == "men")))
})

test_that("entry-state matrix is valid and uses the model's labels", {
  init <- inputs$initial
  states <- setdiff(names(init), c("mm_cluster", "sex", "agegrp"))
  probs <- as.matrix(init[, states, with = FALSE])
  expect_true(all(abs(rowSums(probs) - 1) < 1e-8))
  expect_true(all(probs >= 0 & probs <= 1))
  expect_equal(nrow(unique(init[, c("mm_cluster", "sex", "agegrp")])), 98)
  expect_setequal(unique(init$mm_cluster), unique(inputs$strata$mm_cluster))
  expect_setequal(unique(init$sex), unique(inputs$strata$sex))
  expect_true(all(init$agegrp %in% inputs$strata$agegrp))
  expect_setequal(states, c(unique(inputs$strata$from), "PIS_PHS", "DTH"))
})

test_that("a missing file gives a clear error", {
  tmp <- withr::local_tempdir()
  expect_error(tm_default_model(tmp), "Default input file\\(s\\) not found")
})

test_that("an inconsistent entry-state matrix is rejected", {
  tmp <- withr::local_tempdir()
  file.copy(file.path(inputs$path, c("Present_coef_model.rds", "Present_vcov_model.rds",
                                     "Present_design_matrix.rds")), tmp)
  bad <- data.table::copy(inputs$initial)
  data.table::set(bad, 1L, "NoEvent", bad$NoEvent[1] - 0.1)
  data.table::fwrite(bad, file.path(tmp, "Initial_Transition.csv"))
  expect_error(tm_default_model(tmp), "do not sum to 1: rows 1")
})

test_that("print method summarises the inputs", {
  expect_output(print(inputs), "1638 strata = 13 current states x 2 sexes x 9 age groups x 7 clusters")
})
