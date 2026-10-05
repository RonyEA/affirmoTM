# tm_matrix() (a user matrix) and tm_matrix_from_model() (the model example).

toy <- data.frame(
  from = c("Well", "Sick", "Dead"),
  Well = c(0.90, 0.20, 0),
  Sick = c(0.08, 0.70, 0),
  Dead = c(0.02, 0.10, 1)
)

# ---- User path --------------------------------------------------------------

test_that("a valid user matrix is accepted and its death state detected", {
  m <- tm_matrix(toy)
  expect_s3_class(m, "tm_matrix")
  expect_equal(m$states, c("Well", "Sick", "Dead"))
  expect_equal(m$death, "Dead")
  expect_length(m$strata, 0)
  expect_equal(m$n_sim, 0L)
  expect_output(print(m), "none \\(one matrix for everyone\\)")
})

test_that("every state needs rows, and death states are those whose rows always go back to themselves", {
  expect_error(tm_matrix(toy[1:2, ]), "column but no rows: Dead. Every state needs its own rows")

  strat <- data.table::rbindlist(list(cbind(sex = "men", toy), cbind(sex = "women", toy)))
  expect_equal(tm_matrix(strat)$death, "Dead")
  # Sick stays put with probability 1 for women only: it can be left, so it is a living state.
  strat[sex == "women" & from == "Sick", `:=`(Well = 0, Sick = 1, Dead = 0)]
  expect_equal(tm_matrix(strat)$death, "Dead")
  # Staying put with probability 1 in every stratum makes it a death state.
  strat[from == "Sick", `:=`(Well = 0, Sick = 1, Dead = 0)]
  expect_equal(tm_matrix(strat)$death, c("Sick", "Dead"))
})

test_that("a stratified matrix is read from a data.frame or a CSV file", {
  strat <- rbind(cbind(sex = "men", toy), cbind(sex = "women", toy))
  m <- tm_matrix(strat)
  expect_equal(m$strata, "sex")
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(strat, path)
  expect_equal(tm_matrix(path)$probs, m$probs)
})

test_that("an explicit strata argument names stratifiers outside the defaults", {
  strat <- cbind(region = rep(c("north", "south"), each = 3), rbind(toy, toy))
  expect_error(tm_matrix(strat), "not numeric: region")
  expect_equal(tm_matrix(strat, strata = "region")$strata, "region")
})

test_that("a year column (years since entry) is part of the row key, not a state", {
  by_year <- rbind(cbind(year = 1L, toy), cbind(year = 2L, toy))
  m <- tm_matrix(by_year)
  expect_equal(m$states, c("Well", "Sick", "Dead"))
  expect_equal(m$years, 1:2)
  expect_equal(m$death, "Dead")
  expect_output(print(m), "by year since entry, 1 to 2 \\(year column\\)")
  expect_null(tm_matrix(toy)$years)
  expect_output(print(tm_matrix(toy)), "the same rows every year")

  expect_error(tm_matrix(rbind(cbind(year = 0L, toy), cbind(year = 1L, toy))), "`year` must hold whole numbers, 1 or more")
  expect_error(tm_matrix(cbind(year = 1.5, toy)), "`year` must hold whole numbers, 1 or more")
  expect_error(tm_matrix(rbind(cbind(year = 1L, toy), cbind(year = 1L, toy))), "Duplicated rows \\(same year, from\\)")
  # Year 2 has the men's rows only.
  strat <- rbind(cbind(year = 1L, sex = "men", toy), cbind(year = 1L, sex = "women", toy),
                 cbind(year = 2L, sex = "men", toy))
  expect_error(tm_matrix(strat), "Every year needs a row for every stratum \\(2 strata\\); year\\(s\\) 2 have fewer")
  expect_error(tm_matrix(cbind(year = 1L, sex = "men", toy), strata = c("sex", "year")), "not a stratifier")
})

test_that("broken matrices are rejected with clear errors", {
  bad <- toy
  bad$Dead[1] <- 0.03
  expect_error(tm_matrix(bad), "Rows do not sum to 1 \\(tolerance 1e-8\\): row 1 sums to 1.01")

  bad <- toy
  bad$Well[2] <- 1.0
  bad$Sick[2] <- -0.1
  expect_error(tm_matrix(bad), "outside \\[0, 1\\] in rows 2")

  bad <- toy
  bad$Well[1] <- NA
  expect_error(tm_matrix(bad), "Missing probabilities in rows 1")

  expect_error(tm_matrix(rbind(toy, toy[1, ])), "Duplicated rows \\(same from\\): rows 4")

  strat <- rbind(cbind(sex = "men", toy), cbind(sex = "women", toy[1, ]))
  expect_error(tm_matrix(strat), "1 strata do not have a row for every state \\(3 expected\\). First: sex = women")

  bad <- rbind(toy, data.frame(from = "Gone", Well = 1, Sick = 0, Dead = 0))
  expect_error(tm_matrix(bad), "row but no column: Gone")

  expect_error(tm_matrix(toy[, c("Well", "Sick", "Dead")]), "needs a `from` column")
  expect_error(tm_matrix("no-such-file.csv"), "file not found")
})

# ---- Model path -------------------------------------------------------------

model <- tm_default_model()
point <- tm_matrix_from_model(model)
draws <- tm_matrix_from_model(model, n_sim = 200, seed = 42)
allowed <- data.table::fread(system.file("extdata", "allowed_transitions.csv", package = "affirmoTM"))

stratum <- function(m, from_state, sex_ = "men", agegrp_ = "65-69", cluster = "Unspecified") {
  m$probs[from == from_state & sex == sex_ & agegrp == agegrp_ & mm_cluster == cluster]
}

# Largest probability given to a move the allowed-transitions table rules out (living states only).
impossible_mass <- function(m) {
  living <- m$probs[from %in% allowed$from]
  mask <- as.matrix(allowed[match(living$from, allowed$from), m$states, with = FALSE])
  probs <- as.matrix(living[, m$states, with = FALSE])
  max(probs[mask == 0])
}

test_that("the point estimate is a valid matrix with the expected structure", {
  expect_equal(point$n_sim, 0L)
  expect_equal(nrow(point$probs), 126 * 19)
  expect_equal(point$strata, c("agegrp", "sex", "mm_cluster"))
  expect_equal(length(point$states), 19)
  expect_equal(point$death, c("CurrentDeath", "AB_Death", "AHS_Death", "AIS_Death", "AHS_AB_Death", "AIS_AB_Death"))
  expect_true(all(abs(rowSums(point$probs[, point$states, with = FALSE]) - 1) < 1e-8))
})

test_that("the model's death states get rows that always go back to themselves", {
  dead <- point$probs[from %in% point$death]
  expect_equal(nrow(dead), 126 * 6)
  probs <- as.matrix(dead[, point$states, with = FALSE])
  expect_identical(probs[cbind(seq_len(nrow(dead)), match(dead$from, point$states))], rep(1, nrow(dead)))
  expect_identical(sum(probs), as.numeric(nrow(dead)))
  expect_equal(nrow(draws$probs[from %in% draws$death]), 200 * 126 * 6)
})

test_that("the point estimate reproduces the reference values (men, 65-69, Unspecified)", {
  ref <- stratum(point, "NoEvent")
  expect_lt(abs(ref$NoEvent - 0.9753), 5e-5)
  expect_lt(abs(ref$AB - 0.0035), 5e-5)
  expect_lt(abs(ref$AHS - 0.0019), 5e-5)
  expect_lt(abs(ref$AIS - 0.0053), 5e-5)
  expect_lt(abs(ref$CurrentDeath - 0.0131), 5e-5)
  # PIS -> PIS is 0.9503 before the Figure 1 mask; removing PIS -> AHS (0.0076) and rescaling gives 0.9575.
  expect_lt(abs(stratum(point, "PIS")$PIS - 0.9575), 5e-5)
})

test_that("moves marked impossible get exactly zero probability", {
  expect_identical(impossible_mass(point), 0)
  expect_identical(impossible_mass(draws), 0)
  unmasked <- tm_matrix_from_model(model, allowed = NULL)
  expect_gt(max(unmasked$probs[from == "NoEvent"]$PIS), 0)
  expect_lt(max(unmasked$probs[from == "NoEvent"]$PIS), 1e-6)
})

test_that("draws are complete, valid and numbered", {
  expect_equal(draws$n_sim, 200L)
  expect_equal(nrow(draws$probs), 200 * 126 * 19)
  probs <- as.matrix(draws$probs[, draws$states, with = FALSE])
  expect_false(anyNA(probs))
  expect_true(all(is.finite(probs)))
  expect_true(all(abs(rowSums(probs) - 1) < 1e-8))
})

test_that("draws are plausible in well-populated strata", {
  ref <- stratum(draws, "NoEvent")
  expect_gt(quantile(ref$NoEvent, 0.025), 0.97)
  p0 <- stratum(point, "NoEvent")
  for (to in c("NoEvent", "AB", "AHS", "AIS", "CurrentDeath")) {
    expect_lt(abs(mean(ref[[to]]) / p0[[to]] - 1), 0.05, label = paste("NoEvent ->", to))
  }
  ais <- stratum(draws, "AIS", "women", "80-84", "Cardiovascular")
  p0 <- stratum(point, "AIS", "women", "80-84", "Cardiovascular")
  for (to in c("PIS", "CurrentDeath", "AIS_Death")) {
    expect_lt(abs(mean(ais[[to]]) / p0[[to]] - 1), 0.05, label = paste("AIS ->", to))
  }
})

test_that("the same seed gives identical draws and a different seed does not", {
  a <- tm_matrix_from_model(model, n_sim = 3, seed = 7)
  b <- tm_matrix_from_model(model, n_sim = 3, seed = 7)
  c <- tm_matrix_from_model(model, n_sim = 3, seed = 8)
  expect_identical(a$probs, b$probs)
  expect_false(isTRUE(all.equal(a$probs, c$probs)))
})

test_that("the vcov repair makes the matrix exactly symmetric and positive semi-definite", {
  r <- repair_vcov(model$vcov)
  expect_gt(r$n_negative_eigen, 0)
  expect_lt(r$max_asymmetry, 1e-6)
  expect_true(isSymmetric(r$vcov, tol = 0))
  expect_gt(min(eigen(r$vcov, symmetric = TRUE, only.values = TRUE)$values), -1e-10)
  expect_equal(draws$model_info$vcov_repair$n_negative_eigen, r$n_negative_eigen)
})

test_that("both paths agree: the point estimate survives a CSV round trip", {
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(point$probs, path)
  back <- tm_matrix(path)
  expect_equal(back$states, point$states)
  expect_equal(back$death, point$death)
  expect_equal(back$strata, point$strata)
  expect_equal(back$probs, point$probs, tolerance = 1e-12)
})

test_that("a malformed allowed-transitions table is rejected", {
  expect_error(tm_matrix_from_model(model, allowed = allowed[, -"PIS"]), "columns must match")
  expect_error(tm_matrix_from_model(model, allowed = allowed[-1]), "one row per current state; missing: NoEvent")
  expect_error(tm_matrix_from_model(model, allowed = "missing.csv"), "file not found")
  expect_error(tm_matrix_from_model(model, n_sim = -1), "whole number")
})
