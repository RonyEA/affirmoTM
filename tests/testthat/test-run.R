# tm_run(), tm_summarise(), tm_compare() and the cost-effectiveness plane, end to end on the toy project.

# Nobody leaves Well at ages 65-79 (Well can be left at 80-109, so it is a living state): patients aged 70 stay
# Well for the whole 3-year horizon.
stay_well <- toy_matrix(p_sick = 0, p_dead_well_by_band = c("65-79" = 0, "80-109" = 0.05))

test_that("a deterministic toy run gives the hand-calculated results", {
  # Three Well years per patient. The entry year takes the usual-care values in both arms; iABC's own values
  # apply in the two years after entry.
  costs <- data.table::data.table(state = c("Well", "Sick", "Dead"), usual_care = c(100, 1000, 500),
                                  iabc = c(200, 1000, 500))
  utilities <- data.table::data.table(state = c("Well", "Sick"), usual_care = c(0.9, 0.5), iabc = c(1, 0.5))
  # The intervention cost adds 30 to iABC in the first year after entry.
  local_toy_project(usual_care = stay_well, iabc = stay_well, costs = costs, utilities = utilities,
                    intervention_cost = toy_intervention_cost(30), settings = toy_settings(n_runs = 3, horizon = 3))
  results <- suppressMessages(tm_run())

  uc <- results$summary[arm == "usual_care"]
  iabc <- results$summary[arm == "iabc"]
  expect_equal(uc$patients, 10)
  expect_equal(uc$cost_per_patient, 300)
  expect_equal(uc$qaly_per_patient, 2.7)
  expect_equal(uc$intervention_cost_per_patient, 0)
  expect_equal(iabc$cost_per_patient, 100 + (200 + 30) + 200)
  expect_equal(iabc$intervention_cost_per_patient, 30)
  expect_equal(iabc$intervention_cost_total, 300)
  expect_equal(iabc$qaly_per_patient, 0.9 + 1 + 1)
  expect_equal(uc$deaths, 0)
  expect_equal(uc$sick, 0)

  expect_equal(results$comparison$delta_cost, 230)
  expect_equal(results$comparison$delta_intervention_cost, 30)
  expect_equal(results$comparison$delta_qaly, 0.2)
  expect_equal(results$comparison$icer, 1150)
  expect_output(print(results), "Incremental cost: +\u20ac230 \\(intervention cost: \u20ac30\\)")
  expect_equal(results$comparison$quadrant, "costlier, more QALYs")
  expect_equal(nrow(results$comparison_by_run), 3)
})

test_that("year columns with the same values in every year give exactly the same results", {
  costs <- data.table::data.table(state = c("Well", "Sick", "Dead"), usual_care = c(100, 1000, 500),
                                  iabc = c(150, 1000, 500))
  utilities <- data.table::data.table(state = c("Well", "Sick"), usual_care = c(0.9, 0.5), iabc = c(0.9, 0.5))
  by_year <- function(x) data.table::rbindlist(lapply(1:4, function(y) data.table::copy(x)[, year := y]))
  iabc <- toy_matrix(p_sick = 0.05)
  cohort <- toy_cohort(n = 300, entry_cycle = rep(0:2, 100))

  local_toy_project(iabc = iabc, cohort = cohort, costs = costs, utilities = utilities)
  plain <- suppressMessages(tm_run())
  local_toy_project(iabc = by_year(iabc), cohort = cohort, costs = by_year(costs), utilities = by_year(utilities))
  expanded <- suppressMessages(tm_run())
  expect_equal(expanded$by_run, plain$by_run)
  expect_equal(expanded$comparison, plain$comparison)
})

test_that("stratifier columns with the same values in every stratum give exactly the same results", {
  costs <- data.table::data.table(state = c("Well", "Sick", "Dead"), usual_care = c(100, 1000, 500),
                                  iabc = c(150, 1000, 500))
  utilities <- data.table::data.table(state = c("Well", "Sick"), usual_care = c(0.9, 0.5), iabc = c(0.9, 0.5))
  by_stratum <- function(x) data.table::rbindlist(lapply(c("men", "women"), function(s)
    data.table::rbindlist(lapply(toy_bands, function(a) data.table::copy(x)[, `:=`(agegrp = a, sex = s)]))))
  iabc <- toy_matrix(p_sick = 0.05)
  cohort <- toy_cohort(n = 300, age = rep(c(70, 79, 90), 100), sex = rep(c("men", "women"), 150))

  local_toy_project(iabc = iabc, cohort = cohort, costs = costs, utilities = utilities)
  plain <- suppressMessages(tm_run())
  local_toy_project(iabc = iabc, cohort = cohort, costs = by_stratum(costs), utilities = by_stratum(utilities))
  stratified <- suppressMessages(tm_run())
  expect_equal(stratified$by_run, plain$by_run)
})

test_that("a patient whose entry stratum has no entry-state row stops tm_run() before anything is written", {
  local_toy_project(cohort = toy_cohort(n = 2, sex = c("men", "women")), entry_states = toy_entry_states()[sex == "men"])
  expect_error(tm_run(), "entry_states.csv has no row for the entry stratum of 1 patients \\(ids 2\\)")
  expect_false(dir.exists("outputs"))
})

test_that("tm_run() writes every output file and matches its parts", {
  local_toy_project(cohort = toy_cohort(n = 300), iabc = toy_matrix(p_sick = 0.05),
                    settings = toy_settings(n_runs = 4, wtp = c(1000, 5000)))
  results <- suppressMessages(tm_run())
  expect_true(all(file.exists(file.path("outputs", c("summary.csv", "summary_by_run.csv", "comparison.csv",
                                                      "comparison_by_run.csv", "ce_plane.png")))))
  expect_equal(results$by_run$seed, rep(1:4, each = 2))
  expect_equal(results$summary[arm == "iabc", cost_per_patient],
               results$by_run[arm == "iabc", mean(cost_per_patient)])
  expect_equal(results$comparison$icer,
               mean(results$comparison_by_run$delta_cost) / mean(results$comparison_by_run$delta_qaly))
  expect_equal(results$wtp, c(1000, 5000))
  expect_equal(data.table::fread("outputs/comparison.csv")$icer, results$comparison$icer)
})

test_that("run r uses seed + r - 1, so a single run can be reproduced", {
  local_toy_project(cohort = toy_cohort(n = 300), settings = toy_settings(n_runs = 3))
  results <- suppressMessages(tm_run())
  run2 <- toy_run(seed = 2)
  single <- tm_summarise(toy_outcomes(run2), run2$matrices$usual_care$death, run2$settings)
  expect_equal(single[order(arm)]$cost_total, results$by_run[run == 2][order(arm)]$cost_total)
})

test_that("tm_summarise() counts events and deaths", {
  local_toy_project(cohort = toy_cohort(n = 200))
  run <- toy_run()
  s <- tm_summarise(toy_outcomes(run), "Dead", run$settings)
  expect_equal(s[arm == "usual_care", sick], run$sim[arm == "usual_care", sum(state == "Sick")])
  expect_equal(s[arm == "usual_care", deaths], run$sim[arm == "usual_care", sum(state == "Dead")])
})

test_that("tm_run() lists the death states it found before the runs", {
  local_toy_project()
  messages <- testthat::capture_messages(tm_run())
  expect_match(messages[1], "^Death states: Dead\\.")
})

test_that("bad settings for a run are reported", {
  local_toy_project(settings = toy_settings(n_runs = 0))
  expect_error(tm_run(), "needs `n_runs`")
  local_toy_project(settings = toy_settings(events = list(sick = "Ill")))
  expect_error(tm_run(), "event `sick` lists states not in the transition matrices: Ill")
  local_toy_project(settings = toy_settings(wtp = -5))
  expect_error(suppressMessages(tm_run()), "`wtp` must be a list of positive numbers")
  local_toy_project(settings = toy_settings(events = list(deaths = "Dead")))
  expect_error(tm_run(), "event names cannot be .*deaths; rename deaths")
})

test_that("the ICER is reported by quadrant, so a negative or undefined ICER is not misread", {
  expect_equal(icer_quadrant(c(100, -100, 100, -100, 100), c(0.1, 0.1, -0.1, -0.1, 0)),
               c("costlier, more QALYs", "dominant", "dominated", "cheaper, fewer QALYs", "no QALY difference"))
  expect_equal(icer_text(20558.4, "costlier, more QALYs", "\u20ac"), "\u20ac20,558 per QALY gained")
  expect_equal(icer_text(1000, "cheaper, fewer QALYs", "\u20ac"), "\u20ac1,000 saved per QALY lost")
  expect_equal(icer_text(-1000, "dominant", "\u20ac"), "iABC dominant (cheaper, more QALYs)")
  expect_equal(icer_text(NaN, "no QALY difference", "\u20ac"), "undefined (no difference in QALYs)")

  # iABC costs more and gives fewer QALYs: the ICER is -1000, but iABC is dominated.
  costs <- data.table::data.table(state = c("Well", "Sick", "Dead"), usual_care = c(100, 1000, 500),
                                  iabc = c(200, 1000, 500))
  utilities <- data.table::data.table(state = c("Well", "Sick"), usual_care = c(0.9, 0.5), iabc = c(0.8, 0.5))
  local_toy_project(usual_care = stay_well, iabc = stay_well, costs = costs, utilities = utilities,
                    settings = toy_settings(horizon = 3))
  results <- suppressMessages(tm_run())
  expect_equal(results$comparison$icer, -1000)
  expect_equal(data.table::fread("outputs/comparison.csv")$quadrant, "dominated")
  expect_output(print(results), "ICER: +iABC dominated \\(costlier, fewer QALYs\\)")
  labels <- unlist(lapply(ggplot2::ggplot_build(ce_plane(results))$data, function(d) d$label))
  expect_true("Mean: iABC dominated (costlier, fewer QALYs)" %in% labels)
})

test_that("the results print and the plane is a ggplot with one point per run", {
  local_toy_project(cohort = toy_cohort(n = 200), iabc = toy_matrix(p_sick = 0.05),
                    settings = toy_settings(n_runs = 3, wtp = 10000))
  results <- suppressMessages(tm_run())
  expect_output(print(results), "ICER:")
  p <- ce_plane(results)
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 3)
})

test_that("willingness-to-pay lines stay inside the plotted area", {
  ends <- wtp_line_ends(c(1000, 100000), x_lim = 0.01, y_lim = 50)
  expect_true(all(ends$x_end <= 0.01 + 1e-12))
  expect_true(all(ends$y_end <= 50 + 1e-9))
  expect_equal(ends$y_end, ends$wtp * ends$x_end)
  expect_equal(nrow(wtp_line_ends(numeric(0), 1, 1)), 0)
})

test_that("the plane is centred on the origin, showing all four quadrants", {
  local_toy_project(cohort = toy_cohort(n = 200), iabc = toy_matrix(p_sick = 0.05),
                    settings = toy_settings(n_runs = 3, wtp = 10000))
  results <- suppressMessages(tm_run())
  limits <- ce_plane(results)$coordinates$limits
  expect_equal(limits$x[1], -limits$x[2])
  expect_equal(limits$y[1], -limits$y[2])
  expect_true(all(abs(results$comparison_by_run$delta_qaly) <= limits$x[2]))
  expect_true(all(abs(results$comparison_by_run$delta_cost) <= limits$y[2]))
})
