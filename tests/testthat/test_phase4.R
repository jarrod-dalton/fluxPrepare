
test_that("build_ttv_state builds consecutive intervals and reconstructs predictors", {
  splits <- prepare_splits(
    data.frame(pid = c("a", "b"), split = c("train", "test")),
    id_col = "pid", split_col = "split"
  )

  bp <- data.frame(
    pid = c("a", "a", "a", "b", "b"),
    time = c(0, 10, 25, 0, 10),
    sbp = c(120, 130, 128, 110, 115),
    dbp = c(80, 85, 84, 70, 72)
  )

  tables <- list(bp = bp)
  specs <- list(bp = list(id_col = "pid", time_col = "time", vars = c("sbp", "dbp"), group = "bp"))
  obs <- prepare_observations(tables, specs)

  out <- build_ttv_state(
    observations = obs,
    splits = splits,
    outcome_group = "bp",
    outcome_vars = c("sbp"),
    predictor_vars = c("sbp", "dbp"),
    keep_provenance = TRUE
  )

  expect_s3_class(out, "ps_ttv_state")
  expect_equal(nrow(out), 3)

  # patient a: (0 -> 10), (10 -> 25); patient b: (0 -> 10)
  expect_equal(out$t0, c(0, 10, 0))
  expect_equal(out$t1, c(10, 25, 10))
  expect_equal(out$deltat, c(10, 15, 10))

  # predictors at t0 equal observed values at t0
  expect_equal(out$sbp, c(120, 130, 110))
  expect_equal(out$dbp, c(80, 85, 70))
  expect_equal(out$.prov_sbp, c("observed", "observed", "observed"))

  # outcome sbp at t1
  expect_equal(out$sbp.1, c(130, 128, 115))
})

test_that("build_ttv_state censors intervals and sets outcomes to NA", {
  splits <- prepare_splits(
    data.frame(pid = c("a"), split = c("train")),
    id_col = "pid", split_col = "split"
  )

  bp <- data.frame(
    pid = c("a", "a", "a"),
    time = c(0, 10, 25),
    sbp = c(120, 130, 128),
    dbp = c(80, 85, 84)
  )
  tables <- list(bp = bp)
  specs <- list(bp = list(id_col = "pid", time_col = "time", vars = c("sbp", "dbp"), group = "bp"))
  obs <- prepare_observations(tables, specs)

  followup <- data.frame(patient_id = "a", followup_start = 0, followup_end = 12)

  out <- build_ttv_state(
    observations = obs,
    splits = splits,
    outcome_group = "bp",
    outcome_vars = c("sbp"),
    predictor_vars = c("sbp"),
    followup = followup,
    fu_start_col = "followup_start",
    fu_end_col = "followup_end",
    keep_provenance = FALSE
  )

  expect_equal(nrow(out), 2)
  expect_equal(out$t0, c(0, 10))
  expect_equal(out$t1, c(10, 12))
  expect_equal(out$censored, c(FALSE, TRUE))
  expect_equal(out$end_type, c("observed", "followup_end"))
  expect_true(is.na(out$sbp.1[2]))
  expect_equal(out$sbp.1[1], 130)
})

test_that("build_ttv_state supports multivariate outcomes and sampling per patient", {
  splits <- prepare_splits(
    data.frame(pid = c("a", "b"), split = c("train", "test")),
    id_col = "pid", split_col = "split"
  )

  bp <- data.frame(
    pid = c("a", "a", "b"),
    time = c(0, 10, 0),
    sbp = c(120, 130, 110),
    dbp = c(80, 85, 70)
  )
  bmp <- data.frame(
    pid = c("a", "a", "a", "b", "b"),
    time = c(5, 15, 25, 5, 15),
    glucose = c(100, 105, 110, 90, 95),
    sodium = c(140, 141, 142, 138, 139)
  )

  tables <- list(bp = bp, bmp = bmp)
  specs <- list(
    bp = list(id_col = "pid", time_col = "time", vars = c("sbp", "dbp"), group = "bp"),
    bmp = list(id_col = "pid", time_col = "time", vars = c("glucose", "sodium"), group = "bmp")
  )
  obs <- prepare_observations(tables, specs)

  out <- build_ttv_state(
    observations = obs,
    splits = splits,
    outcome_group = "bmp",
    outcome_vars = c("glucose", "sodium"),
    predictor_vars = c("sbp"),
    max_intervals_per_patient = 1,
    seed = 123,
    keep_provenance = TRUE
  )

  expect_equal(nrow(out), 2)
  expect_true(all(out$patient_id %in% c("a", "b")))
  expect_true(all(c("glucose", "sodium") %in% names(out)))

  # sampling is reproducible
  out2 <- build_ttv_state(
    observations = obs,
    splits = splits,
    outcome_group = "bmp",
    outcome_vars = c("glucose", "sodium"),
    predictor_vars = c("sbp"),
    max_intervals_per_patient = 1,
    seed = 123,
    keep_provenance = TRUE
  )
  expect_equal(out$t0, out2$t0)
  expect_equal(out$t1, out2$t1)
})
