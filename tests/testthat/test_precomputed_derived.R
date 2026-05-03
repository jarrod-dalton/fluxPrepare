# Tests verifying precomputed derived columns flow through the full pipeline:
# prepare_observations() -> reconstruct_state_at() -> build_ttv_state() / build_ttv_decision()

test_that("precomputed derived columns survive prepare_observations and reconstruct at anchors", {
  # Simulate a table where sbp_lag1 was precomputed before entering the pipeline
  raw <- data.frame(
    entity_id = c("p1", "p1", "p1", "p2", "p2"),
    time      = c(0,     5,    10,    0,    5),
    sbp       = c(120,  130,   115,  140,  135),
    sbp_lag1  = c(NA,   120,   130,   NA,  140),
    stringsAsFactors = FALSE
  )

  obs <- prepare_observations(
    tables = list(vitals = raw),
    specs = list(vitals = list(
      id_col = "entity_id", time_col = "time",
      vars = c("sbp", "sbp_lag1"), group = "bp"
    ))
  )

  # sbp_lag1 must survive as a column

  expect_true("sbp_lag1" %in% names(obs))

  # Reconstruct at anchor t0=5 for p1: should pick up sbp=130, sbp_lag1=120
  anchors <- data.frame(entity_id = "p1", t0 = 5, stringsAsFactors = FALSE)
  out <- reconstruct_state_at(anchors, obs, vars = c("sbp", "sbp_lag1"))
  expect_equal(out$sbp, 130)
  expect_equal(out$sbp_lag1, 120)
})

test_that("precomputed derived columns flow through build_ttv_state", {
  raw <- data.frame(
    entity_id = c("p1", "p1", "p1"),
    time      = c(0,     5,    10),
    sbp       = c(120,  130,   115),
    sbp_lag1  = c(NA,   120,   130),
    stringsAsFactors = FALSE
  )

  obs <- prepare_observations(
    tables = list(vitals = raw),
    specs = list(vitals = list(
      id_col = "entity_id", time_col = "time",
      vars = c("sbp", "sbp_lag1"), group = "bp"
    ))
  )

  splits <- prepare_splits(data.frame(entity_id = "p1", split = "train", stringsAsFactors = FALSE))

  out <- build_ttv_state(
    observations = obs,
    splits = splits,
    outcome_group = "bp",
    outcome_vars = c("sbp"),
    predictor_vars = c("sbp", "sbp_lag1"),
    keep_provenance = FALSE
  )

  expect_s3_class(out, "flux_ttv_state")
  expect_true("sbp_lag1" %in% names(out))

  # First interval: t0=0, predictors reconstructed at t0=0
  row1 <- out[out$t0 == 0, ]
  expect_equal(row1$sbp_lag1, NA_real_)  # No lag available at t=0

  # Second interval: t0=5, predictors reconstructed at t0=5
  row2 <- out[out$t0 == 5, ]
  expect_equal(row2$sbp_lag1, 120)  # Lag from t=0 observation
})

test_that("precomputed derived columns flow through build_ttv_decision", {
  raw <- data.frame(
    entity_id = c("p1", "p1"),
    time      = c(0,     5),
    sbp       = c(120,  130),
    sbp_lag1  = c(NA,   120),
    stringsAsFactors = FALSE
  )

  obs <- prepare_observations(
    tables = list(vitals = raw),
    specs = list(vitals = list(
      id_col = "entity_id", time_col = "time",
      vars = c("sbp", "sbp_lag1"), group = "bp"
    ))
  )

  splits <- prepare_splits(data.frame(entity_id = "p1", split = "train", stringsAsFactors = FALSE))

  decisions <- data.frame(
    entity_id = "p1",
    decision_time = 5,
    decision_point_id = "bp_review",
    selected_action = "intensify",
    stringsAsFactors = FALSE
  )

  out <- build_ttv_decision(
    decisions = decisions,
    observations = obs,
    splits = splits,
    predictor_vars = c("sbp", "sbp_lag1"),
    keep_provenance = FALSE
  )

  expect_s3_class(out, "flux_ttv_decision")
  expect_true("sbp_lag1" %in% names(out))
  expect_equal(out$sbp, 130)
  expect_equal(out$sbp_lag1, 120)
  expect_equal(out$selected_action, "intensify")
})
