# Tests verifying decision/policy/action (D/P/A) metadata flows correctly through
# build_ttv_decision, including multiple decision points and rich action metadata.

test_that("build_ttv_decision carries all non-id/non-time columns by default", {
  splits <- prepare_splits(data.frame(entity_id = c("p1", "p2"), split = c("train", "test"), stringsAsFactors = FALSE))

  obs <- prepare_observations(
    tables = list(bp = data.frame(
      entity_id = c("p1", "p1", "p2"),
      time = c(0, 5, 0),
      sbp = c(120, 130, 110),
      stringsAsFactors = FALSE
    )),
    specs = list(bp = list(id_col = "entity_id", time_col = "time", vars = "sbp", group = "bp"))
  )

  decisions <- data.frame(
    entity_id = c("p1", "p2"),
    decision_time = c(5, 0),
    decision_point_id = c("bp_review", "bp_review"),
    selected_action = c("intensify", "observe"),
    action_confidence = c(0.92, 0.55),
    policy_version = c("v2.1", "v2.1"),
    stringsAsFactors = FALSE
  )

  out <- build_ttv_decision(
    decisions = decisions,
    observations = obs,
    splits = splits,
    predictor_vars = "sbp",
    keep_provenance = FALSE
  )

  # All decision metadata columns should be preserved
  expect_true("selected_action" %in% names(out))
  expect_true("action_confidence" %in% names(out))
  expect_true("policy_version" %in% names(out))
  expect_equal(out$selected_action, c("intensify", "observe"))
  expect_equal(out$action_confidence, c(0.92, 0.55))
})

test_that("build_ttv_decision supports multiple decision point types", {
  splits <- prepare_splits(data.frame(entity_id = "p1", split = "train", stringsAsFactors = FALSE))

  obs <- prepare_observations(
    tables = list(bp = data.frame(
      entity_id = c("p1", "p1", "p1"),
      time = c(0, 3, 7),
      sbp = c(120, 135, 140),
      stringsAsFactors = FALSE
    )),
    specs = list(bp = list(id_col = "entity_id", time_col = "time", vars = "sbp", group = "bp"))
  )

  decisions <- data.frame(
    entity_id = c("p1", "p1"),
    decision_time = c(3, 7),
    decision_point_id = c("bp_review", "med_adjust"),
    selected_action = c("observe", "add_statin"),
    stringsAsFactors = FALSE
  )

  out <- build_ttv_decision(
    decisions = decisions,
    observations = obs,
    splits = splits,
    predictor_vars = "sbp",
    keep_provenance = FALSE
  )

  expect_equal(nrow(out), 2)
  expect_equal(out$decision_point_id, c("bp_review", "med_adjust"))
  expect_equal(out$selected_action, c("observe", "add_statin"))
  expect_equal(out$sbp, c(135, 140))
})

test_that("build_ttv_decision carry_cols selects specific columns", {
  splits <- prepare_splits(data.frame(entity_id = "p1", split = "train", stringsAsFactors = FALSE))

  obs <- prepare_observations(
    tables = list(bp = data.frame(
      entity_id = "p1", time = 0, sbp = 120, stringsAsFactors = FALSE
    )),
    specs = list(bp = list(id_col = "entity_id", time_col = "time", vars = "sbp", group = "bp"))
  )

  decisions <- data.frame(
    entity_id = "p1",
    decision_time = 0,
    decision_point_id = "bp_review",
    selected_action = "observe",
    internal_score = 0.42,
    stringsAsFactors = FALSE
  )

  out <- build_ttv_decision(
    decisions = decisions,
    observations = obs,
    splits = splits,
    predictor_vars = "sbp",
    carry_cols = c("decision_point_id", "selected_action"),
    keep_provenance = FALSE
  )

  expect_true("selected_action" %in% names(out))
  expect_true("decision_point_id" %in% names(out))
  # internal_score should NOT be carried when carry_cols is explicit

  expect_false("internal_score" %in% names(out))
})

test_that("build_ttv_decision with row_policy drop_incomplete removes missing predictors", {
  splits <- prepare_splits(data.frame(entity_id = c("p1", "p2"), split = c("train", "test"), stringsAsFactors = FALSE))

  obs <- prepare_observations(
    tables = list(bp = data.frame(
      entity_id = c("p1"),
      time = c(5),
      sbp = c(130),
      stringsAsFactors = FALSE
    )),
    specs = list(bp = list(id_col = "entity_id", time_col = "time", vars = "sbp", group = "bp"))
  )

  decisions <- data.frame(
    entity_id = c("p1", "p2"),
    decision_time = c(5, 5),
    decision_point_id = c("review", "review"),
    selected_action = c("act", "wait"),
    stringsAsFactors = FALSE
  )

  out <- build_ttv_decision(
    decisions = decisions,
    observations = obs,
    splits = splits,
    predictor_vars = "sbp",
    row_policy = "drop_incomplete",
    keep_provenance = FALSE
  )

  # p2 has no observations, so should be dropped
  expect_equal(nrow(out), 1)
  expect_equal(out$entity_id, "p1")
})
