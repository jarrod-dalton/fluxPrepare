test_that("build_ttv_decision reconstructs predictors at decision anchors", {
  splits <- prepare_splits(
    data.frame(pid = c("a", "b"), split = c("train", "test")),
    id_col = "pid", split_col = "split"
  )

  obs <- prepare_observations(
    tables = list(bp = data.frame(
      pid = c("a", "a", "b"),
      time = c(0, 4, 0),
      sbp = c(120, 130, 110),
      dbp = c(80, 85, 70),
      stringsAsFactors = FALSE
    )),
    specs = list(bp = list(id_col = "pid", time_col = "time", vars = c("sbp", "dbp"), group = "bp"))
  )

  decisions <- data.frame(
    pid = c("a", "b"),
    decision_time = c(4, 0),
    decision_point_id = c("bp_review", "bp_review"),
    selected_action = c("intensify", "observe"),
    stringsAsFactors = FALSE
  )

  out <- build_ttv_decision(
    decisions = decisions,
    observations = obs,
    splits = splits,
    predictor_vars = c("sbp", "dbp"),
    id_col = "pid",
    keep_provenance = TRUE
  )

  expect_s3_class(out, "flux_ttv_decision")
  expect_equal(out$split, c("train", "test"))
  expect_equal(out$decision_point_id, c("bp_review", "bp_review"))
  expect_equal(out$selected_action, c("intensify", "observe"))
  expect_equal(out$decision_time, c(4, 0))
  expect_equal(out$sbp, c(130, 110))
  expect_equal(out$dbp, c(85, 70))
  expect_equal(out$.prov_sbp, c("observed", "observed"))
})


test_that("build_ttv_decision supports Date decision times via time_spec", {
  splits <- prepare_splits(data.frame(entity_id = "a", split = "train", stringsAsFactors = FALSE))

  obs <- prepare_observations(
    tables = list(bp = data.frame(
      entity_id = c("a", "a"),
      time = c(0, 7),
      sbp = c(120, 130),
      stringsAsFactors = FALSE
    )),
    specs = list(bp = list(id_col = "entity_id", time_col = "time", vars = c("sbp"), group = "bp"))
  )

  decisions <- data.frame(
    entity_id = "a",
    decision_time = as.Date("1970-01-08"),
    decision_point_id = "weekly_review",
    stringsAsFactors = FALSE
  )
  tspec <- fluxCore::time_spec(unit = "days", origin = as.Date("1970-01-01"), zone = "UTC")

  out <- build_ttv_decision(
    decisions = decisions,
    observations = obs,
    splits = splits,
    predictor_vars = c("sbp"),
    time_spec = tspec
  )

  expect_equal(out$decision_time, 7)
  expect_equal(out$sbp, 130)
})