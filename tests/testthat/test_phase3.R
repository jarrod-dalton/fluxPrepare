test_that("reconstruct_state_at performs LOCF with lookback and staleness", {
  bp <- data.frame(pid = c("a", "a"), time = c(1, 4), sbp = c(120, 130), dbp = c(80, 85))
  tables <- list(bp = bp)
  specs <- list(bp = list(id_col = "pid", time_col = "time", vars = c("sbp", "dbp"), group = "bp"))
  obs <- prepare_observations(tables, specs)

  anchors <- data.frame(pid = c("a", "a"), t0 = c(3, 5))

  out <- reconstruct_state_at(anchors, obs, vars = c("sbp", "dbp"), id_col = "pid", time_col = "t0")
  expect_s3_class(out, "flux_state_asof")
  expect_equal(out$sbp, c(120, 130))
  expect_equal(out$.prov_sbp, c("carried_forward", "carried_forward"))

  out2 <- reconstruct_state_at(anchors, obs, vars = c("sbp"), id_col = "pid", time_col = "t0", staleness = 1)
  expect_true(is.na(out2$sbp[1]))
  expect_equal(out2$sbp[2], 130)

  out3 <- reconstruct_state_at(anchors, obs, vars = c("sbp"), id_col = "pid", time_col = "t0", lookback = 1)
  expect_true(is.na(out3$sbp[1]))
  expect_equal(out3$sbp[2], 130)
})


test_that("reconstruct_state_at converts Date anchors using time_spec", {
  bp <- data.frame(pid = c("a", "a"), time = c(1, 4), sbp = c(120, 130), dbp = c(80, 85))
  tables <- list(bp = bp)
  specs <- list(bp = list(id_col = "pid", time_col = "time", vars = c("sbp", "dbp"), group = "bp"))
  obs <- prepare_observations(tables, specs)

  anchors <- data.frame(pid = c("a", "a"), t0 = as.Date(c("1970-01-03", "1970-01-05")))
  tspec <- fluxCore::time_spec(unit = "days", origin = as.Date("1970-01-01"), zone = "UTC")

  out <- reconstruct_state_at(anchors, obs, vars = c("sbp"), id_col = "pid", time_col = "t0", time_spec = tspec)

  expect_s3_class(out, "flux_state_asof")
  expect_equal(out$sbp, c(120, 130))
})


test_that("reconstruct_state_at can preserve decision-point anchor metadata", {
  bp <- data.frame(pid = c("a", "a"), time = c(1, 4), sbp = c(120, 130), dbp = c(80, 85))
  tables <- list(bp = bp)
  specs <- list(bp = list(id_col = "pid", time_col = "time", vars = c("sbp", "dbp"), group = "bp"))
  obs <- prepare_observations(tables, specs)

  anchors <- data.frame(
    entity_id = c("a", "a"),
    decision_time = c(3, 4),
    decision_point_id = c("dp_pre", "dp_post"),
    selected_action = c("wait", "treat"),
    stringsAsFactors = FALSE
  )

  out <- reconstruct_state_at(
    anchors,
    obs,
    vars = c("sbp", "dbp"),
    time_col = "decision_time",
    keep_anchor_cols = TRUE,
    keep_provenance = TRUE
  )

  expect_s3_class(out, "flux_state_asof")
  expect_true(all(c("decision_point_id", "selected_action", "t0") %in% names(out)))
  expect_equal(out$decision_point_id, c("dp_pre", "dp_post"))
  expect_equal(out$selected_action, c("wait", "treat"))
  expect_equal(out$sbp, c(120, 130))
  expect_equal(out$.prov_sbp, c("carried_forward", "observed"))
})
