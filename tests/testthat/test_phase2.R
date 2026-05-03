test_that("build_ttv_event builds one-step intervals with censoring", {
  splits <- prepare_splits(
    data.frame(pid = c("a","b"), split = c("train","test")),
    id_col = "pid", split_col = "split"
  )

  # Entity a has target event at t=5; entity b does not
  ev <- prepare_events(
    data.frame(pid = c("a","a","b"), t = c(1,5,2), type = c("visit","mi","visit")),
    id_col = "pid", time_col = "t", type_col = "type"
  )

  followup <- data.frame(
    entity_id = c("a","b"),
    followup_start = c(0,0),
    followup_end = c(10,10)
  )

  out <- build_ttv_event(
    events = ev,
    splits = splits,
    event_type = "mi",
    t0_strategy = "followup_start",
    followup = followup,
    fu_start_col = "followup_start",
    fu_end_col = "followup_end"
  )

  expect_s3_class(out, "flux_ttv_event")
  expect_equal(out$t0, c(0,0))
  expect_equal(out$event_occurred, c(TRUE, FALSE))
  expect_equal(out$t1, c(5,10))
  expect_equal(out$deltat, c(5,10))
})

test_that("build_ttv_event errors when followup missing entities", {
  splits <- prepare_splits(
    data.frame(pid = c("a","b"), split = c("train","test")),
    id_col = "pid", split_col = "split"
  )
  ev <- prepare_events(
    data.frame(pid = c("a","b"), t = c(1,2), type = c("mi","mi")),
    id_col = "pid", time_col = "t", type_col = "type"
  )

  followup <- data.frame(entity_id = "a", followup_start = 0, followup_end = 10)
  expect_error(
    build_ttv_event(ev, splits, event_type = "mi", followup = followup),
    "missing"
  )
})


test_that("build_ttv_event can include reconstructed predictor variables at t0", {
  splits <- prepare_splits(
    data.frame(pid = c("a","b"), split = c("train","test")),
    id_col = "pid", split_col = "split"
  )

  ev <- prepare_events(
    data.frame(pid = c("a","a","b"), t = c(1,5,2), type = c("visit","mi","visit")),
    id_col = "pid", time_col = "t", type_col = "type"
  )

  followup <- data.frame(
    entity_id = c("a","b"),
    followup_start = c(0,0),
    followup_end = c(10,10)
  )

  obs <- prepare_observations(
    tables = list(bp = data.frame(
      pid = c("a", "a", "b"),
      time = c(0, 4, 0),
      sbp = c(120, 125, 110),
      dbp = c(80, 82, 70)
    )),
    specs = list(bp = list(id_col = "pid", time_col = "time", vars = c("sbp", "dbp"), group = "bp"))
  )

  out <- build_ttv_event(
    events = ev,
    observations = obs,
    splits = splits,
    predictor_vars = c("sbp", "dbp"),
    event_type = "mi",
    t0_strategy = "followup_start",
    followup = followup,
    fu_start_col = "followup_start",
    fu_end_col = "followup_end",
    keep_provenance = TRUE
  )

  expect_true(all(c("sbp", "dbp", ".prov_sbp", ".prov_dbp") %in% names(out)))
  expect_equal(out$sbp, c(120, 110))
  expect_equal(out$dbp, c(80, 70))
  expect_equal(out$.prov_sbp, c("observed", "observed"))
})
