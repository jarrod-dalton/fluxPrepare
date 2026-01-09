test_that("ps_build_ttv_event builds one-step intervals with censoring", {
  splits <- ps_prepare_splits(
    data.frame(pid = c("a","b"), split = c("train","test")),
    id_col = "pid", split_col = "split"
  )

  # Patient a has target event at t=5; patient b does not
  ev <- ps_prepare_events(
    data.frame(pid = c("a","a","b"), t = c(1,5,2), type = c("visit","mi","visit")),
    id_col = "pid", time_col = "t", type_col = "type"
  )

  followup <- data.frame(
    patient_id = c("a","b"),
    followup_start = c(0,0),
    followup_end = c(10,10)
  )

  out <- ps_build_ttv_event(
    events = ev,
    splits = splits,
    event_type = "mi",
    t0_strategy = "followup_start",
    followup = followup,
    fu_start_col = "followup_start",
    fu_end_col = "followup_end"
  )

  expect_s3_class(out, "ps_ttv_event")
  expect_equal(out$t0, c(0,0))
  expect_equal(out$event_occurred, c(TRUE, FALSE))
  expect_equal(out$t1, c(5,10))
  expect_equal(out$deltat, c(5,10))
})

test_that("ps_build_ttv_event errors when followup missing patients", {
  splits <- ps_prepare_splits(
    data.frame(pid = c("a","b"), split = c("train","test")),
    id_col = "pid", split_col = "split"
  )
  ev <- ps_prepare_events(
    data.frame(pid = c("a","b"), t = c(1,2), type = c("mi","mi")),
    id_col = "pid", time_col = "t", type_col = "type"
  )

  followup <- data.frame(patient_id = "a", followup_start = 0, followup_end = 10)
  expect_error(
    ps_build_ttv_event(ev, splits, event_type = "mi", followup = followup),
    "missing"
  )
})
