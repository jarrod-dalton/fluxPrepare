
test_that("build_ttv_event fails fast on bad event times and event_type", {
  splits <- prepare_splits(
    data.frame(pid = c("a"), split = c("train")),
    id_col = "pid", split_col = "split"
  )

  # Bad (missing) time in events should error
  bad_ev_time <- data.frame(patient_id = "a", time = c(1, NA), event_type = c("mi", "mi"))
  expect_error(
    build_ttv_event(bad_ev_time, splits, event_type = "mi"),
    "time"
  )

  # Bad (empty) event_type values should error
  bad_ev_type <- data.frame(patient_id = "a", time = c(1, 2), event_type = c("mi", ""))
  expect_error(
    build_ttv_event(bad_ev_type, splits, event_type = "mi"),
    "event_type"
  )
})
