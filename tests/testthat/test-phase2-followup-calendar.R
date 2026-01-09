test_that("ps_build_ttv_event converts followup Date times using ctx$time", {
  splits <- ps_prepare_splits(
    data.frame(pid = c("a","b"), split = c("train","test")),
    id_col = "pid", split_col = "split"
  )

  ev <- ps_prepare_events(
    data.frame(pid = c("a","a","b"), t = c(1,5,2), type = c("visit","mi","visit")),
    id_col = "pid", time_col = "t", type_col = "type", sort = FALSE
  )

  # Followup in calendar time (Date); maps to numeric days since 1970-01-01.
  followup <- data.frame(
    patient_id = c("a","b"),
    followup_start = as.Date(c("1970-01-01","1970-01-01")),
    followup_end   = as.Date(c("1970-01-11","1970-01-11"))
  )

  ctx <- list(time = list(unit = "days", zone = "UTC"))

  out <- ps_build_ttv_event(
    ev, splits,
    ctx = ctx,
    event_type = "mi",
    followup = followup
  )

  expect_true(is.numeric(out$t0))
  expect_true(is.numeric(out$t1))
  expect_true(all(out$t0 == 0))
  expect_true(all(out$t1 <= 10))
})
