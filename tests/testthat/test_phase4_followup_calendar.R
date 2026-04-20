test_that("build_ttv_state converts followup Date times using ctx$time", {
  splits <- prepare_splits(
    data.frame(pid = c("a","b"), split = c("train","test")),
    id_col = "pid", split_col = "split"
  )

  # Two observations per entity so we form at least one interval.
  obs <- prepare_observations(
    tables = list(
      labs = data.frame(
        pid = c("a","a","b","b"),
        t   = c(1,5,2,6),
        sbp = c(120, 130, 110, 115),
        stringsAsFactors = FALSE
      )
    ),
    specs = list(
      labs = list(id_col = "pid", time_col = "t", vars = c("sbp"), group = "bp")
    ),
    ctx = NULL,
    sort = FALSE
  )

  followup <- data.frame(
    entity_id = c("a","b"),
    followup_start = as.Date(c("1970-01-01","1970-01-01")),
    followup_end   = as.Date(c("1970-01-06","1970-01-06"))
  )

  ctx <- list(time = list(unit = "days", zone = "UTC"))

  out <- build_ttv_state(
    observations = obs,
    splits = splits,
    ctx = ctx,
    outcome_group = "bp",
    outcome_vars = "sbp",
    predictor_vars = "sbp",
    followup = followup,
    row_policy = "return_all"
  )

  expect_true(is.numeric(out$t0))
  expect_true(is.numeric(out$t1))
  expect_true(all(out$t1 <= 5))
})