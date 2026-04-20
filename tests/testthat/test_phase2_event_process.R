

test_that("build_ttv_event_process splits at observation times with optional min_dt", {
  events <- data.frame(
    entity_id = c("p1","p1"),
    time = c(36, 200),
    event_type = c("hosp","other"),
    stringsAsFactors = FALSE
  )
  observations <- data.frame(
    entity_id = rep("p1", 3),
    time = c(7, 14, 28),
    group = rep("labs", 3),
    stringsAsFactors = FALSE
  )
  splits <- data.frame(entity_id = "p1", split = "train", stringsAsFactors = FALSE)
  followup <- data.frame(entity_id = "p1", followup_start = 0, followup_end = 100, stringsAsFactors = FALSE)

  spec0 <- spec_event_process(
    event_types = c("hosp","mi"),
    split_on_groups = "labs",
    min_dt = 0,
    t0_strategy = "fixed",
    fixed_t0 = 0
  )

  out0 <- build_ttv_event_process(events, observations, splits, spec0, followup = followup)
  expect_equal(out0$t0, c(0, 7, 14, 28))
  expect_equal(out0$t1, c(7, 14, 28, 36))
  expect_equal(out0$event_occurred, c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(out0$event_type, c(NA, NA, NA, "hosp"))

  spec20 <- spec_event_process(
    event_types = c("hosp","mi"),
    split_on_groups = "labs",
    min_dt = 20,
    t0_strategy = "fixed",
    fixed_t0 = 0
  )
  out20 <- build_ttv_event_process(events, observations, splits, spec20, followup = followup)
  expect_equal(out20$t0, c(0, 28))
  expect_equal(out20$t1, c(28, 36))
  expect_equal(out20$event_occurred, c(FALSE, TRUE))
  expect_equal(out20$event_type, c(NA, "hosp"))
})


test_that("build_ttv_event_process can segment on meaningful covariate changes", {
  events <- data.frame(
    entity_id = c("p1"),
    time = c(36),
    event_type = c("hosp"),
    stringsAsFactors = FALSE
  )
  observations <- data.frame(
    entity_id = rep("p1", 3),
    time = c(7, 14, 28),
    group = rep("labs", 3),
    ldl = c(140, 142, 180),
    stringsAsFactors = FALSE
  )
  splits <- data.frame(entity_id = "p1", split = "train", stringsAsFactors = FALSE)
  followup <- data.frame(entity_id = "p1", followup_start = 0, followup_end = 100, stringsAsFactors = FALSE)

  spec <- spec_event_process(
    event_types = c("hosp","mi"),
    split_on_groups = "labs",
    segment_on_vars = "ldl",
    segment_rules = segment_bins(ldl = c(-Inf, 160, Inf)),
    min_dt = 0,
    t0_strategy = "fixed",
    fixed_t0 = 0
  )

  out <- build_ttv_event_process(events, observations, splits, spec, followup = followup)
  # Only the candidate at t=28 moves LDL into a new bin (>=160)
  expect_equal(out$t0, c(0, 28))
  expect_equal(out$t1, c(28, 36))
  expect_equal(out$event_occurred, c(FALSE, TRUE))
  expect_equal(out$event_type, c(NA, "hosp"))
})


test_that("build_ttv_event_process can generate candidate times from vars without groups", {
  events <- data.frame(
    entity_id = c("p1"),
    time = c(36),
    event_type = c("hosp"),
    stringsAsFactors = FALSE
  )
  observations <- data.frame(
    entity_id = rep("p1", 3),
    time = c(7, 14, 28),
    group = rep("labs", 3),
    ldl = c(140, 142, 180),
    stringsAsFactors = FALSE
  )
  splits <- data.frame(entity_id = "p1", split = "train", stringsAsFactors = FALSE)
  followup <- data.frame(entity_id = "p1", followup_start = 0, followup_end = 100, stringsAsFactors = FALSE)

  spec <- spec_event_process(
    event_types = c("hosp","mi"),
    split_on_groups = NULL,
    segment_on_vars = "ldl",
    segment_rules = segment_bins(ldl = c(-Inf, 160, Inf)),
    candidate_times = "vars",
    min_dt = 0,
    t0_strategy = "fixed",
    fixed_t0 = 0
  )

  out <- build_ttv_event_process(events, observations, splits, spec, followup = followup)
  expect_equal(out$t0, c(0, 28))
  expect_equal(out$t1, c(28, 36))
  expect_equal(out$event_occurred, c(FALSE, TRUE))
  expect_equal(out$event_type, c(NA, "hosp"))
})
