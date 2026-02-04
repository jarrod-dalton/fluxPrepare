test_that("Phase 6: reconstruct_state_at can add Core-derived variables at anchors", {
  # Build canonical observations
  obs <- data.frame(
    patient_id = c("p1","p1","p1","p2"),
    time = c(0, 5, 10, 10),
    group = c("bp","bp","bp","bp"),
    sbp = c(120, 130, 115, 140),
    stringsAsFactors = FALSE
  )

  # Anchors
  anchors <- data.frame(
    patient_id = c("p1","p1","p2"),
    t0 = c(5, 10, 0),
    stringsAsFactors = FALSE
  )

  schema <- patientSimCore::default_patient_schema()
  schema$sbp <- list(type = "continuous", default = NA_real_, coerce = as.numeric)

  derived_fns <- list(
    sbp_lag1 = patientSimCore::lag_of("sbp_lag1", patientSimCore::var("sbp"), k = 1, include_current = FALSE, force = TRUE),
    n_sbp_12 = patientSimCore::derive("n_sbp_12", target = patientSimCore::var("sbp"), lookback_t = 12, fn = "count", include_current = FALSE, force = FALSE)
  )

  provider <- core_derived_provider(schema = schema, derived_var_fns = derived_fns)

  out <- reconstruct_state_at(
    anchors = anchors,
    observations = obs,
    vars = c("sbp"),
    keep_provenance = FALSE,
    derived_vars = c("sbp_lag1", "n_sbp_12"),
    derived_provider = provider,
    derived_context = list(observations = obs),
    derived_on_missing = "na",
    keep_derived_provenance = TRUE,
    count_no_history = "zero",
    count_vars = "n_sbp_12"
  )

  # p1, t0=5: sbp at t0 is 130; lag1 is 120; count in window excluding current is 1
  row1 <- out[out$patient_id == "p1" & out$t0 == 5, , drop = FALSE]
  expect_equal(row1$sbp, 130)
  expect_equal(row1$sbp_lag1, 120)
  expect_equal(row1$n_sbp_12, 1)
  expect_true(row1$.avail_sbp_lag1)
  expect_true(row1$.avail_n_sbp_12)

  # p2, t0=0: no history; count coerced to 0, but availability FALSE
  row2 <- out[out$patient_id == "p2" & out$t0 == 0, , drop = FALSE]
  expect_equal(row2$n_sbp_12, 0)
  expect_false(row2$.avail_n_sbp_12)
})
