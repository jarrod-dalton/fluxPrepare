# Additional Phase 6 coverage to pin anchor-boundary and missingness semantics.

library(testthat)

.build_schema <- function() {
  schema <- patientSimCore::default_patient_schema()
  schema$sbp <- list(type = "continuous", default = NA_real_, coerce = as.numeric)
  schema
}

.build_obs <- function() {
  data.frame(
    patient_id = c("p1", "p1", "p1", "p2"),
    time       = c(0, 5, 10, 10),
    group      = c("bp", "bp", "bp", "bp"),
    sbp        = c(120, 130, 115, 140),
    stringsAsFactors = FALSE
  )
}

test_that("Phase 6: include_current toggles whether anchor-time observations are counted", {
  obs <- .build_obs()

  anchors <- data.frame(
    patient_id = c("p1"),
    t0 = c(5),
    stringsAsFactors = FALSE
  )

  schema <- .build_schema()

  derived_fns <- list(
    n_sbp_12_excl = patientSimCore::derive(
      "n_sbp_12_excl",
      target = patientSimCore::var("sbp"),
      lookback_t = 12,
      fn = "count",
      include_current = FALSE,
      force = FALSE
    ),
    n_sbp_12_incl = patientSimCore::derive(
      "n_sbp_12_incl",
      target = patientSimCore::var("sbp"),
      lookback_t = 12,
      fn = "count",
      include_current = TRUE,
      force = FALSE
    )
  )

  provider <- core_derived_provider(schema = schema, derived_var_fns = derived_fns)

  out <- reconstruct_state_at(
    anchors = anchors,
    observations = obs,
    vars = c("sbp"),
    keep_provenance = FALSE,
    derived_vars = c("n_sbp_12_excl", "n_sbp_12_incl"),
    derived_provider = provider,
    derived_context = list(observations = obs),
    derived_on_missing = "na",
    keep_derived_provenance = TRUE,
    count_no_history = "zero",
    count_vars = c("n_sbp_12_excl", "n_sbp_12_incl")
  )

  row <- out[1, , drop = FALSE]
  expect_equal(row$n_sbp_12_excl, 1)
  expect_equal(row$n_sbp_12_incl, 2)
})

test_that("Phase 6: multiple anchors for same patient do not leak state across anchors", {
  obs <- .build_obs()

  anchors <- data.frame(
    patient_id = c("p1", "p1"),
    t0 = c(5, 10),
    stringsAsFactors = FALSE
  )

  schema <- .build_schema()

  derived_fns <- list(
    sbp_lag1 = patientSimCore::lag_of(
      "sbp_lag1",
      patientSimCore::var("sbp"),
      k = 1,
      include_current = FALSE,
      force = TRUE
    ),
    n_sbp_12 = patientSimCore::derive(
      "n_sbp_12",
      target = patientSimCore::var("sbp"),
      lookback_t = 12,
      fn = "count",
      include_current = FALSE,
      force = FALSE
    )
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

  r5  <- out[out$patient_id == "p1" & out$t0 == 5,  , drop = FALSE]
  r10 <- out[out$patient_id == "p1" & out$t0 == 10, , drop = FALSE]

  expect_equal(r5$sbp, 130)
  expect_equal(r5$sbp_lag1, 120)
  expect_equal(r5$n_sbp_12, 1)

  expect_equal(r10$sbp, 115)
  expect_equal(r10$sbp_lag1, 130)
  expect_equal(r10$n_sbp_12, 2)
})

test_that("Phase 6: derived_on_missing='error' fails if a derived value is missing", {
  obs <- .build_obs()

  anchors <- data.frame(
    patient_id = c("p2"),
    t0 = c(0),
    stringsAsFactors = FALSE
  )

  schema <- .build_schema()

  derived_fns <- list(
    sbp_lag1_nf = patientSimCore::lag_of(
      "sbp_lag1_nf",
      patientSimCore::var("sbp"),
      k = 1,
      include_current = FALSE,
      force = FALSE
    )
  )

  provider <- core_derived_provider(schema = schema, derived_var_fns = derived_fns)

  expect_error(
    reconstruct_state_at(
      anchors = anchors,
      observations = obs,
      vars = c("sbp"),
      keep_provenance = FALSE,
      derived_vars = c("sbp_lag1_nf"),
      derived_provider = provider,
      derived_context = list(observations = obs),
      derived_on_missing = "error",
      keep_derived_provenance = TRUE
    ),
    regexp = "derived|missing|sbp_lag1_nf|avail",
    ignore.case = TRUE
  )
})
