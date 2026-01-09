test_that("Phase 5: batch manifests + outputs are deterministic for fixed inputs", {
  skip_if_not_installed("digest")
  skip_if_not_installed("jsonlite")

  tmp1 <- tempfile("psprep_phase5_det1_")
  tmp2 <- tempfile("psprep_phase5_det2_")
  dir.create(tmp1)
  dir.create(tmp2)

  splits <- data.frame(
    patient_id = c("p1", "p2"),
    split = c("train", "test"),
    stringsAsFactors = FALSE
  )
  splits <- ps_prepare_splits(splits)

  obs_tbl <- data.frame(
    patient_id = c("p1", "p1", "p2", "p2"),
    time = c(0, 10, 0, 10),
    sbp = c(120, 130, 110, 115),
    stringsAsFactors = FALSE
  )
  observations <- ps_prepare_observations(
    tables = list(bp = obs_tbl),
    specs = list(bp = list(id_col = "patient_id", time_col = "time", vars = c("sbp"), group = "bp"))
  )

  schema <- list(
    sbp = list(type = "continuous", default = NA_real_)
  )

  spec1 <- ps_spec_state(
    schema = schema,
    name = "bp_sbp",
    outcome_group = "bp",
    outcome_vars = c("sbp"),
    predictor_vars = c("sbp"),
    lookback = 100,
    staleness = 100,
    keep_provenance = FALSE
  )

  man1 <- ps_build_ttv_batch(
    specs = list(spec1),
    splits = splits,
    observations = observations,
    out_dir = tmp1,
    format = "rds",
    overwrite = TRUE,
    strict = TRUE,
    seed = 123,
    chunk = list(method = "n_chunks", n_chunks = 1, shuffle = FALSE)
  )

  man2 <- ps_build_ttv_batch(
    specs = list(spec1),
    splits = splits,
    observations = observations,
    out_dir = tmp2,
    format = "rds",
    overwrite = TRUE,
    strict = TRUE,
    seed = 123,
    chunk = list(method = "n_chunks", n_chunks = 1, shuffle = FALSE)
  )

  # Manifest content should be identical up to directory-specific paths and timestamps.
  keep <- c("spec_id", "spec_name", "spec_hash", "task", "fun", "chunk_id", "n_rows", "n_patients", "status", "error_message")
  expect_equal(man1[, keep], man2[, keep])

  # Data outputs should be identical.
  d1 <- readRDS(man1$path_data[1])
  d2 <- readRDS(man2$path_data[1])

  # Stable ordering for comparison
  ord_cols <- intersect(c("patient_id", "t0", "t1", "interval_id"), names(d1))
  if (length(ord_cols) > 0) {
    o1 <- do.call(order, d1[ord_cols])
    o2 <- do.call(order, d2[ord_cols])
    d1 <- d1[o1, , drop = FALSE]
    d2 <- d2[o2, , drop = FALSE]
    rownames(d1) <- NULL
    rownames(d2) <- NULL
  }

  expect_equal(d1, d2)
})
