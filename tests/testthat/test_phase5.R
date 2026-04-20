test_that("build_ttv_batch writes datasets + manifest (rds)", {
  skip_if_not_installed("digest")
  skip_if_not_installed("jsonlite")

  tmp <- tempfile("psprep_phase5_")
  dir.create(tmp)

  splits <- data.frame(
    entity_id = c("p1", "p2"),
    split = c("train", "test"),
    stringsAsFactors = FALSE
  )
  splits <- prepare_splits(splits)

  obs_tbl <- data.frame(
    entity_id = c("p1", "p1", "p2", "p2"),
    time = c(0, 10, 0, 10),
    sbp = c(120, 130, 110, 115),
    stringsAsFactors = FALSE
  )
  observations <- prepare_observations(
    tables = list(bp = obs_tbl),
    specs = list(bp = list(id_col = "entity_id", time_col = "time", vars = c("sbp"), group = "bp"))
  )

  schema <- list(
    sbp = list(type = "continuous", default = NA_real_)
  )

  spec1 <- spec_state(
    schema = schema,
    name = "bp_sbp",
    outcome_group = "bp",
    outcome_vars = c("sbp"),
    predictor_vars = c("sbp"),
    lookback = 100,
    staleness = 100,
    keep_provenance = FALSE
  )

  man <- build_ttv_batch(
    specs = list(spec1),
    splits = splits,
    observations = observations,
    out_dir = tmp,
    format = "rds",
    overwrite = TRUE,
    strict = TRUE,
    chunk = list(method = "n_chunks", n_chunks = 1, shuffle = FALSE)
  )

  expect_s3_class(man, "flux_manifest")
  expect_true(file.exists(file.path(tmp, "ttv_manifest.csv")))
  expect_true(file.exists(file.path(tmp, "ttv_manifest.rds")))
  expect_true(file.exists(man$path_data[1]))
  expect_true(file.exists(man$path_metadata[1]))

  dat <- readRDS(man$path_data[1])
  expect_true(is.data.frame(dat))
  expect_true(nrow(dat) > 0)
})
