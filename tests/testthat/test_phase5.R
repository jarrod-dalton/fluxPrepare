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


test_that("build_ttv_batch passes observations into event specs with predictor vars", {
  skip_if_not_installed("digest")
  skip_if_not_installed("jsonlite")

  tmp <- tempfile("psprep_phase5_event_")
  dir.create(tmp)

  splits <- prepare_splits(
    data.frame(entity_id = c("p1", "p2"), split = c("train", "test"), stringsAsFactors = FALSE)
  )

  events <- prepare_events(
    data.frame(entity_id = c("p1", "p1", "p2"), time = c(1, 5, 2), event_type = c("visit", "mi", "visit"))
  )

  observations <- prepare_observations(
    tables = list(bp = data.frame(
      entity_id = c("p1", "p1", "p2"),
      time = c(0, 4, 0),
      sbp = c(120, 125, 110),
      stringsAsFactors = FALSE
    )),
    specs = list(bp = list(id_col = "entity_id", time_col = "time", vars = c("sbp"), group = "bp"))
  )

  followup <- data.frame(entity_id = c("p1", "p2"), followup_start = c(0, 0), followup_end = c(10, 10))

  spec1 <- spec_event(
    event_type = "mi",
    predictor_vars = c("sbp"),
    keep_provenance = TRUE
  )

  man <- build_ttv_batch(
    specs = list(spec1),
    splits = splits,
    events = events,
    observations = observations,
    followup = followup,
    out_dir = tmp,
    format = "rds",
    overwrite = TRUE,
    strict = TRUE,
    chunk = list(method = "n_chunks", n_chunks = 1, shuffle = FALSE)
  )

  dat <- readRDS(man$path_data[1])
  expect_true(all(c("sbp", ".prov_sbp") %in% names(dat)))
  expect_equal(dat$sbp, c(120, 110))
})


test_that("build_ttv_batch writes decision datasets from spec_decision", {
  skip_if_not_installed("digest")
  skip_if_not_installed("jsonlite")

  tmp <- tempfile("psprep_phase5_decision_")
  dir.create(tmp)

  splits <- prepare_splits(
    data.frame(entity_id = c("p1", "p2"), split = c("train", "test"), stringsAsFactors = FALSE)
  )

  observations <- prepare_observations(
    tables = list(bp = data.frame(
      entity_id = c("p1", "p1", "p2"),
      time = c(0, 4, 0),
      sbp = c(120, 125, 110),
      stringsAsFactors = FALSE
    )),
    specs = list(bp = list(id_col = "entity_id", time_col = "time", vars = c("sbp"), group = "bp"))
  )

  decisions <- data.frame(
    entity_id = c("p1", "p2"),
    decision_time = c(4, 0),
    decision_point_id = c("bp_review", "bp_review"),
    selected_action = c("intensify", "observe"),
    stringsAsFactors = FALSE
  )

  spec1 <- spec_decision(
    predictor_vars = c("sbp"),
    carry_cols = c("decision_point_id", "selected_action"),
    keep_provenance = TRUE
  )
  spec1$args$decisions <- decisions

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

  dat <- readRDS(man$path_data[1])
  expect_s3_class(dat, "flux_ttv_decision")
  expect_true(all(c("decision_point_id", "selected_action", "sbp", ".prov_sbp") %in% names(dat)))
  expect_equal(dat$sbp, c(125, 110))
})
