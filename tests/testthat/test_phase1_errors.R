test_that("Phase 1: prepare_splits errors clearly on missing required columns", {
  bad <- data.frame(entity_id = c("p1", "p2"), stringsAsFactors = FALSE)
  expect_error(
    prepare_splits(bad),
    "missing required column(s): split",
    fixed = TRUE
  )
})

test_that("Phase 1: prepare_events errors clearly on missing required columns", {
  bad <- data.frame(entity_id = c("p1", "p2"), stringsAsFactors = FALSE)
  expect_error(
    prepare_events(bad),
    "missing required column(s): time",
    fixed = TRUE
  )
})

test_that("Phase 1: prepare_observations errors clearly when a table is missing required columns", {
  tbl <- data.frame(entity_id = c("p1", "p1"), t = c(0, 1), sbp = c(120, 130), stringsAsFactors = FALSE)
  expect_error(
    prepare_observations(
      tables = list(bp = tbl),
      specs = list(bp = list(id_col = "entity_id", time_col = "time", vars = c("sbp"), group = "bp"))
    ),
    "missing required column(s): time",
    fixed = TRUE
  )
})
