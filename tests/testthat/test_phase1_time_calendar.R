library(testthat)


test_that("Phase 1: prepare_events converts Date time using ctx$time", {
  ctx <- list(time = list(unit = "weeks", origin = as.Date("1970-01-01"), zone = "UTC"))
  ev <- data.frame(
    patient_id = c("p1", "p1"),
    time = as.Date(c("1970-01-01", "1970-01-08")),
    event_type = c("a", "b"),
    stringsAsFactors = FALSE
  )
  out <- prepare_events(ev, ctx = ctx, sort = FALSE)
  expect_equal(out$time, c(0, 1))
  expect_equal(attr(out, "time_class"), "Date")
})


test_that("Phase 1: prepare_events errors on Date time without ctx", {
  ev <- data.frame(
    patient_id = c("p1", "p1"),
    time = as.Date(c("1970-01-01", "1970-01-08")),
    event_type = c("a", "b"),
    stringsAsFactors = FALSE
  )
  expect_error(prepare_events(ev, sort = FALSE), "ctx$time$unit", fixed = TRUE)
})


test_that("Phase 1: prepare_observations converts Date time using ctx$time", {
  ctx <- list(time = list(unit = "weeks", origin = as.Date("1970-01-01"), zone = "UTC"))
  labs <- data.frame(
    pid = c("p1", "p1"),
    dt = as.Date(c("1970-01-01", "1970-01-08")),
    sbp = c(120, 130),
    stringsAsFactors = FALSE
  )
  tables <- list(labs = labs)
  specs <- list(labs = list(id_col = "pid", time_col = "dt", vars = c("sbp"), group = "vitals"))

  out <- prepare_observations(tables, specs, keep_source = FALSE, ctx = ctx, sort = FALSE)
  expect_equal(out$time, c(0, 1))
  expect_equal(unname(attr(out, "time_classes")["labs"]), "Date")
})
