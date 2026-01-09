test_that("ps_prepare_splits enforces uniqueness and allowed labels", {
  df <- data.frame(id = c("a", "b", "c"), split = c("Train", "test", "VALIDATION"))
  out <- ps_prepare_splits(df, id_col = "id", split_col = "split")
  expect_s3_class(out, "ps_splits")
  expect_equal(out$split, c("train", "test", "validation"))

  df2 <- rbind(df, data.frame(id = "a", split = "train"))
  expect_error(ps_prepare_splits(df2, id_col = "id", split_col = "split"), "must be unique")
})

test_that("ps_prepare_events supports single table and list of tables", {
  ev <- data.frame(pid = c("a","a","b"), t = c(2,1,5), type = c("x","x","y"))
  out <- ps_prepare_events(ev, id_col = "pid", time_col = "t", type_col = "type")
  expect_s3_class(out, "ps_events")
  expect_equal(out$time, c(1,2,5))

  ev1 <- data.frame(pid=c("a","b"), t=c(1,2))
  ev2 <- data.frame(pid=c("a"), t=c(3))
  out2 <- ps_prepare_events(list(admit=ev1, discharge=ev2), id_col="pid", time_col="t")
  expect_equal(unique(out2$event_type), c("admit","discharge"))
  expect_true(all(out2$source_table %in% c("admit","discharge")))
})

test_that("ps_prepare_observations binds groups and preserves vars", {
  bp <- data.frame(pid=c("a","a"), time=c(1,4), sbp=c(120,130), dbp=c(80,85))
  bmp <- data.frame(pid=c("a"), ts=c(2), glucose=c(100))
  tables <- list(bp=bp, bmp=bmp)
  specs <- list(
    bp = list(id_col="pid", time_col="time", vars=c("sbp","dbp"), group="bp"),
    bmp = list(id_col="pid", time_col="ts", vars=c("glucose"), group="bmp")
  )
  out <- ps_prepare_observations(tables, specs)
  expect_s3_class(out, "ps_observations")
  expect_true(all(c("patient_id","time","group","sbp","dbp","glucose") %in% names(out)))
  expect_equal(out$group, c("bp","bmp","bp"))
})
