test_that("ps_reconstruct_state_at performs LOCF with lookback and staleness", {
  bp <- data.frame(pid = c("a", "a"), time = c(1, 4), sbp = c(120, 130), dbp = c(80, 85))
  tables <- list(bp = bp)
  specs <- list(bp = list(id_col = "pid", time_col = "time", vars = c("sbp", "dbp"), group = "bp"))
  obs <- ps_prepare_observations(tables, specs)

  anchors <- data.frame(pid = c("a", "a"), t0 = c(3, 5))

  out <- ps_reconstruct_state_at(anchors, obs, vars = c("sbp", "dbp"), id_col = "pid", time_col = "t0")
  expect_s3_class(out, "ps_state_asof")
  expect_equal(out$sbp, c(120, 130))
  expect_equal(out$.prov_sbp, c("carried_forward", "carried_forward"))

  out2 <- ps_reconstruct_state_at(anchors, obs, vars = c("sbp"), id_col = "pid", time_col = "t0", staleness = 1)
  expect_true(is.na(out2$sbp[1]))
  expect_equal(out2$sbp[2], 130)

  out3 <- ps_reconstruct_state_at(anchors, obs, vars = c("sbp"), id_col = "pid", time_col = "t0", lookback = 1)
  expect_true(is.na(out3$sbp[1]))
  expect_equal(out3$sbp[2], 130)
})
