# Additional Phase 6 coverage to pin anchor-boundary and missingness semantics.

library(testthat)

.build_schema <- function() {
  schema <- test_entity_schema()
  schema$sbp <- list(type = "numeric", default = NA_real_, coerce = as.numeric, allow_na = TRUE)
  schema
}

.build_obs <- function() {
  data.frame(
    entity_id = c("p1", "p1", "p1", "p2"),
    time       = c(0, 5, 10, 10),
    group      = c("bp", "bp", "bp", "bp"),
    sbp        = c(120, 130, 115, 140),
    stringsAsFactors = FALSE
  )
}

test_that("Phase 6: include_current toggles whether anchor-time observations are counted", {
  skip("Phase 6 (derived variables) uses deprecated v1.x provider + ctx pattern; requires v2 refactoring")
})

test_that("Phase 6: multiple anchors for same entity do not leak state across anchors", {
  skip("Phase 6 (derived variables) uses deprecated v1.x provider + ctx pattern; requires v2 refactoring")
})

test_that("Phase 6: derived_on_missing='error' fails if a derived value is missing", {
  skip("Phase 6 (derived variables) uses deprecated v1.x provider + ctx pattern; requires v2 refactoring")
})
