#' Reconstruct as-of state at anchor times
#'
#' Reconstructs requested variables as the most recent observed non-missing values at or before each anchor time, subject to lookback and staleness constraints.
#'
#' @param anchors A data.frame with entity id and anchor time.
#' @param observations Canonical observation store from prepare_observations().
#' @param vars Character vector of variable names to reconstruct.
#' @param id_col Entity id column in anchors.
#' @param time_col Anchor time column in anchors.
#' @param keep_anchor_cols Logical; if TRUE, preserve non-id/non-time anchor columns in the output.
#' @param lookback Numeric lookback window.
#' @param staleness Numeric maximum age; scalar or named per-variable vector.
#' @param keep_provenance If TRUE, add per-variable provenance columns.
#' @param row_policy One of "return_all" or "drop_incomplete".
#' @param time_spec Optional fluxCore time_spec object used when anchor/observation times are Date/POSIXct.
#'
#' @return A data.frame with reconstructed values at anchors and optional provenance columns.
#'
#' @export
reconstruct_state_at <- function(anchors,
                                   observations,
                                   vars,
                                   id_col = "entity_id",
                                   time_col = "t0",
                                   keep_anchor_cols = FALSE,
                                   lookback = Inf,
                                   staleness = Inf,
                                   keep_provenance = TRUE,
                                   row_policy = c("return_all", "drop_incomplete"),
                                   time_spec = NULL) {
  .flux_assert_data_frame(anchors, "anchors")
  .flux_assert_data_frame(observations, "observations")
  .flux_assert_has_cols(anchors, c(id_col, time_col), "anchors")
  .flux_assert_has_cols(observations, c("entity_id", "time"), "observations")

  if (!is.logical(keep_anchor_cols) || length(keep_anchor_cols) != 1L || is.na(keep_anchor_cols)) {
    stop("reconstruct_state_at(): keep_anchor_cols must be TRUE/FALSE.", call. = FALSE)
  }
  keep_anchor_cols <- isTRUE(keep_anchor_cols)

  if (!is.character(vars) || length(vars) < 1L) {
    stop("reconstruct_state_at(): `vars` must be a non-empty character vector.", call. = FALSE)
  }
  .flux_assert_has_cols(observations, vars, "observations")

  anchor_extra <- NULL
  a <- anchors[, c(id_col, time_col)]
  names(a) <- c("entity_id", "t0")
  if (isTRUE(keep_anchor_cols)) {
    extra_cols <- setdiff(names(anchors), c(id_col, time_col))
    anchor_extra <- anchors[, extra_cols, drop = FALSE]
  }
  a$entity_id <- as.character(a$entity_id)
  resolved_time_spec <- time_spec
  if (inherits(a$t0, "Date") || inherits(a$t0, "POSIXt")) {
    resolved_time_spec <- .resolve_time_spec(resolved_time_spec, "reconstruct_state_at")
  }
  a$t0 <- .flux_coerce_time_numeric(a$t0, resolved_time_spec, "anchors$t0")
  .flux_assert_time_numeric(a$t0, "anchors$t0")

  if (anyNA(a$entity_id) || any(a$entity_id == "")) {
    stop("reconstruct_state_at(): anchors entity_id contains missing/empty values.", call. = FALSE)
  }

  obs <- observations
  obs$entity_id <- as.character(obs$entity_id)
    # observations should already be numeric from prepare_observations(),
  # but allow Date/POSIXct here when time_spec is provided.
  if (is.null(resolved_time_spec) && (inherits(obs$time, "Date") || inherits(obs$time, "POSIXt"))) {
    resolved_time_spec <- .resolve_time_spec(resolved_time_spec, "reconstruct_state_at")
  }
  obs$time <- .flux_coerce_time_numeric(obs$time, resolved_time_spec, "observations$time")
  .flux_assert_time_numeric(obs$time, "observations$time")

  if (!is.numeric(lookback) || length(lookback) != 1L) {
    stop("reconstruct_state_at(): `lookback` must be a single numeric value.", call. = FALSE)
  }
  if (is.finite(lookback) && lookback < 0) {
    stop("reconstruct_state_at(): `lookback` must be >= 0.", call. = FALSE)
  }

  stal <- .flux_norm_named_numeric(staleness, vars, "staleness")

  # deterministic: observations are expected sorted by (entity_id, time, group)
  obs_by_pid <- split(obs, obs$entity_id)

  out <- a
  if (isTRUE(keep_anchor_cols) && !is.null(anchor_extra) && ncol(anchor_extra) > 0L) {
    out <- cbind(out, anchor_extra, stringsAsFactors = FALSE)
  }
  for (v in vars) out[[v]] <- NA

  if (isTRUE(keep_provenance)) {
    for (v in vars) {
      out[[paste0(".time_", v)]] <- NA_real_
      out[[paste0(".prov_", v)]] <- "missing"
    }
  }

  for (i in seq_len(nrow(a))) {
    pid <- a$entity_id[[i]]
    t0 <- a$t0[[i]]

    o <- obs_by_pid[[pid]]
    if (is.null(o) || nrow(o) == 0L) next

    elig <- o$time <= t0
    if (is.finite(lookback)) elig <- elig & (o$time >= (t0 - lookback))
    if (!any(elig)) next
    oo <- o[elig, , drop = FALSE]

    for (v in vars) {
      vv <- oo[[v]]
      ok <- !is.na(vv)
      if (!any(ok)) next

      tt <- oo$time[ok]
      t_last <- max(tt)

      max_age <- stal[[v]]
      if (is.finite(max_age) && (t0 - t_last) > max_age) {
        next
      }

      # if multiple rows at t_last, take last for determinism
      cand <- which(ok & oo$time == t_last)
      k <- cand[[length(cand)]]
      out[[v]][[i]] <- vv[[k]]

      if (isTRUE(keep_provenance)) {
        out[[paste0(".time_", v)]][[i]] <- t_last
        out[[paste0(".prov_", v)]][[i]] <- if (isTRUE(all.equal(t_last, t0))) "observed" else "carried_forward"
      }
    }
  }

  row_policy <- match.arg(row_policy)

  if (row_policy == "drop_incomplete") {
    keep <- stats::complete.cases(out[, vars, drop = FALSE])
    out <- out[keep, , drop = FALSE]
  }

  rownames(out) <- NULL
  class(out) <- c("flux_state_asof", class(out))
  attr(out, "vars") <- vars
  attr(out, "lookback") <- lookback
  attr(out, "staleness") <- stal
  out
}

.flux_norm_named_numeric <- function(x, vars, context) {
  if (is.null(x) || (length(x) == 1L && isTRUE(is.infinite(x)))) {
    return(stats::setNames(rep(Inf, length(vars)), vars))
  }
  if (is.numeric(x) && length(x) == 1L) {
    if (is.na(x) || x < 0) stop(sprintf("reconstruct_state_at(): `%s` must be >= 0 or Inf.", context), call. = FALSE)
    return(stats::setNames(rep(as.numeric(x), length(vars)), vars))
  }
  if (is.numeric(x) && !is.null(names(x)) && all(names(x) != "")) {
    miss <- setdiff(vars, names(x))
    if (length(miss) > 0) {
      stop(sprintf("reconstruct_state_at(): `%s` missing entries for vars: %s", context, paste0(miss, collapse = ", ")),
           call. = FALSE)
    }
    bad <- is.na(x[vars]) | x[vars] < 0
    if (any(bad)) {
      stop(sprintf("reconstruct_state_at(): `%s` contains NA/negative values.", context), call. = FALSE)
    }
    return(stats::setNames(as.numeric(x[vars]), vars))
  }
  stop(sprintf("reconstruct_state_at(): `%s` must be a single numeric value or a named numeric vector.", context),
       call. = FALSE)
}
