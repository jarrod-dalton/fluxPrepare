reconstruct_state_at <- function(anchors,
                                   observations,
                                   vars,
                                   id_col = "patient_id",
                                   time_col = "t0",
                                   lookback = Inf,
                                   staleness = Inf,
                                   keep_provenance = TRUE,
                                   row_policy = c("return_all", "drop_incomplete"),
                                   derived_vars = NULL,
                                   derived_provider = NULL,
                                   derived_context = NULL,
                                   derived_on_missing = c("na", "error"),
                                   keep_derived_provenance = FALSE,
                                   count_no_history = c("na", "zero"),
                                   count_vars = NULL,
                                   ctx = NULL) {
  .ps_assert_data_frame(anchors, "anchors")
  .ps_assert_data_frame(observations, "observations")
  .ps_assert_has_cols(anchors, c(id_col, time_col), "anchors")
  .ps_assert_has_cols(observations, c("patient_id", "time"), "observations")

  if (!is.character(vars) || length(vars) < 1L) {
    stop("reconstruct_state_at(): `vars` must be a non-empty character vector.", call. = FALSE)
  }
  .ps_assert_has_cols(observations, vars, "observations")

  a <- anchors[, c(id_col, time_col)]
  names(a) <- c("patient_id", "t0")
  a$patient_id <- as.character(a$patient_id)
  time_spec <- NULL
  if (inherits(a$t0, "Date") || inherits(a$t0, "POSIXt")) {
    time_spec <- .ps_time_spec_or_stop(ctx, "reconstruct_state_at")
  }
  a$t0 <- .ps_coerce_time_numeric(a$t0, time_spec, "anchors$t0")
  .ps_assert_time_numeric(a$t0, "anchors$t0")

  if (anyNA(a$patient_id) || any(a$patient_id == "")) {
    stop("reconstruct_state_at(): anchors patient_id contains missing/empty values.", call. = FALSE)
  }

  obs <- observations
  obs$patient_id <- as.character(obs$patient_id)
    # observations should already be numeric from prepare_observations(),
  # but allow Date/POSIXct here when ctx is provided.
  if (is.null(time_spec) && (inherits(obs$time, "Date") || inherits(obs$time, "POSIXt"))) {
    time_spec <- .ps_time_spec_or_stop(ctx, "reconstruct_state_at")
  }
  obs$time <- .ps_coerce_time_numeric(obs$time, time_spec, "observations$time")
  .ps_assert_time_numeric(obs$time, "observations$time")

  if (!is.numeric(lookback) || length(lookback) != 1L) {
    stop("reconstruct_state_at(): `lookback` must be a single numeric value.", call. = FALSE)
  }
  if (is.finite(lookback) && lookback < 0) {
    stop("reconstruct_state_at(): `lookback` must be >= 0.", call. = FALSE)
  }

  stal <- .ps_norm_named_numeric(staleness, vars, "staleness")

  # deterministic: observations are expected sorted by (patient_id, time, group)
  obs_by_pid <- split(obs, obs$patient_id)

  out <- a
  for (v in vars) out[[v]] <- NA

  if (isTRUE(keep_provenance)) {
    for (v in vars) {
      out[[paste0(".time_", v)]] <- NA_real_
      out[[paste0(".prov_", v)]] <- "missing"
    }
  }

  for (i in seq_len(nrow(a))) {
    pid <- a$patient_id[[i]]
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
  derived_on_missing <- match.arg(derived_on_missing)
  count_no_history <- match.arg(count_no_history)

  if (!is.null(derived_vars)) {
    if (!is.character(derived_vars) || length(derived_vars) < 1L) {
      stop("reconstruct_state_at(): derived_vars must be NULL or a non-empty character vector.", call. = FALSE)
    }
    derived_vars <- unique(as.character(derived_vars))
    if (is.null(derived_provider)) {
      stop("reconstruct_state_at(): derived_provider must be provided when derived_vars is not NULL.", call. = FALSE)
    }
    if (is.null(derived_context)) {
      derived_context <- list(observations = observations)
    } else {
      if (!is.list(derived_context)) stop("reconstruct_state_at(): derived_context must be NULL or a list.", call. = FALSE)
      if (is.null(derived_context$observations)) derived_context$observations <- observations
    }
  }
  # Phase 6: optionally add derived variables at anchors via provider (Core re-use).
  if (!is.null(derived_vars)) {
    out <- add_derived_at(
      state_at = out,
      anchors = out[, c("patient_id", "t0"), drop = FALSE],
      derived_vars = derived_vars,
      provider = derived_provider,
      context = derived_context,
      derived_on_missing = derived_on_missing,
      keep_derived_provenance = keep_derived_provenance,
      count_no_history = count_no_history,
      count_vars = count_vars
    )
  }

  if (row_policy == "drop_incomplete") {
    needed <- c(vars, if (!is.null(derived_vars)) derived_vars else character(0))
    needed <- unique(needed)
    keep <- stats::complete.cases(out[, needed, drop = FALSE])
    out <- out[keep, , drop = FALSE]
  }

  rownames(out) <- NULL
  class(out) <- c("ps_state_asof", class(out))
  attr(out, "vars") <- vars
  attr(out, "lookback") <- lookback
  attr(out, "staleness") <- stal
  out
}

.ps_norm_named_numeric <- function(x, vars, context) {
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
