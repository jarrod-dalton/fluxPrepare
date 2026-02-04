# ------------------------------------------------------------------------------
# Phase 6: Derived-variable integration (Core re-use via provider)
# ------------------------------------------------------------------------------

#' Create a patientSimCore-backed derived-variable provider
#'
#' This provider evaluates patientSimCore-style derived variable functions
#' (f(patient, j, t) -> scalar or NULL) at anchor times by reconstructing a
#' minimal Patient history from the provided observation/event context.
#'
#' @param schema A patientSimCore schema (named list) used to initialize Patient objects.
#' @param derived_var_fns Named list of derived variable functions compatible with patientSimCore.
#' @return A provider object (list) with a `$compute()` method.
#' @export
core_derived_provider <- function(schema, derived_var_fns) {
  if (!is.list(schema) || is.null(names(schema))) {
    stop("core_derived_provider(): `schema` must be a named list (patientSimCore schema).", call. = FALSE)
  }
  if (!is.list(derived_var_fns) || length(derived_var_fns) == 0L) {
    stop("core_derived_provider(): `derived_var_fns` must be a non-empty named list of functions.", call. = FALSE)
  }
  nms <- names(derived_var_fns)
  if (is.null(nms) || any(nms == "")) {
    stop("core_derived_provider(): `derived_var_fns` must have non-empty names.", call. = FALSE)
  }
  for (nm in nms) {
    if (!is.function(derived_var_fns[[nm]])) {
      stop(sprintf("core_derived_provider(): derived_var_fns[['%s']] must be a function.", nm), call. = FALSE)
    }
  }

  provider <- list(
    name = "patientSimCore",
    schema = schema,
    derived_var_fns = derived_var_fns
  )

  provider$compute <- function(state_at, anchors, derived_vars, context = list()) {
    # state_at: data.frame with patient_id,t0 and reconstructed base vars (not strictly required here)
    .ps_assert_data_frame(anchors, "anchors")
    .ps_assert_has_cols(anchors, c("patient_id", "t0"), "anchors")
    anchors$patient_id <- as.character(anchors$patient_id)
    anchors$t0 <- .ps_coerce_time_numeric(anchors$t0)

    if (!is.character(derived_vars) || length(derived_vars) < 1L) {
      stop("provider$compute(): `derived_vars` must be a non-empty character vector.", call. = FALSE)
    }
    derived_vars <- unique(as.character(derived_vars))

    # require patientSimCore at runtime (Suggests)
    if (!requireNamespace("patientSimCore", quietly = TRUE)) {
      stop("core_derived_provider requires package 'patientSimCore' to be installed.", call. = FALSE)
    }

    obs <- context$observations
    if (is.null(obs)) stop("provider$compute(): context$observations is required.", call. = FALSE)
    .ps_assert_data_frame(obs, "context$observations")
    .ps_assert_has_cols(obs, c("patient_id", "time", "group"), "context$observations")
    obs$patient_id <- as.character(obs$patient_id)
    obs$time <- .ps_coerce_time_numeric(obs$time)

    ev <- context$events
    if (!is.null(ev)) {
      .ps_assert_data_frame(ev, "context$events")
      .ps_assert_has_cols(ev, c("patient_id", "time", "event_type"), "context$events")
      ev$patient_id <- as.character(ev$patient_id)
      ev$time <- .ps_coerce_time_numeric(ev$time)
      ev$event_type <- as.character(ev$event_type)
    }

    # Limit derived functions to those provided
    fns_all <- provider$derived_var_fns
    missing_fns <- setdiff(derived_vars, names(fns_all))
    if (length(missing_fns) > 0L) {
      stop(sprintf("provider$compute(): requested derived vars not in provider: %s",
                   paste(missing_fns, collapse = ", ")), call. = FALSE)
    }
    fns <- fns_all[derived_vars]

    # helper: build a minimal Patient from obs/events for one patient
    build_patient <- function(pid, t0) {
      # IMPORTANT: include anchor-time (time == t0) rows in the reconstructed
      # Patient history. Whether anchor-time observations contribute to any
      # specific derived variable is controlled by the derived function itself
      # (e.g., include_current = TRUE/FALSE).
      obs_i <- obs[obs$patient_id == pid & obs$time <= t0, , drop = FALSE]
      ev_i <- if (!is.null(ev)) ev[ev$patient_id == pid & ev$time <= t0, , drop = FALSE] else NULL

      times <- c(obs_i$time, if (!is.null(ev_i)) ev_i$time, t0)
      times <- times[is.finite(times)]
      time0 <- if (length(times) > 0L) min(times) else t0

      p <- patientSimCore::new_patient(init = list(), schema = provider$schema, time0 = time0)
      p$derived_vars <- fns
      p$id <- pid
      # Build observation change map: for each time, merge all rows' non-NA changes
      change_cols <- setdiff(colnames(obs_i), c("patient_id", "time", "group"))
      if (length(change_cols) > 0L && nrow(obs_i) > 0L) {
        # order for deterministic merge
        obs_i <- obs_i[order(obs_i$time, obs_i$group), , drop = FALSE]
      }

      # times to iterate over: union of obs times and event times (anchors do not add events)
      run_times <- sort(unique(c(obs_i$time, if (!is.null(ev_i)) ev_i$time)))
      for (tt in run_times) {
        # apply any events at this time
        if (!is.null(ev_i)) {
          e_tt <- ev_i[ev_i$time == tt, , drop = FALSE]
          if (nrow(e_tt) > 0L) {
            for (k in seq_len(nrow(e_tt))) {
              p$update(time = tt, event_type = e_tt$event_type[k], changes = NULL)
            }
          }
        }

        # apply observation changes at this time
        o_tt <- obs_i[obs_i$time == tt, , drop = FALSE]
        if (nrow(o_tt) > 0L && length(change_cols) > 0L) {
          ch <- list()
          for (cc in change_cols) {
            v <- o_tt[[cc]]
            # take last non-NA value at that time (deterministic due to ordering)
            w <- which(!is.na(v))
            if (length(w) > 0L) {
              idx <- w[length(w)]

              val <- v[idx]
              if (!is.na(val)) ch[[cc]] <- val
            }
          }
          if (length(ch) > 0L) {
            p$update(time = tt, event_type = "OBS", changes = ch)
          } else {
            # still record the observation-time as an event to align intervals
            p$update(time = tt, event_type = "OBS", changes = NULL)
          }
        }
      }

      p
    }

    # compute derived vars at each anchor
    out <- anchors[, c("patient_id", "t0"), drop = FALSE]
    for (dv in derived_vars) out[[dv]] <- NA_real_

    # Do patient-wise for efficiency
    by_pid <- split(seq_len(nrow(anchors)), anchors$patient_id)
    for (pid in names(by_pid)) {
      idx_rows <- by_pid[[pid]]
      for (rr in idx_rows) {
        t0 <- anchors$t0[rr]
        p <- build_patient(pid, t0)
        # snapshot_at_time returns list of base+derived; extract derived
        snap <- p$snapshot_at_time(time = t0)
        for (dv in derived_vars) {
          if (!is.null(snap[[dv]])) {
            out[[dv]][rr] <- as.numeric(snap[[dv]])
          } else {
            out[[dv]][rr] <- NA_real_
          }
        }
      }
    }

    out
  }

  class(provider) <- c("ps_derived_provider", "list")
  provider
}

#' Add derived variables at anchor times
#'
#' @param state_at Data.frame produced by reconstruct_state_at().
#' @param anchors Data.frame with patient_id and t0 (same rows as state_at).
#' @param derived_vars Character vector of derived variable names to compute.
#' @param provider Derived-variable provider (e.g., from core_derived_provider()).
#' @param context List containing at least `observations`, and optionally `events`.
#' @param derived_on_missing "na" (default) to fill with NA when not computable; "error" to stop.
#' @param keep_derived_provenance If TRUE, add `.avail_<var>` columns.
#' @param count_no_history If "zero", replace NA with 0 for variables listed in `count_vars`.
#' @param count_vars Character vector of derived vars to treat as count-like (eligible for NA->0 when requested).
#' @return state_at with derived variables appended (and optional availability flags).
#' @export
add_derived_at <- function(state_at,
                              anchors,
                              derived_vars,
                              provider,
                              context = list(),
                              derived_on_missing = c("na", "error"),
                              keep_derived_provenance = FALSE,
                              count_no_history = c("na", "zero"),
                              count_vars = NULL) {
  .ps_assert_data_frame(state_at, "state_at")
  .ps_assert_data_frame(anchors, "anchors")
  .ps_assert_has_cols(anchors, c("patient_id", "t0"), "anchors")

  if (!is.character(derived_vars) || length(derived_vars) < 1L) {
    stop("add_derived_at(): derived_vars must be a non-empty character vector.", call. = FALSE)
  }
  derived_vars <- unique(as.character(derived_vars))

  derived_on_missing <- match.arg(derived_on_missing)
  count_no_history <- match.arg(count_no_history)

  if (!is.list(provider) || is.null(provider$compute) || !is.function(provider$compute)) {
    stop("add_derived_at(): provider must be a provider object with a $compute() function.", call. = FALSE)
  }

  if (!is.null(count_vars)) {
    if (!is.character(count_vars)) stop("add_derived_at(): count_vars must be NULL or character.", call. = FALSE)
    count_vars <- unique(as.character(count_vars))
    bad <- setdiff(count_vars, derived_vars)
    if (length(bad) > 0L) stop("add_derived_at(): count_vars must be subset of derived_vars.", call. = FALSE)
  }

  d <- provider$compute(state_at = state_at, anchors = anchors, derived_vars = derived_vars, context = context)

  .ps_assert_data_frame(d, "derived result")
  .ps_assert_has_cols(d, c("patient_id", "t0"), "derived result")

  # Ensure all derived columns exist
  for (dv in derived_vars) {
    if (!dv %in% names(d)) d[[dv]] <- NA_real_
  }
  d <- d[, c("patient_id", "t0", derived_vars), drop = FALSE]

  # Availability flags (computed pre zero-fill)
  avail <- NULL
  if (isTRUE(keep_derived_provenance)) {
    avail <- lapply(derived_vars, function(v) !is.na(d[[v]]))
    names(avail) <- paste0(".avail_", derived_vars)
    avail <- as.data.frame(avail, stringsAsFactors = FALSE)
  }

  if (!is.null(avail)) {
    d <- cbind(d, avail)
  }

  if (derived_on_missing == "error") {
    miss_any <- vapply(derived_vars, function(v) any(is.na(d[[v]])), logical(1))
    if (any(miss_any)) {
      stop(sprintf("add_derived_at(): derived vars not computable for some rows: %s",
                   paste(names(miss_any)[miss_any], collapse = ", ")), call. = FALSE)
    }
  }

  # Optional NA->0 for count-like derived vars
  if (count_no_history == "zero" && !is.null(count_vars) && length(count_vars) > 0L) {
    for (v in count_vars) {
      na_idx <- is.na(d[[v]])
      if (any(na_idx)) d[[v]][na_idx] <- 0L
    }
  }

  # merge back (preserve row order)
  out <- state_at
  key_out <- paste(out$patient_id, out$t0)
  key_d <- paste(d$patient_id, d$t0)
  mi <- match(key_out, key_d)

  add_cols <- setdiff(names(d), c("patient_id", "t0"))
  for (cc in add_cols) {
    out[[cc]] <- d[[cc]][mi]
  }

  out
}

