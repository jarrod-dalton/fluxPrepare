build_ttv_event <- function(events,
                               splits,
                               ctx = NULL,
                               event_type,
                               t0_strategy = c("followup_start", "first_event", "fixed"),
                               fixed_t0 = 0,
                               followup = NULL,
                               fu_start_col = "followup_start",
                               fu_end_col = "followup_end",
                               death_col = NULL) {
  .flux_assert_data_frame(events, "events")
  .flux_assert_data_frame(splits, "splits")
  .flux_assert_has_cols(events, c("entity_id", "time", "event_type"), "events")
  .flux_assert_has_cols(splits, c("entity_id", "split"), "splits")

  if (!is.character(event_type) || length(event_type) != 1 || is.na(event_type) || event_type == "") {
    stop("build_ttv_event(): event_type must be a non-empty character scalar.", call. = FALSE)
  }

  t0_strategy <- match.arg(t0_strategy)
  fixed_t0 <- as.numeric(fixed_t0)
  if (!is.finite(fixed_t0)) stop("build_ttv_event(): fixed_t0 must be finite.", call. = FALSE)

  # Normalize splits to character columns
  splits <- splits[, c("entity_id", "split"), drop = FALSE]
  splits$entity_id <- as.character(splits$entity_id)
  splits$split <- as.character(splits$split)

  # Restrict events to known entities in splits
  events <- events[, c("entity_id", "time", "event_type"), drop = FALSE]
  events$entity_id <- as.character(events$entity_id)
  events$event_type <- as.character(events$event_type)
  events$time <- .flux_coerce_time_numeric(events$time)
  .flux_assert_time_numeric(events$time, "build_ttv_event(): events$time")

  if (anyNA(events$event_type) || any(trimws(events$event_type) == "")) {
    stop("build_ttv_event(): events$event_type contains missing/empty values.", call. = FALSE)
  }

  # Compute follow-up start/end if provided
  fu <- .flux_prepare_followup(followup, splits, fu_start_col, fu_end_col, death_col, ctx, "build_ttv_event")

  # Entity universe
  pats <- splits$entity_id

  # Determine t0 per entity
  t0 <- rep(NA_real_, length(pats))
  names(t0) <- pats

  if (t0_strategy == "fixed") {
    t0[] <- fixed_t0
  } else {
    # first observed event time per entity
    # events may not be sorted; compute min time per entity
    first_time <- tapply(events$time, events$entity_id, min)

    if (t0_strategy == "first_event") {
      t0[] <- as.numeric(first_time[pats])
    } else if (t0_strategy == "followup_start") {
      if (!is.null(fu)) {
        t0[] <- fu$fu_start
      } else {
        t0[] <- as.numeric(first_time[pats])
      }
    }
  }

  # Validate t0 exists
  if (anyNA(t0)) {
    bad <- names(t0)[is.na(t0)]
    stop(sprintf("build_ttv_event(): unable to define t0 for %d entity(s) (missing events and/or follow-up). Example(s): %s",
                 length(bad), paste0(utils::head(bad, 10), collapse = ", ")), call. = FALSE)
  }

  # Censoring time
  censor_time <- rep(NA_real_, length(pats))
  names(censor_time) <- pats
  if (!is.null(fu)) {
    censor_time[] <- fu$censor_time
  } else {
    # If no follow-up, censor at max observed event time for entity
    last_time <- tapply(events$time, events$entity_id, max)
    censor_time[] <- as.numeric(last_time[pats])
  }

  if (anyNA(censor_time)) {
    bad <- names(censor_time)[is.na(censor_time)]
    stop(sprintf("build_ttv_event(): unable to define censoring time for %d entity(s). Example(s): %s",
                 length(bad), paste0(utils::head(bad, 10), collapse = ", ")), call. = FALSE)
  }

  # Find next event time after t0 for each entity
  # Approach: filter events to type and time > t0, then take min per entity
  is_target <- events$event_type == event_type
  ev_target <- events[is_target, , drop = FALSE]

  # split by entity (fast enough for phase 2; can optimize later)
  next_time <- rep(NA_real_, length(pats))
  names(next_time) <- pats
  if (nrow(ev_target) > 0) {
    ev_split <- split(ev_target$time, ev_target$entity_id)
    for (pid in intersect(names(ev_split), pats)) {
      tt <- ev_split[[pid]]
      tt <- tt[tt > t0[[pid]]]
      if (length(tt) > 0) next_time[[pid]] <- min(tt)
    }
  }

  # Determine t1 and labels
  event_occurred <- !is.na(next_time) & (next_time <= censor_time)
  t1 <- ifelse(event_occurred, next_time, censor_time)

  # If censor_time < t0, fail loudly (bad follow-up table or time scale)
  if (any(t1 < t0)) {
    bad <- names(t1)[t1 < t0]
    stop(sprintf("build_ttv_event(): found %d entity(s) with t1 < t0 (check follow-up bounds/time scale). Example(s): %s",
                 length(bad), paste0(utils::head(bad, 10), collapse = ", ")), call. = FALSE)
  }

  deltat <- t1 - t0
  time_to_event <- deltat

  out <- data.frame(
    entity_id = pats,
    split = splits$split,
    t0 = as.numeric(t0),
    t1 = as.numeric(t1),
    deltat = as.numeric(deltat),
    event_occurred = as.logical(event_occurred),
    time_to_event = as.numeric(time_to_event),
    censoring_time = as.numeric(censor_time),
    stringsAsFactors = FALSE
  )

  # Attach spec + metadata
  attr(out, "spec") <- list(
    task = "event",
    event_type = event_type,
    t0_strategy = t0_strategy,
    fixed_t0 = fixed_t0,
    followup = !is.null(followup),
    fu_start_col = fu_start_col,
    fu_end_col = fu_end_col,
    death_col = death_col
  )

  meta <- list(
    n_entities = nrow(out),
    split_counts = as.list(table(out$split)),
    event_rate = mean(out$event_occurred),
    built_with = "build_ttv_event"
  )
  attr(out, "metadata") <- meta

  class(out) <- c("flux_ttv_event", class(out))
  out
}


build_ttv_event_process <- function(events,
                                      observations,
                                      splits,
                                      spec,
                                      followup = NULL,
                                      ctx = NULL) {
  .flux_assert_data_frame(events, "events")
  .flux_assert_data_frame(observations, "observations")
  .flux_assert_data_frame(splits, "splits")
  .flux_assert_has_cols(events, c("entity_id", "time", "event_type"), "events")
  .flux_assert_has_cols(observations, c("entity_id", "time", "group"), "observations")
  .flux_assert_has_cols(splits, c("entity_id", "split"), "splits")

  if (!inherits(spec, "spec_event_process")) {
    stop("build_ttv_event_process(): spec must be a spec_event_process object.", call. = FALSE)
  }

  # Normalize splits
  splits <- splits[, c("entity_id", "split"), drop = FALSE]
  splits$entity_id <- as.character(splits$entity_id)
  splits$split <- as.character(splits$split)

  pats <- splits$entity_id

  # Normalize events
  events <- events[, c("entity_id", "time", "event_type"), drop = FALSE]
  events$entity_id <- as.character(events$entity_id)
  events$event_type <- as.character(events$event_type)
  events$time <- .flux_coerce_time_numeric(events$time)
  .flux_assert_time_numeric(events$time, "build_ttv_event_process(): events$time")

  # Normalize observations (keep only required cols + any segmentation vars)
  keep_vars <- character(0)
  if (!is.null(spec$segment_on_vars)) keep_vars <- as.character(spec$segment_on_vars)
  keep_cols <- unique(c("entity_id", "time", "group", keep_vars))
  .flux_assert_has_cols(observations, keep_cols, "observations")
  observations <- observations[, keep_cols, drop = FALSE]
  observations$entity_id <- as.character(observations$entity_id)
  observations$group <- as.character(observations$group)
  observations$time <- .flux_coerce_time_numeric(observations$time)
  .flux_assert_time_numeric(observations$time, "build_ttv_event_process(): observations$time")

  # Compute follow-up
  fu <- .flux_prepare_followup(followup, splits, spec$fu_start_col, spec$fu_end_col, spec$death_col, ctx,
                             "build_ttv_event_process")

  # Determine t0 per entity (same policies as build_ttv_event)
  t0 <- rep(NA_real_, length(pats))
  names(t0) <- pats
  if (spec$t0_strategy == "fixed") {
    t0[] <- as.numeric(spec$fixed_t0)
  } else {
    first_time <- tapply(events$time, events$entity_id, min)
    if (spec$t0_strategy == "first_event") {
      t0[] <- as.numeric(first_time[pats])
    } else if (spec$t0_strategy == "followup_start") {
      if (!is.null(fu)) {
        t0[] <- fu$fu_start
      } else {
        t0[] <- as.numeric(first_time[pats])
      }
    }
  }

  if (anyNA(t0)) {
    bad <- names(t0)[is.na(t0)]
    stop(sprintf("build_ttv_event_process(): unable to define t0 for %d entity(s). Example(s): %s",
                 length(bad), paste0(utils::head(bad, 10), collapse = ", ")), call. = FALSE)
  }

  # Censoring time
  censor_time <- rep(NA_real_, length(pats))
  names(censor_time) <- pats
  if (!is.null(fu)) {
    censor_time[] <- fu$censor_time
  } else {
    last_time <- tapply(events$time, events$entity_id, max)
    censor_time[] <- as.numeric(last_time[pats])
  }
  if (anyNA(censor_time)) {
    bad <- names(censor_time)[is.na(censor_time)]
    stop(sprintf("build_ttv_event_process(): unable to define censoring time for %d entity(s). Example(s): %s",
                 length(bad), paste0(utils::head(bad, 10), collapse = ", ")), call. = FALSE)
  }

  # Identify next event (any cause in process) after t0
  target_types <- as.character(spec$event_types)
  ev_target <- events[events$event_type %in% target_types, , drop = FALSE]

  next_time <- rep(NA_real_, length(pats)); names(next_time) <- pats
  next_type <- rep(NA_character_, length(pats)); names(next_type) <- pats
  if (nrow(ev_target) > 0) {
    ev_split <- split(ev_target, ev_target$entity_id)
    for (pid in intersect(names(ev_split), pats)) {
      df <- ev_split[[pid]]
      df <- df[df$time > t0[[pid]], , drop = FALSE]
      if (nrow(df) > 0) {
        j <- which.min(df$time)
        next_time[[pid]] <- df$time[[j]]
        next_type[[pid]] <- df$event_type[[j]]
      }
    }
  }

  event_before_censor <- !is.na(next_time) & (next_time <= censor_time)
  end_time <- ifelse(event_before_censor, next_time, censor_time)

  if (any(end_time < t0)) {
    bad <- names(end_time)[end_time < t0]
    stop(sprintf("build_ttv_event_process(): found %d entity(s) with t1 < t0 (check follow-up bounds/time scale). Example(s): %s",
                 length(bad), paste0(utils::head(bad, 10), collapse = ", ")), call. = FALSE)
  }

  # Pre-split observations by entity for candidate segmentation times
  split_groups <- spec$split_on_groups
  seg_vars <- spec$segment_on_vars
  seg_rules <- spec$segment_rules


  cand_mode <- if (!is.null(spec$candidate_times)) as.character(spec$candidate_times) else "groups"
  if (!cand_mode %in% c("groups", "vars", "groups_or_vars")) {
    stop("build_ttv_event_process(): spec$candidate_times must be one of 'groups', 'vars', or 'groups_or_vars'.", call. = FALSE)
  }

  # Pre-split observations by entity for efficient candidate extraction
  obs_split_all <- split(observations, observations$entity_id)
  min_dt <- as.numeric(spec$min_dt)

  rows <- vector("list", length(pats))
  names(rows) <- pats

  for (pid in pats) {
    t0p <- t0[[pid]]
    t_end <- end_time[[pid]]

    # candidate segmentation times strictly inside (t0, t_end)
    cand <- numeric(0)
    o <- NULL
    if (pid %in% names(obs_split_all)) o <- obs_split_all[[pid]]
    if (!is.null(o) && nrow(o) > 0) {
      tt <- numeric(0)

      if (cand_mode %in% c("groups", "groups_or_vars") && !is.null(split_groups)) {
        tt_g <- unique(o$time[o$group %in% split_groups])
        tt <- c(tt, tt_g)
      }

      if (cand_mode %in% c("vars", "groups_or_vars")) {
        # times where at least one seg var is observed (non-missing)
        has_any <- rep(FALSE, nrow(o))
        for (v in seg_vars) has_any <- has_any | !is.na(o[[v]])
        tt_v <- unique(o$time[has_any])
        tt <- c(tt, tt_v)
      }

      tt <- unique(tt)
      tt <- tt[tt > t0p & tt < t_end]
      if (length(tt) > 0) cand <- sort(tt)
    }

    kept <- numeric(0)
    last_keep <- t0p
    last_vals <- NULL

    if (!is.null(seg_vars)) {
      # reconstruct as-of values at baseline and all candidate times for meaningful-change checks
      anchors <- data.frame(entity_id = rep(pid, 1 + length(cand)), t0 = c(t0p, cand), stringsAsFactors = FALSE)
      rec <- reconstruct_state_at(anchors = anchors, observations = observations, vars = seg_vars,
                                     keep_provenance = FALSE, ctx = ctx)
      # baseline values
      last_vals <- as.list(rec[1, seg_vars, drop = FALSE])
      rec_map <- rec[-1, , drop = FALSE]
    }

    if (length(cand) > 0) {
      for (j in seq_along(cand)) {
        tt <- cand[[j]]
        # min spacing first (but we still allow baseline initialization for seg vars)
        too_close <- ((tt - last_keep) < min_dt)

        keep_it <- !too_close
        if (!is.null(seg_vars)) {
          keep_it <- FALSE
          cur_row <- rec_map[j, , drop = FALSE]
          for (v in seg_vars) {
            old <- last_vals[[v]][[1]]
            new <- cur_row[[v]][[1]]

            # If baseline is missing (e.g., fixed t0 before first measurement),
            # initialize baseline from the first non-missing value without
            # forcing a new interval boundary.
            if (is.na(old) && !is.na(new)) {
              last_vals[[v]] <- new
              next
            }

            if (!too_close && .flux_is_meaningful_change(old, new, v, seg_rules)) {
              keep_it <- TRUE
              break
            }
          }
        }

        if (isTRUE(keep_it)) {
          kept <- c(kept, tt)
          last_keep <- tt
          if (!is.null(seg_vars)) {
            for (v in seg_vars) last_vals[[v]] <- cur_row[[v]][[1]]
          }
        }
      }
    }

    bounds <- c(t0p, kept, t_end)
    # ensure strictly increasing
    bounds <- unique(bounds)
    bounds <- sort(bounds)

    if (length(bounds) < 2) next

    t0s <- bounds[-length(bounds)]
    t1s <- bounds[-1]
    deltat <- t1s - t0s

    # event occurs only at terminal interval if event_before_censor
    ev_occ <- rep(FALSE, length(t0s))
    ev_type <- rep(NA_character_, length(t0s))
    if (isTRUE(event_before_censor[[pid]])) {
      # terminal interval ends at the event time
      ev_occ[length(ev_occ)] <- TRUE
      ev_type[length(ev_type)] <- next_type[[pid]]
    }

    rows[[pid]] <- data.frame(
      entity_id = pid,
      split = splits$split[splits$entity_id == pid][1],
      t0 = as.numeric(t0s),
      t1 = as.numeric(t1s),
      deltat = as.numeric(deltat),
      event_occurred = as.logical(ev_occ),
      event_type = as.character(ev_type),
      censoring_time = as.numeric(censor_time[[pid]]),
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  attr(out, "spec") <- spec
  meta <- list(
    n_entities = length(unique(out$entity_id)),
    n_rows = nrow(out),
    split_counts = as.list(table(out$split)),
    event_rate_entities = mean(tapply(out$event_occurred, out$entity_id, any))
  )
  attr(out, "metadata") <- meta
  out
}

.flux_is_meaningful_change <- function(old, new, var, rules) {
  # Default: any change among non-missing values
  if (is.null(rules)) {
    if (is.na(old) || is.na(new)) return(FALSE)
    return(!identical(old, new))
  }
  if (!is.list(rules)) {
    stop("build_ttv_event_process(): segment_rules must be a list when provided.", call. = FALSE)
  }

  # flip: treat as categorical/boolean; any change is meaningful
  flip_vars <- rules$flip
  if (!is.null(flip_vars) && var %in% as.character(flip_vars)) {
    if (is.na(old) || is.na(new)) return(FALSE)
    return(!identical(old, new))
  }

  # bins: meaning is a change in bin membership
  bins <- rules$bins
  if (!is.null(bins) && !is.null(bins[[var]])) {
    cuts <- as.numeric(bins[[var]])
    if (length(cuts) < 2L) {
      stop(sprintf("build_ttv_event_process(): segment_rules$bins[['%s']] must have length >= 2.", var), call. = FALSE)
    }
    if (is.na(old) || is.na(new)) return(FALSE)
    b0 <- findInterval(old, vec = cuts, rightmost.closed = TRUE)
    b1 <- findInterval(new, vec = cuts, rightmost.closed = TRUE)
    return(b0 != b1)
  }

  # eps: absolute change threshold
  eps <- rules$eps
  if (!is.null(eps) && !is.null(eps[[var]])) {
    thr <- as.numeric(eps[[var]])
    if (!is.finite(thr) || thr < 0) {
      stop(sprintf("build_ttv_event_process(): segment_rules$eps[['%s']] must be finite and >= 0.", var), call. = FALSE)
    }
    if (is.na(old) || is.na(new)) return(FALSE)
    return(abs(as.numeric(new) - as.numeric(old)) >= thr)
  }

  # rel_eps: relative change threshold
  rel <- rules$rel_eps
  if (!is.null(rel) && !is.null(rel[[var]])) {
    thr <- as.numeric(rel[[var]])
    if (!is.finite(thr) || thr < 0) {
      stop(sprintf("build_ttv_event_process(): segment_rules$rel_eps[['%s']] must be finite and >= 0.", var), call. = FALSE)
    }
    if (is.na(old) || is.na(new)) return(FALSE)
    denom <- max(abs(as.numeric(old)), 1)
    return(abs(as.numeric(new) - as.numeric(old)) / denom >= thr)
  }

  # fallback: any change
  if (is.na(old) || is.na(new)) return(FALSE)
  !identical(old, new)
}

.flux_prepare_followup <- function(followup, splits, fu_start_col, fu_end_col, death_col, ctx, fn_name) {
  if (is.null(followup)) return(NULL)
  .flux_assert_data_frame(followup, "followup")
  cols <- c("entity_id", fu_start_col, fu_end_col)
  .flux_assert_has_cols(followup, cols, "followup")
  if (!is.null(death_col)) .flux_assert_has_cols(followup, c(death_col), "followup")

  fu <- followup[, cols, drop = FALSE]

# Time handling: followup_start/followup_end/death_time may be numeric, Date, or POSIXct.
time_spec <- NULL
if (inherits(fu[[fu_start_col]], "Date") || inherits(fu[[fu_start_col]], "POSIXt") ||
    inherits(fu[[fu_end_col]], "Date") || inherits(fu[[fu_end_col]], "POSIXt") ||
    (!is.null(death_col) && (inherits(followup[[death_col]], "Date") || inherits(followup[[death_col]], "POSIXt")))) {
  time_spec <- .flux_time_spec_or_stop(ctx, fn_name)
}


  fu_time_class_start <- class(fu[[fu_start_col]])[1]
  fu_time_class_end <- class(fu[[fu_end_col]])[1]
  names(fu) <- c("entity_id", "fu_start", "fu_end")
  fu$entity_id <- as.character(fu$entity_id)
  fu$fu_start <- .flux_coerce_time_numeric(fu$fu_start, time_spec, "followup$fu_start")
  if (!fu_time_class_start %in% c("numeric", "integer") || !fu_time_class_end %in% c("numeric", "integer")) {
  }
  fu$fu_end <- .flux_coerce_time_numeric(fu$fu_end, time_spec, "followup$fu_end")

  if (!is.null(death_col)) {
    fu$death_time <- .flux_coerce_time_numeric(followup[[death_col]], time_spec, "followup$death_time")
  } else {
    fu$death_time <- NA_real_
  }

  # Join onto splits order
  idx <- match(splits$entity_id, fu$entity_id)
  if (anyNA(idx)) {
    missing <- splits$entity_id[is.na(idx)]
    stop(sprintf("build_ttv_event(): followup table is missing %d entity(s) from splits. Example(s): %s",
                 length(missing), paste0(utils::head(missing, 10), collapse = ", ")), call. = FALSE)
  }

  fu <- fu[idx, , drop = FALSE]

  # Censoring time: min(fu_end, death_time) when death provided
  censor <- fu$fu_end
  if (any(!is.na(fu$death_time))) {
    censor <- pmin(censor, fu$death_time, na.rm = TRUE)
  }

  if (anyNA(fu$fu_start) || anyNA(fu$fu_end)) {
    stop("build_ttv_event(): followup_start/followup_end contains missing values after coercion.", call. = FALSE)
  }

  out <- data.frame(
    entity_id = fu$entity_id,
    fu_start = fu$fu_start,
    fu_end = fu$fu_end,
    death_time = as.numeric(fu$death_time),
    censor_time = as.numeric(censor),
    stringsAsFactors = FALSE
  )
  out
}
