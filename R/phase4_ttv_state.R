build_ttv_state <- function(observations,
                               splits,
                               ctx = NULL,
                               outcome_group,
                               outcome_vars,
                               predictor_vars,
                               followup = NULL,
                               fu_start_col = "followup_start",
                               fu_end_col = "followup_end",
                               death_col = NULL,
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
                               max_intervals_per_entity = NULL,
                               seed = NULL) {
  row_policy <- match.arg(row_policy)
  derived_on_missing <- match.arg(derived_on_missing)
  count_no_history <- match.arg(count_no_history)

  .flux_assert_data_frame(observations, "observations")
  .flux_assert_data_frame(splits, "splits")
  .flux_assert_has_cols(observations, c("entity_id", "time", "group"), "observations")
  .flux_assert_has_cols(splits, c("entity_id", "split"), "splits")

  if (!is.character(outcome_group) || length(outcome_group) != 1L || is.na(outcome_group) || trimws(outcome_group) == "") {
    stop("build_ttv_state(): outcome_group must be a non-empty character scalar.", call. = FALSE)
  }
  outcome_group <- as.character(outcome_group)

  if (!is.character(outcome_vars) || length(outcome_vars) < 1L) {
    stop("build_ttv_state(): outcome_vars must be a non-empty character vector.", call. = FALSE)
  }
  outcome_vars <- unique(as.character(outcome_vars))

  if (!is.character(predictor_vars) || length(predictor_vars) < 1L) {
    stop("build_ttv_state(): predictor_vars must be a non-empty character vector.", call. = FALSE)
  }
  predictor_vars <- unique(as.character(predictor_vars))

  .flux_assert_has_cols(observations, outcome_vars, "observations")
  .flux_assert_has_cols(observations, predictor_vars, "observations")

  if (!is.null(max_intervals_per_entity)) {
    if (!is.numeric(max_intervals_per_entity) || length(max_intervals_per_entity) != 1L || is.na(max_intervals_per_entity)) {
      stop("build_ttv_state(): max_intervals_per_entity must be NULL or a single positive integer.", call. = FALSE)
    }
    max_intervals_per_entity <- as.integer(max_intervals_per_entity)
    if (max_intervals_per_entity < 1L) {
      stop("build_ttv_state(): max_intervals_per_entity must be >= 1.", call. = FALSE)
    }
  }
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) {
      stop("build_ttv_state(): seed must be NULL or a single integer.", call. = FALSE)
    }
    seed <- as.integer(seed)
  }

  # Normalize splits
  splits <- splits[, c("entity_id", "split"), drop = FALSE]
  splits$entity_id <- as.character(splits$entity_id)
  splits$split <- as.character(splits$split)

  # Normalize observations
  obs <- observations
  obs$entity_id <- as.character(obs$entity_id)
  obs$group <- as.character(obs$group)
  obs$time <- .flux_coerce_time_numeric(obs$time)
  .flux_assert_time_numeric(obs$time, "build_ttv_state(): observations$time")

  if (anyNA(obs$entity_id) || any(obs$entity_id == "")) {
    stop("build_ttv_state(): observations$entity_id contains missing/empty values.", call. = FALSE)
  }
  if (anyNA(obs$group) || any(obs$group == "")) {
    stop("build_ttv_state(): observations$group contains missing/empty values.", call. = FALSE)
  }

  # Restrict to entities in splits
  pats <- splits$entity_id
  obs <- obs[obs$entity_id %in% pats, , drop = FALSE]

  # Ensure deterministic ordering
  ord <- order(obs$entity_id, obs$time, obs$group)
  obs <- obs[ord, , drop = FALSE]
  rownames(obs) <- NULL

  # Follow-up (optional)
  fu <- .flux_prepare_followup(followup, splits, fu_start_col, fu_end_col, death_col, ctx, "build_ttv_state")

  # Build consecutive (t0, t1) intervals from the outcome group observation times
  obs_out <- obs[obs$group == outcome_group, c("entity_id", "time", outcome_vars), drop = FALSE]

  if (nrow(obs_out) == 0L) {
    stop(sprintf("build_ttv_state(): no observations found for outcome_group '%s'.", outcome_group), call. = FALSE)
  }

  # Split outcome observations by entity
  by_pid <- split(obs_out, obs_out$entity_id)

  interval_rows <- vector("list", length(by_pid))
  names(interval_rows) <- names(by_pid)

  for (pid in names(by_pid)) {
    d <- by_pid[[pid]]
    if (nrow(d) < 2L) next

    times <- d$time
    # Consecutive pairs
    t0 <- times[-length(times)]
    t1_obs <- times[-1L]

    # Keep a row index for the t1 observation within d so we can pull outcome vars.
    # When there are multiple rows at the same time, we take the last occurrence for determinism.
    t1_idx <- integer(length(t1_obs))
    for (k in seq_along(t1_obs)) {
      cand <- which(d$time == t1_obs[[k]])
      t1_idx[[k]] <- cand[[length(cand)]]
    }

    interval_rows[[pid]] <- data.frame(
      entity_id = rep(pid, length(t0)),
      t0 = as.numeric(t0),
      t1_obs = as.numeric(t1_obs),
      t1_row = as.integer(t1_idx),
      stringsAsFactors = FALSE
    )
  }

  intervals <- do.call(rbind, interval_rows)
  if (is.null(intervals) || nrow(intervals) == 0L) {
    stop(sprintf("build_ttv_state(): insufficient observations to form intervals for outcome_group '%s' (need >=2 per entity).", outcome_group),
         call. = FALSE)
  }
  rownames(intervals) <- NULL

  # Apply follow-up censoring and eligibility
  if (!is.null(fu)) {
    # Join followup columns onto intervals via entity_id (splits order ensures all entities exist)
    fu_map <- fu
    idx <- match(intervals$entity_id, fu_map$entity_id)
    if (anyNA(idx)) {
      stop("build_ttv_state(): internal error matching follow-up to intervals.", call. = FALSE)
    }

    fu_start <- fu_map$fu_start[idx]
    fu_end <- fu_map$fu_end[idx]
    censor_time <- fu_map$censor_time[idx]
    death_time <- fu_map$death_time[idx]

    # Keep intervals with t0 within follow-up and before censoring
    keep <- (intervals$t0 >= fu_start) & (intervals$t0 < censor_time)
    intervals <- intervals[keep, , drop = FALSE]
    fu_start <- fu_start[keep]
    fu_end <- fu_end[keep]
    censor_time <- censor_time[keep]
    death_time <- death_time[keep]

    if (nrow(intervals) == 0L) {
      stop("build_ttv_state(): no intervals remain after applying follow-up eligibility rules.", call. = FALSE)
    }

    # Censor interval end
    t1_censor <- pmin(intervals$t1_obs, censor_time)
    censored <- intervals$t1_obs > t1_censor

    # End type
    end_type <- rep("observed", length(t1_censor))
    if (any(censored)) {
      end_type[censored] <- "followup_end"
      # If death_time exists and determines the censor time, label as death
      has_death <- !is.na(death_time)
      if (any(has_death & censored)) {
        is_death_end <- has_death & censored & (abs(censor_time - death_time) < 1e-12) & (death_time <= fu_end)
        end_type[is_death_end] <- "death"
      }
    }

    intervals$t1 <- as.numeric(t1_censor)
    intervals$censored <- as.logical(censored)
    intervals$end_type <- as.character(end_type)
  } else {
    intervals$t1 <- as.numeric(intervals$t1_obs)
    intervals$censored <- FALSE
    intervals$end_type <- "observed"
  }

  # Ensure valid positive-length intervals
  keep2 <- intervals$t1 > intervals$t0
  intervals <- intervals[keep2, , drop = FALSE]
  if (nrow(intervals) == 0L) {
    stop("build_ttv_state(): no positive-length intervals remain after censoring.", call. = FALSE)
  }

  # Optional sampling per entity
  if (!is.null(max_intervals_per_entity)) {
    if (!is.null(seed)) set.seed(seed)
    split_int <- split(seq_len(nrow(intervals)), intervals$entity_id)
    keep_idx <- integer(0)
    for (pid in names(split_int)) {
      idxs <- split_int[[pid]]
      if (length(idxs) <= max_intervals_per_entity) {
        keep_idx <- c(keep_idx, idxs)
      } else {
        keep_idx <- c(keep_idx, sample(idxs, size = max_intervals_per_entity, replace = FALSE))
      }
    }
    keep_idx <- sort(keep_idx)
    intervals <- intervals[keep_idx, , drop = FALSE]
    rownames(intervals) <- NULL
  }

  # Reconstruct predictors at t0
  anchors <- data.frame(entity_id = intervals$entity_id, t0 = intervals$t0, stringsAsFactors = FALSE)
  x <- reconstruct_state_at(
    anchors = anchors,
    observations = obs,
    vars = predictor_vars,
    id_col = "entity_id",
    time_col = "t0",
    lookback = lookback,
    staleness = staleness,
    keep_provenance = keep_provenance
  )

  # Pull outcomes at the observed (uncensored) t1 time. For censored rows, outcomes are NA.
  y_mat <- matrix(NA, nrow = nrow(intervals), ncol = length(outcome_vars))
  colnames(y_mat) <- outcome_vars

  # Build per-entity lookup for pulling t1 outcomes
  by_pid_full <- split(obs_out, obs_out$entity_id)

  for (i in seq_len(nrow(intervals))) {
    if (isTRUE(intervals$censored[[i]])) next

    pid <- intervals$entity_id[[i]]
    t1_obs <- intervals$t1_obs[[i]]

    d <- by_pid_full[[pid]]
    if (is.null(d) || nrow(d) == 0L) next

    # Use last occurrence at t1_obs for determinism
    cand <- which(d$time == t1_obs)
    if (length(cand) == 0L) next
    k <- cand[[length(cand)]]

    for (j in seq_along(outcome_vars)) {
      v <- outcome_vars[[j]]
      y_mat[i, j] <- d[[v]][[k]]
    }
  }

  y <- as.data.frame(y_mat, stringsAsFactors = FALSE)

  # Assemble output
  out <- data.frame(
    entity_id = intervals$entity_id,
    stringsAsFactors = FALSE
  )

  # Join split
  split_idx <- match(out$entity_id, splits$entity_id)
  if (anyNA(split_idx)) {
    stop("build_ttv_state(): internal error matching splits to output.", call. = FALSE)
  }
  out$split <- splits$split[split_idx]

  out$t0 <- as.numeric(intervals$t0)
  out$t1 <- as.numeric(intervals$t1)
  out$deltat <- as.numeric(intervals$t1 - intervals$t0)
  out$censored <- as.logical(intervals$censored)
  out$end_type <- as.character(intervals$end_type)

  # Predictors
  x_keep <- setdiff(names(x), c("entity_id", "t0"))
  out <- cbind(out, x[, x_keep, drop = FALSE])

  # Outcomes
  out <- cbind(out, y)

  # Ensure deterministic unique column names when predictors and outcomes overlap
  # (e.g., predictor sbp at t0 and outcome sbp at t1). Base cbind/data.frame may
  # allow duplicate names; tests and downstream use expect make.unique-style names.
  names(out) <- make.unique(names(out), sep = ".")

  rownames(out) <- NULL

  # Attach spec + metadata
  attr(out, "spec") <- list(
    task = "state",
    outcome_group = outcome_group,
    outcome_vars = outcome_vars,
    predictor_vars = predictor_vars,
    followup = !is.null(followup),
    fu_start_col = fu_start_col,
    fu_end_col = fu_end_col,
    death_col = death_col,
    lookback = lookback,
    staleness = staleness,
    keep_provenance = keep_provenance,
    max_intervals_per_entity = max_intervals_per_entity,
    seed = seed
  )

  meta <- list(
    n_rows = nrow(out),
    n_entities = length(unique(out$entity_id)),
    split_counts = as.list(table(out$split)),
    censor_rate = mean(out$censored),
    built_with = "build_ttv_state"
  )
  attr(out, "metadata") <- meta

  class(out) <- c("flux_ttv_state", class(out))
  out
}
