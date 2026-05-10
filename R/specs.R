#' Construct a validated state-model spec
#'
#' Constructs a classed spec object for one-step state-transition datasets, validated against a fluxCore schema.
#'
#' @param schema A fluxCore schema (named list).
#' @param outcome_group Character scalar defining the observation group for interval anchors.
#' @param outcome_vars Character vector of outcome variables.
#' @param predictor_vars Character vector of predictor variables.
#' @param name Optional human-readable name.
#' @param lookback Lookback window passed to build_ttv_state().
#' @param staleness Staleness window passed to build_ttv_state().
#' @param keep_provenance Logical passed to build_ttv_state().
#' @param row_policy One of "return_all" or "drop_incomplete".
#' @param outcome_prefix Character prefix for outcome column names (default \code{"outcome_"}).
#'   Set to \code{NULL} to use \code{make.unique()} (legacy behavior).
#'
#' @return A spec object with class c("spec_state", "flux_spec").
#'
#' @export
spec_state <- function(schema,
                          outcome_group,
                          outcome_vars,
                          predictor_vars,
                          name = NULL,
                          lookback = Inf,
                          staleness = Inf,
                          keep_provenance = FALSE,
                          row_policy = c("return_all", "drop_incomplete"),
                          outcome_prefix = "outcome_") {

  row_policy <- match.arg(row_policy)

  if (!is.character(outcome_group) || length(outcome_group) != 1L || is.na(outcome_group) || trimws(outcome_group) == "") {
    stop("spec_state(): outcome_group must be a non-empty character scalar.", call. = FALSE)
  }
  outcome_group <- as.character(outcome_group)

  if (!is.character(outcome_vars) || length(outcome_vars) < 1L) {
    stop("spec_state(): outcome_vars must be a non-empty character vector.", call. = FALSE)
  }
  outcome_vars <- unique(as.character(outcome_vars))

  if (!is.character(predictor_vars) || length(predictor_vars) < 1L) {
    stop("spec_state(): predictor_vars must be a non-empty character vector.", call. = FALSE)
  }
  predictor_vars <- unique(as.character(predictor_vars))

  if (!is.null(name)) {
    if (!is.character(name) || length(name) != 1L || is.na(name) || trimws(name) == "") {
      stop("spec_state(): name must be NULL or a non-empty character scalar.", call. = FALSE)
    }
    name <- as.character(name)
  }

  # Validate schema and variables up front (Core is authoritative)
  schema <- fluxCore::schema_validate(schema)
  all_vars <- unique(c(outcome_vars, predictor_vars))
  fluxCore::schema_assert_vars(schema, all_vars)

  if (!is.numeric(lookback) || length(lookback) != 1L || is.na(lookback)) {
    stop("spec_state(): lookback must be a single numeric value (use Inf for no limit).", call. = FALSE)
  }
  lookback <- as.numeric(lookback)

  if (!is.numeric(staleness) || length(staleness) != 1L || is.na(staleness)) {
    stop("spec_state(): staleness must be a single numeric value (use Inf for no limit).", call. = FALSE)
  }
  staleness <- as.numeric(staleness)

  if (!is.logical(keep_provenance) || length(keep_provenance) != 1L || is.na(keep_provenance)) {
    stop("spec_state(): keep_provenance must be TRUE/FALSE.", call. = FALSE)
  }
  keep_provenance <- isTRUE(keep_provenance)

  if (!is.null(outcome_prefix)) {
    if (!is.character(outcome_prefix) || length(outcome_prefix) != 1L || is.na(outcome_prefix)) {
      stop("spec_state(): outcome_prefix must be NULL or a single character string.", call. = FALSE)
    }
  }

  spec <- list(
    name = name,
    task = "state",
    fun = "build_ttv_state",
    schema = schema,
    args = list(
      outcome_group = outcome_group,
      outcome_vars = outcome_vars,
      predictor_vars = predictor_vars,
      lookback = lookback,
      staleness = staleness,
      keep_provenance = keep_provenance,
      row_policy = row_policy,
      outcome_prefix = outcome_prefix
    )
  )

  class(spec) <- c("spec_state", "flux_spec")
  spec
}

#' Construct a validated event-model spec
#'
#' Constructs a classed spec object for one-step event-time datasets.
#'
#' @param event_type Character scalar specifying the event type to model.
#' @param name Optional human-readable name.
#' @param t0_strategy How to define baseline t0.
#' @param fixed_t0 Numeric scalar used when t0_strategy = "fixed".
#' @param fu_start_col Follow-up start column name.
#' @param fu_end_col Follow-up end column name.
#' @param death_col Optional death time column name.
#' @param predictor_vars Optional character vector of predictor variables reconstructed at t0.
#' @param lookback Lookback passed to build_ttv_event() when predictor_vars is supplied.
#' @param staleness Staleness passed to build_ttv_event() when predictor_vars is supplied.
#' @param keep_provenance Logical passed to build_ttv_event() when predictor_vars is supplied.
#'
#' @return A spec object with class c("spec_event", "flux_spec").
#'
#' @export
spec_event <- function(event_type,
                          name = NULL,
                          t0_strategy = c("followup_start", "first_event", "fixed"),
                          fixed_t0 = 0,
                          fu_start_col = "followup_start",
                          fu_end_col = "followup_end",
                          death_col = NULL,
                          predictor_vars = NULL,
                          lookback = Inf,
                          staleness = Inf,
                          keep_provenance = TRUE) {

  if (!is.character(event_type) || length(event_type) != 1L || is.na(event_type) || trimws(event_type) == "") {
    stop("spec_event(): event_type must be a non-empty character scalar.", call. = FALSE)
  }
  event_type <- as.character(event_type)

  if (!is.null(name)) {
    if (!is.character(name) || length(name) != 1L || is.na(name) || trimws(name) == "") {
      stop("spec_event(): name must be NULL or a non-empty character scalar.", call. = FALSE)
    }
    name <- as.character(name)
  }

  t0_strategy <- match.arg(t0_strategy)
  fixed_t0 <- as.numeric(fixed_t0)
  if (!is.finite(fixed_t0)) stop("spec_event(): fixed_t0 must be finite.", call. = FALSE)

  if (!is.character(fu_start_col) || length(fu_start_col) != 1L || is.na(fu_start_col) || trimws(fu_start_col) == "") {
    stop("spec_event(): fu_start_col must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.character(fu_end_col) || length(fu_end_col) != 1L || is.na(fu_end_col) || trimws(fu_end_col) == "") {
    stop("spec_event(): fu_end_col must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.null(death_col)) {
    if (!is.character(death_col) || length(death_col) != 1L || is.na(death_col) || trimws(death_col) == "") {
      stop("spec_event(): death_col must be NULL or a non-empty character scalar.", call. = FALSE)
    }
    death_col <- as.character(death_col)
  }

  if (!is.null(predictor_vars)) {
    if (!is.character(predictor_vars) || length(predictor_vars) < 1L) {
      stop("spec_event(): predictor_vars must be NULL or a non-empty character vector.", call. = FALSE)
    }
    predictor_vars <- unique(as.character(predictor_vars))
  }

  if (!is.numeric(lookback) || length(lookback) != 1L || is.na(lookback)) {
    stop("spec_event(): lookback must be a single numeric value (use Inf for no limit).", call. = FALSE)
  }
  lookback <- as.numeric(lookback)

  if (!is.numeric(staleness) || length(staleness) != 1L || is.na(staleness)) {
    stop("spec_event(): staleness must be a single numeric value (use Inf for no limit).", call. = FALSE)
  }
  staleness <- as.numeric(staleness)

  if (!is.logical(keep_provenance) || length(keep_provenance) != 1L || is.na(keep_provenance)) {
    stop("spec_event(): keep_provenance must be TRUE/FALSE.", call. = FALSE)
  }
  keep_provenance <- isTRUE(keep_provenance)

  spec <- list(
    name = name,
    task = "event",
    fun = "build_ttv_event",
    args = list(
      event_type = event_type,
      t0_strategy = t0_strategy,
      fixed_t0 = fixed_t0,
      fu_start_col = fu_start_col,
      fu_end_col = fu_end_col,
      death_col = death_col,
      predictor_vars = predictor_vars,
      lookback = lookback,
      staleness = staleness,
      keep_provenance = keep_provenance
    )
  )

  class(spec) <- c("spec_event", "flux_spec")
  spec
}

#' Construct a validated decision-model spec
#'
#' Constructs a classed spec object for decision-point datasets built from
#' decision anchors plus reconstructed state at decision time.
#'
#' @param predictor_vars Character vector of predictor variables reconstructed at decision time.
#' @param name Optional human-readable name.
#' @param decision_point_col Decision point identifier column in the decisions table.
#' @param carry_cols Optional additional decision columns to preserve.
#' @param lookback Lookback passed to build_ttv_decision().
#' @param staleness Staleness passed to build_ttv_decision().
#' @param keep_provenance Logical passed to build_ttv_decision().
#' @param row_policy One of "return_all" or "drop_incomplete".
#'
#' @return A spec object with class c("spec_decision", "flux_spec").
#'
#' @export
spec_decision <- function(predictor_vars,
                          name = NULL,
                          decision_point_col = "decision_point_id",
                          carry_cols = NULL,
                          lookback = Inf,
                          staleness = Inf,
                          keep_provenance = TRUE,
                          row_policy = c("return_all", "drop_incomplete")) {
  row_policy <- match.arg(row_policy)

  if (!is.character(predictor_vars) || length(predictor_vars) < 1L) {
    stop("spec_decision(): predictor_vars must be a non-empty character vector.", call. = FALSE)
  }
  predictor_vars <- unique(as.character(predictor_vars))

  if (!is.null(name)) {
    if (!is.character(name) || length(name) != 1L || is.na(name) || trimws(name) == "") {
      stop("spec_decision(): name must be NULL or a non-empty character scalar.", call. = FALSE)
    }
    name <- as.character(name)
  }

  if (!is.character(decision_point_col) || length(decision_point_col) != 1L || is.na(decision_point_col) || trimws(decision_point_col) == "") {
    stop("spec_decision(): decision_point_col must be a non-empty character scalar.", call. = FALSE)
  }
  decision_point_col <- as.character(decision_point_col)

  if (!is.null(carry_cols)) {
    if (!is.character(carry_cols)) stop("spec_decision(): carry_cols must be NULL or a character vector.", call. = FALSE)
    carry_cols <- unique(as.character(carry_cols))
  }

  if (!is.numeric(lookback) || length(lookback) != 1L || is.na(lookback)) {
    stop("spec_decision(): lookback must be a single numeric value (use Inf for no limit).", call. = FALSE)
  }
  lookback <- as.numeric(lookback)

  if (!is.numeric(staleness) || length(staleness) != 1L || is.na(staleness)) {
    stop("spec_decision(): staleness must be a single numeric value (use Inf for no limit).", call. = FALSE)
  }
  staleness <- as.numeric(staleness)

  if (!is.logical(keep_provenance) || length(keep_provenance) != 1L || is.na(keep_provenance)) {
    stop("spec_decision(): keep_provenance must be TRUE/FALSE.", call. = FALSE)
  }
  keep_provenance <- isTRUE(keep_provenance)

  spec <- list(
    name = name,
    task = "decision",
    fun = "build_ttv_decision",
    args = list(
      predictor_vars = predictor_vars,
      decision_point_col = decision_point_col,
      carry_cols = carry_cols,
      lookback = lookback,
      staleness = staleness,
      keep_provenance = keep_provenance,
      row_policy = row_policy
    )
  )

  class(spec) <- c("spec_decision", "flux_spec")
  spec
}

#' Segmentation rules from bin cutpoints
#'
#' Constructs segmentation rules that trigger interval splitting when a variable
#' crosses a specified bin boundary.
#'
#' @param ... Named numeric vectors of strictly increasing cutpoints, one per variable.
#' Endpoints may include -Inf and Inf.
#'
#' @return A list with class "segment_rules" containing $bins.
#'
#' @export
segment_bins <- function(...) {
  bins <- list(...)
  if (length(bins) == 0L) stop("segment_bins(): supply at least one variable.", call. = FALSE)
  for (nm in names(bins)) {
    v <- bins[[nm]]
    if (!is.numeric(v) || length(v) < 2L || any(is.na(v)) || any(is.nan(v))) {
      stop(sprintf("segment_bins(): '%s' must be a numeric vector of length >= 2 (NA/NaN not allowed).", nm), call. = FALSE)
    }
    v <- as.numeric(v)
    # Allow +/-Inf endpoints (e.g., c(-Inf, 160, Inf)), but require strictly increasing cutpoints.
    if (!all(diff(v) > 0)) {
      stop(sprintf("segment_bins(): '%s' must be strictly increasing (e.g., c(-Inf, 160, Inf)).", nm), call. = FALSE)
    }
    bins[[nm]] <- v
  }
  out <- list(bins = bins)
  class(out) <- "segment_rules"
  out
}

#' Segmentation rules from absolute-change thresholds
#'
#' Constructs segmentation rules that trigger interval splitting when absolute
#' change in a variable exceeds the provided threshold.
#'
#' @param ... Named non-negative numeric thresholds, one per variable.
#'
#' @return A list with class "segment_rules" containing $eps.
#'
#' @export
segment_eps <- function(...) {
  eps <- c(...)
  if (length(eps) == 0L) stop("segment_eps(): supply at least one variable.", call. = FALSE)
  if (!is.numeric(eps) || any(!is.finite(eps)) || any(eps < 0)) {
    stop("segment_eps(): all thresholds must be finite and >= 0.", call. = FALSE)
  }
  out <- list(eps = as.list(as.numeric(eps)))
  class(out) <- "segment_rules"
  out
}

#' Segmentation rules from relative-change thresholds
#'
#' Constructs segmentation rules that trigger interval splitting when relative
#' change in a variable exceeds the provided threshold.
#'
#' @param ... Named non-negative numeric relative thresholds, one per variable.
#'
#' @return A list with class "segment_rules" containing $rel_eps.
#'
#' @export
segment_rel_eps <- function(...) {
  rel_eps <- c(...)
  if (length(rel_eps) == 0L) stop("segment_rel_eps(): supply at least one variable.", call. = FALSE)
  if (!is.numeric(rel_eps) || any(!is.finite(rel_eps)) || any(rel_eps < 0)) {
    stop("segment_rel_eps(): all thresholds must be finite and >= 0.", call. = FALSE)
  }
  out <- list(rel_eps = as.list(as.numeric(rel_eps)))
  class(out) <- "segment_rules"
  out
}

#' Segmentation rules from value flips
#'
#' Constructs segmentation rules that trigger interval splitting when a monitored
#' variable changes value between candidate times.
#'
#' @param ... One or more variable names to monitor for value changes.
#'
#' @return A list with class "segment_rules" containing $flip.
#'
#' @export
segment_flip <- function(...) {
  vars <- unlist(list(...), use.names = FALSE)
  if (!is.character(vars) || length(vars) < 1L || anyNA(vars) || any(trimws(vars) == "")) {
    stop("segment_flip(): provide one or more non-empty variable names.", call. = FALSE)
  }
  out <- list(flip = unique(as.character(vars)))
  class(out) <- "segment_rules"
  out
}

#' Combine segmentation rule objects
#'
#' Combines multiple segmentation-rule objects into a single rule set. Named rule
#' entries are merged by variable; flip-variable sets are unioned.
#'
#' @param ... One or more objects inheriting from "segment_rules".
#'
#' @return A merged "segment_rules" object.
#'
#' @export
segment_rules_combine <- function(...) {
  rules <- list(...)
  if (length(rules) == 0L) stop("segment_rules_combine(): provide at least one rules object.", call. = FALSE)
  out <- list()
  for (r in rules) {
    if (!is.list(r) || !inherits(r, "segment_rules")) {
      stop("segment_rules_combine(): all inputs must inherit from 'segment_rules'.", call. = FALSE)
    }
    for (k in names(r)) {
      if (k %in% c("bins")) {
        out[[k]] <- utils::modifyList((if (is.null(out[[k]])) list() else out[[k]]), r[[k]])
      } else if (k %in% c("eps", "rel_eps")) {
        out[[k]] <- utils::modifyList((if (is.null(out[[k]])) list() else out[[k]]), r[[k]])
      } else if (k %in% c("flip")) {
        out[[k]] <- unique(c((if (is.null(out[[k]])) character(0) else out[[k]]), r[[k]]))
      }
    }
  }
  class(out) <- "segment_rules"
  out
}


#' Construct an event-process spec for start-stop TTV building
#'
#' Defines an event process and segmentation policy for constructing counting-process (start-stop) TTV datasets.
#'
#' @param event_types Character vector of event types that constitute the process.
#' @param name Optional human-readable name.
#' @param split_on_groups Optional observation groups that can trigger interval splitting.
#' @param segment_on_vars Optional observation variables used for segmentation.
#' @param segment_rules Optional segmentation rules object/list.
#' @param candidate_times One of "groups", "vars", or "groups_or_vars".
#' @param min_dt Minimum spacing between split times (non-negative numeric scalar).
#' @param t0_strategy How to define t0 for each entity.
#' @param fixed_t0 Numeric scalar used when t0_strategy = "fixed".
#' @param fu_start_col Follow-up start column name.
#' @param fu_end_col Follow-up end column name.
#' @param death_col Optional death time column name.
#' @param predictor_vars Optional character vector of predictor variables to reconstruct at each
#'   interval's t0 via \code{reconstruct_state_at()}. Use \code{"all"} to reconstruct all
#'   non-metadata variables found in observations. When NULL (the default), no reconstruction is
#'   performed and the output contains only interval columns.
#' @param lookback Lookback window for predictor reconstruction (default \code{Inf}).
#' @param staleness Maximum staleness for predictor reconstruction (default \code{Inf}).
#' @param keep_provenance Logical; include reconstruction provenance columns (default \code{FALSE}).
#' @param row_policy One of \code{"return_all"} (default) or \code{"drop_incomplete"}.
#'   When \code{"drop_incomplete"}, rows with any missing predictor values are removed.
#'
#' @return A spec object with class c("spec_event_process", "flux_spec").
#'
#' @export
spec_event_process <- function(event_types,
                                  name = NULL,
                                  split_on_groups = NULL,
                                  segment_on_vars = NULL,
                                  segment_rules = NULL,
                                  candidate_times = c("groups", "vars", "groups_or_vars"),
                                  min_dt = 0,
                                  t0_strategy = c("followup_start", "first_event", "fixed"),
                                  fixed_t0 = 0,
                                  fu_start_col = "followup_start",
                                  fu_end_col = "followup_end",
                                  death_col = NULL,
                                  predictor_vars = NULL,
                                  lookback = Inf,
                                  staleness = Inf,
                                  keep_provenance = FALSE,
                                  row_policy = c("return_all", "drop_incomplete")) {

  if (!is.character(event_types) || length(event_types) < 1L || anyNA(event_types) || any(trimws(event_types) == "")) {
    stop("spec_event_process(): event_types must be a non-empty character vector with no missing/empty values.", call. = FALSE)
  }
  event_types <- as.character(event_types)

  if (!is.null(name)) {
    if (!is.character(name) || length(name) != 1L || is.na(name) || trimws(name) == "") {
      stop("spec_event_process(): name must be NULL or a non-empty character scalar.", call. = FALSE)
    }
    name <- as.character(name)
  }

  if (!is.null(split_on_groups)) {
    if (!is.character(split_on_groups) || length(split_on_groups) < 1L || anyNA(split_on_groups) || any(trimws(split_on_groups) == "")) {
      stop("spec_event_process(): split_on_groups must be NULL or a non-empty character vector with no missing/empty values.", call. = FALSE)
    }
    split_on_groups <- as.character(split_on_groups)
  }

  if (!is.null(segment_on_vars)) {
    if (!is.character(segment_on_vars) || length(segment_on_vars) < 1L || anyNA(segment_on_vars) || any(trimws(segment_on_vars) == "")) {
      stop("spec_event_process(): segment_on_vars must be NULL or a non-empty character vector with no missing/empty values.", call. = FALSE)
    }
    segment_on_vars <- as.character(segment_on_vars)
  }

  if (!is.null(segment_rules)) {
    if (!is.list(segment_rules)) stop("spec_event_process(): segment_rules must be NULL or a list.", call. = FALSE)
  }

  candidate_times <- match.arg(candidate_times)
  if (candidate_times != "groups" && is.null(segment_on_vars)) {
    stop("spec_event_process(): candidate_times='vars' or 'groups_or_vars' requires segment_on_vars.", call. = FALSE)
  }

  min_dt <- as.numeric(min_dt)
  if (!is.finite(min_dt) || min_dt < 0) stop("spec_event_process(): min_dt must be a finite non-negative numeric scalar.", call. = FALSE)

  t0_strategy <- match.arg(t0_strategy)
  fixed_t0 <- as.numeric(fixed_t0)
  if (!is.finite(fixed_t0)) stop("spec_event_process(): fixed_t0 must be finite.", call. = FALSE)

  if (!is.character(fu_start_col) || length(fu_start_col) != 1L || is.na(fu_start_col) || trimws(fu_start_col) == "") {
    stop("spec_event_process(): fu_start_col must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.character(fu_end_col) || length(fu_end_col) != 1L || is.na(fu_end_col) || trimws(fu_end_col) == "") {
    stop("spec_event_process(): fu_end_col must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.null(death_col)) {
    if (!is.character(death_col) || length(death_col) != 1L || is.na(death_col) || trimws(death_col) == "") {
      stop("spec_event_process(): death_col must be NULL or a non-empty character scalar.", call. = FALSE)
    }
    death_col <- as.character(death_col)
  }

  if (!is.null(predictor_vars)) {
    if (identical(predictor_vars, "all")) {
      # "all" is stored as-is; resolved at build time from observation columns
      predictor_vars <- "all"
    } else {
      if (!is.character(predictor_vars) || length(predictor_vars) < 1L) {
        stop("spec_event_process(): predictor_vars must be NULL, \"all\", or a non-empty character vector.", call. = FALSE)
      }
      predictor_vars <- unique(as.character(predictor_vars))
    }
  }

  if (!is.numeric(lookback) || length(lookback) != 1L || is.na(lookback)) {
    stop("spec_event_process(): lookback must be a single numeric value (use Inf for no limit).", call. = FALSE)
  }
  lookback <- as.numeric(lookback)

  if (!is.numeric(staleness) || length(staleness) != 1L || is.na(staleness)) {
    stop("spec_event_process(): staleness must be a single numeric value (use Inf for no limit).", call. = FALSE)
  }
  staleness <- as.numeric(staleness)

  if (!is.logical(keep_provenance) || length(keep_provenance) != 1L || is.na(keep_provenance)) {
    stop("spec_event_process(): keep_provenance must be TRUE/FALSE.", call. = FALSE)
  }
  keep_provenance <- isTRUE(keep_provenance)

  row_policy <- match.arg(row_policy)

  spec <- list(
    task = "event_process",
    name = name,
    event_types = event_types,
    split_on_groups = split_on_groups,
    segment_on_vars = segment_on_vars,
    segment_rules = segment_rules,
    candidate_times = candidate_times,
    min_dt = min_dt,
    t0_strategy = t0_strategy,
    fixed_t0 = fixed_t0,
    fu_start_col = fu_start_col,
    fu_end_col = fu_end_col,
    death_col = death_col,
    predictor_vars = predictor_vars,
    lookback = lookback,
    staleness = staleness,
    keep_provenance = keep_provenance,
    row_policy = row_policy
  )

  class(spec) <- c("spec_event_process", "flux_spec")
  spec
}




#' @noRd
print.spec_state <- function(x, ...) {
  cat("<spec_state>
")
  if (!is.null(x$name) && nzchar(x$name)) cat("name:          ", x$name, "
", sep = "")
  cat("outcome_group:  ", x$outcome_group, "
", sep = "")
  cat("outcome_vars:   ", paste(x$outcome_vars, collapse = ", "), "
", sep = "")
  cat("predictor_vars: ", paste(x$predictor_vars, collapse = ", "), "
", sep = "")
  if (!is.null(x$lookback)) cat("lookback:       ", x$lookback, "
", sep = "")
  if (!is.null(x$staleness)) cat("staleness:      ", x$staleness, "
", sep = "")
  invisible(x)
}

#' @noRd
print.spec_event <- function(x, ...) {
  cat("<spec_event>
")
  if (!is.null(x$name) && nzchar(x$name)) cat("name:       ", x$name, "
", sep = "")
  cat("event_type: ", x$event_type, "
", sep = "")
  invisible(x)
}

#' @noRd
print.spec_decision <- function(x, ...) {
  cat("<spec_decision>\n")
  if (!is.null(x$name) && nzchar(x$name)) cat("name:              ", x$name, "\n", sep = "")
  cat("predictor_vars:    ", paste(x$args$predictor_vars, collapse = ", "), "\n", sep = "")
  cat("decision_point_col:", x$args$decision_point_col, "\n", sep = "")
  invisible(x)
}

#' @noRd
print.spec_event_process <- function(x, ...) {
  cat("<spec_event_process>\n")
  if (!is.null(x$name) && nzchar(x$name)) cat("name:           ", x$name, "\n", sep = "")
  cat("event_types:     ", paste(x$event_types, collapse = ", "), "\n", sep = "")
  if (!is.null(x$split_on_groups)) cat("split_on_groups: ", paste(x$split_on_groups, collapse = ", "), "\n", sep = "")
  if (!is.null(x$predictor_vars)) cat("predictor_vars:  ", paste(x$predictor_vars, collapse = ", "), "\n", sep = "")
  cat("min_dt:          ", x$min_dt, "\n", sep = "")
  cat("t0_strategy:     ", x$t0_strategy, "\n", sep = "")
  if (!is.null(x$fixed_t0)) cat("fixed_t0:        ", x$fixed_t0, "\n", sep = "")
  invisible(x)
}



.flux_assert_spec <- function(x, where = "") {
  if (!inherits(x, "flux_spec")) {
    msg <- if (nzchar(where)) sprintf("%s: `specs` must contain flux_spec objects (use spec_state()/spec_event()/spec_decision()).", where) else
      "`specs` must contain flux_spec objects (use spec_state()/spec_event()/spec_decision())."
    stop(msg, call. = FALSE)
  }
  TRUE
}
