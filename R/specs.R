#' Construct a validated state-model spec
#'
#' Specs are the modeler's declared "lens" on raw observation tables.
#' A state spec declares: (a) the outcome group that defines interval anchors,
#' (b) outcome variables, and (c) predictor variables. The spec is validated
#' against a patientSimCore schema at construction time.
#'
#' @param schema A patientSimCore schema (named list).
#' @param outcome_group Character scalar. Observation group used to build anchor intervals.
#' @param outcome_vars Character vector. Outcome variables.
#' @param predictor_vars Character vector. Predictor variables.
#' @param name Optional human-readable name.
#' @param lookback Lookback window (numeric scalar). Passed to \code{build_ttv_state()}.
#' @param staleness Staleness window (numeric scalar). Passed to \code{build_ttv_state()}.
#' @param keep_provenance Logical. Passed to \code{build_ttv_state()}.
#' @param row_policy One of \code{"return_all"} or \code{"drop_incomplete"}.
#' @param derived_vars Optional character vector of derived variable names.
#' @param derived_provider Optional provider string. Passed to \code{build_ttv_state()}.
#' @param derived_context Optional list. Passed to \code{build_ttv_state()}.
#' @param derived_on_missing One of \code{"na"} or \code{"error"}.
#' @param keep_derived_provenance Logical. Passed to \code{build_ttv_state()}.
#' @param count_no_history One of \code{"na"} or \code{"zero"}.
#' @param count_vars Optional character vector of variables for which count-derived vars may be requested.
#'
#' @return A spec object with class \code{"spec_state", "ps_spec"}.
#' @export
spec_state <- function(schema,
                          outcome_group,
                          outcome_vars,
                          predictor_vars,
                          name = NULL,
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
                          count_vars = NULL) {

  row_policy <- match.arg(row_policy)
  derived_on_missing <- match.arg(derived_on_missing)
  count_no_history <- match.arg(count_no_history)

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
  schema <- patientSimCore::ps_schema_validate(schema)
  all_vars <- unique(c(outcome_vars, predictor_vars))
  patientSimCore::ps_schema_assert_vars(schema, all_vars)

  if (!is.null(derived_vars)) {
    if (!is.character(derived_vars)) stop("spec_state(): derived_vars must be NULL or a character vector.", call. = FALSE)
    derived_vars <- unique(as.character(derived_vars))
    patientSimCore::ps_schema_assert_vars(schema, derived_vars)
  }

  if (!is.null(count_vars)) {
    if (!is.character(count_vars)) stop("spec_state(): count_vars must be NULL or a character vector.", call. = FALSE)
    count_vars <- unique(as.character(count_vars))
    patientSimCore::ps_schema_assert_vars(schema, count_vars)
  }

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

  if (!is.null(derived_provider)) {
    if (!is.character(derived_provider) || length(derived_provider) != 1L || is.na(derived_provider) || trimws(derived_provider) == "") {
      stop("spec_state(): derived_provider must be NULL or a non-empty character scalar.", call. = FALSE)
    }
    derived_provider <- as.character(derived_provider)
  }

  if (!is.null(derived_context) && !is.list(derived_context)) {
    stop("spec_state(): derived_context must be NULL or a list.", call. = FALSE)
  }

  if (!is.logical(keep_derived_provenance) || length(keep_derived_provenance) != 1L || is.na(keep_derived_provenance)) {
    stop("spec_state(): keep_derived_provenance must be TRUE/FALSE.", call. = FALSE)
  }
  keep_derived_provenance <- isTRUE(keep_derived_provenance)

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
      derived_vars = derived_vars,
      derived_provider = derived_provider,
      derived_context = derived_context,
      derived_on_missing = derived_on_missing,
      keep_derived_provenance = keep_derived_provenance,
      count_no_history = count_no_history,
      count_vars = count_vars
    )
  )

  class(spec) <- c("spec_state", "ps_spec")
  spec
}

#' Construct a validated event-model spec
#'
#' Event specs declare a target event type and the baseline (t0) policy.
#' Unlike state specs, event specs do not validate against a patientSimCore schema
#' because event types are defined by the user-provided event stream.
#'
#' @param event_type Character scalar specifying the event type to model.
#' @param name Optional human-readable name.
#' @param t0_strategy One of \code{"followup_start"}, \code{"first_event"}, or \code{"fixed"}.
#' @param fixed_t0 Numeric scalar used when \code{t0_strategy = "fixed"}.
#' @param fu_start_col Follow-up start column name.
#' @param fu_end_col Follow-up end column name.
#' @param death_col Optional death time column name.
#'
#' @return A spec object with class \code{"spec_event", "ps_spec"}.
#' @export
spec_event <- function(event_type,
                          name = NULL,
                          t0_strategy = c("followup_start", "first_event", "fixed"),
                          fixed_t0 = 0,
                          fu_start_col = "followup_start",
                          fu_end_col = "followup_end",
                          death_col = NULL) {

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
      death_col = death_col
    )
  )

  class(spec) <- c("spec_event", "ps_spec")
  spec
}

#' Build segmentation rules for time segmentation
#'
#' These helper constructors define what counts as a \emph{meaningful} change in
#' predictors when building start-stop (TTV) event-process datasets.
#'
#' @param ... Named arguments whose names are variable names.
#'
#' @return A segmentation-rules object (a list with class \code{"segment_rules"}).
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

#' @param ... Character vectors of variable names.
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

#' Combine multiple segmentation-rule objects
#'
#' Later rules overwrite earlier rules of the same type for the same variable.
#'
#' @param ... One or more objects created by \code{segment_bins()}, \code{segment_eps()},
#'   \code{segment_rel_eps()}, or \code{segment_flip()}.
#'
#' @return A segmentation-rules object.
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
        out[[k]] <- modifyList((if (is.null(out[[k]])) list() else out[[k]]), r[[k]])
      } else if (k %in% c("eps", "rel_eps")) {
        out[[k]] <- modifyList((if (is.null(out[[k]])) list() else out[[k]]), r[[k]])
      } else if (k %in% c("flip")) {
        out[[k]] <- unique(c((if (is.null(out[[k]])) character(0) else out[[k]]), r[[k]]))
      }
    }
  }
  class(out) <- "segment_rules"
  out
}


#' Construct an event-process spec for start-stop / cause-specific hazard modeling
#'
#' This spec defines an event *process* as a set of event types (causes) and a
#' splitting policy for constructing a counting-process (start-stop) training/test/validation
#' dataset. It is designed to support cause-specific Cox models and parametric competing-risks
#' models (e.g., \pkg{flexsurv}) without imposing a fixed time grid.
#'
#' @param event_types Character vector of event types that constitute the process (causes).
#' @param name Optional human-readable name.
#' @param split_on_groups Optional character vector of observation groups that can trigger interval
#'   splitting (e.g., \code{c("labs", "visits")}). If NULL, no splitting is performed (one interval
#'   per patient until event/censoring).
#' @param segment_on_vars Optional character vector of observation variables to use for time segmentation.
#'   When provided, candidate segmentation times are filtered to those at which at least one of these
#'   variables is observed (non-missing), and a new interval boundary is created only when a
#'   "meaningful change" is detected according to \code{segment_rules}.
#' @param segment_rules Optional segmentation rules. You may pass a list directly, or use
#'   helper constructors such as \code{segment_bins()}, \code{segment_eps()}, \code{segment_rel_eps()}, and
#'   \code{segment_flip()}, optionally combined with \code{segment_rules_combine()}.
#' @param candidate_times How to generate candidate time boundaries before applying \code{min_dt} and
#'   \code{segment_rules}. One of \code{"groups"} (default), \code{"vars"}, or \code{"groups_or_vars"}.
#'   \code{"groups"} uses times from \code{split_on_groups}. \code{"vars"} uses times where
#'   \code{segment_on_vars} are observed. \code{"groups_or_vars"} uses the union. Supported
#'   entries include:\cr
#'   \itemize{
#'     \item \code{eps}: named numeric vector of absolute-change thresholds per variable.
#'     \item \code{rel_eps}: named numeric vector of relative-change thresholds per variable.
#'     \item \code{bins}: named list of numeric cutpoints per variable; segmentation occurs when the
#'       value moves to a different bin.
#'     \item \code{flip}: character vector of variables treated as booleans/categoricals; segmentation
#'       occurs when the value changes.
#'   }
#' @param min_dt Optional minimum spacing between split times (numeric scalar, same time units as
#'   prepared times). When > 0, split candidates closer than \code{min_dt} are ignored.
#' @param t0_strategy How to define t0 for each patient. One of \code{"followup_start"},
#'   \code{"first_event"}, or \code{"fixed"}.
#' @param fixed_t0 Numeric scalar used when \code{t0_strategy = "fixed"}.
#' @param fu_start_col Follow-up start column name.
#' @param fu_end_col Follow-up end column name.
#' @param death_col Optional death time column name.
#'
#' @return A spec object with class \code{"spec_event_process", "ps_spec"}.
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
                                  death_col = NULL) {

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
    death_col = death_col
  )

  class(spec) <- c("spec_event_process", "ps_spec")
  spec
}




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

print.spec_event <- function(x, ...) {
  cat("<spec_event>
")
  if (!is.null(x$name) && nzchar(x$name)) cat("name:       ", x$name, "
", sep = "")
  cat("event_type: ", x$event_type, "
", sep = "")
  invisible(x)
}

print.spec_event_process <- function(x, ...) {
  cat("<spec_event_process>\n")
  if (!is.null(x$name) && nzchar(x$name)) cat("name:           ", x$name, "\n", sep = "")
  cat("event_types:     ", paste(x$event_types, collapse = ", "), "\n", sep = "")
  if (!is.null(x$split_on_groups)) cat("split_on_groups: ", paste(x$split_on_groups, collapse = ", "), "\n", sep = "")
  cat("min_dt:          ", x$min_dt, "\n", sep = "")
  cat("t0_strategy:     ", x$t0_strategy, "\n", sep = "")
  if (!is.null(x$fixed_t0)) cat("fixed_t0:        ", x$fixed_t0, "\n", sep = "")
  invisible(x)
}



.ps_assert_spec <- function(x, where = "") {
  if (!inherits(x, "ps_spec")) {
    msg <- if (nzchar(where)) sprintf("%s: `specs` must contain ps_spec objects (use spec_state()/spec_event()).", where) else
      "`specs` must contain ps_spec objects (use spec_state()/spec_event())."
    stop(msg, call. = FALSE)
  }
  TRUE
}
