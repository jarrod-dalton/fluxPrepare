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
#' @param lookback Lookback window (numeric scalar). Passed to \code{ps_build_ttv_state()}.
#' @param staleness Staleness window (numeric scalar). Passed to \code{ps_build_ttv_state()}.
#' @param keep_provenance Logical. Passed to \code{ps_build_ttv_state()}.
#' @param row_policy One of \code{"return_all"} or \code{"drop_incomplete"}.
#' @param derived_vars Optional character vector of derived variable names.
#' @param derived_provider Optional provider string. Passed to \code{ps_build_ttv_state()}.
#' @param derived_context Optional list. Passed to \code{ps_build_ttv_state()}.
#' @param derived_on_missing One of \code{"na"} or \code{"error"}.
#' @param keep_derived_provenance Logical. Passed to \code{ps_build_ttv_state()}.
#' @param count_no_history One of \code{"na"} or \code{"zero"}.
#' @param count_vars Optional character vector of variables for which count-derived vars may be requested.
#'
#' @return A spec object with class \code{"ps_spec_state", "ps_spec"}.
#' @export
ps_spec_state <- function(schema,
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
    stop("ps_spec_state(): outcome_group must be a non-empty character scalar.", call. = FALSE)
  }
  outcome_group <- as.character(outcome_group)

  if (!is.character(outcome_vars) || length(outcome_vars) < 1L) {
    stop("ps_spec_state(): outcome_vars must be a non-empty character vector.", call. = FALSE)
  }
  outcome_vars <- unique(as.character(outcome_vars))

  if (!is.character(predictor_vars) || length(predictor_vars) < 1L) {
    stop("ps_spec_state(): predictor_vars must be a non-empty character vector.", call. = FALSE)
  }
  predictor_vars <- unique(as.character(predictor_vars))

  if (!is.null(name)) {
    if (!is.character(name) || length(name) != 1L || is.na(name) || trimws(name) == "") {
      stop("ps_spec_state(): name must be NULL or a non-empty character scalar.", call. = FALSE)
    }
    name <- as.character(name)
  }

  # Validate schema and variables up front (Core is authoritative)
  schema <- patientSimCore::ps_schema_validate(schema)
  all_vars <- unique(c(outcome_vars, predictor_vars))
  patientSimCore::ps_schema_assert_vars(schema, all_vars)

  if (!is.null(derived_vars)) {
    if (!is.character(derived_vars)) stop("ps_spec_state(): derived_vars must be NULL or a character vector.", call. = FALSE)
    derived_vars <- unique(as.character(derived_vars))
    patientSimCore::ps_schema_assert_vars(schema, derived_vars)
  }

  if (!is.null(count_vars)) {
    if (!is.character(count_vars)) stop("ps_spec_state(): count_vars must be NULL or a character vector.", call. = FALSE)
    count_vars <- unique(as.character(count_vars))
    patientSimCore::ps_schema_assert_vars(schema, count_vars)
  }

  if (!is.numeric(lookback) || length(lookback) != 1L || is.na(lookback)) {
    stop("ps_spec_state(): lookback must be a single numeric value (use Inf for no limit).", call. = FALSE)
  }
  lookback <- as.numeric(lookback)

  if (!is.numeric(staleness) || length(staleness) != 1L || is.na(staleness)) {
    stop("ps_spec_state(): staleness must be a single numeric value (use Inf for no limit).", call. = FALSE)
  }
  staleness <- as.numeric(staleness)

  if (!is.logical(keep_provenance) || length(keep_provenance) != 1L || is.na(keep_provenance)) {
    stop("ps_spec_state(): keep_provenance must be TRUE/FALSE.", call. = FALSE)
  }
  keep_provenance <- isTRUE(keep_provenance)

  if (!is.null(derived_provider)) {
    if (!is.character(derived_provider) || length(derived_provider) != 1L || is.na(derived_provider) || trimws(derived_provider) == "") {
      stop("ps_spec_state(): derived_provider must be NULL or a non-empty character scalar.", call. = FALSE)
    }
    derived_provider <- as.character(derived_provider)
  }

  if (!is.null(derived_context) && !is.list(derived_context)) {
    stop("ps_spec_state(): derived_context must be NULL or a list.", call. = FALSE)
  }

  if (!is.logical(keep_derived_provenance) || length(keep_derived_provenance) != 1L || is.na(keep_derived_provenance)) {
    stop("ps_spec_state(): keep_derived_provenance must be TRUE/FALSE.", call. = FALSE)
  }
  keep_derived_provenance <- isTRUE(keep_derived_provenance)

  spec <- list(
    name = name,
    task = "state",
    fun = "ps_build_ttv_state",
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

  class(spec) <- c("ps_spec_state", "ps_spec")
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
#' @return A spec object with class \code{"ps_spec_event", "ps_spec"}.
#' @export
ps_spec_event <- function(event_type,
                          name = NULL,
                          t0_strategy = c("followup_start", "first_event", "fixed"),
                          fixed_t0 = 0,
                          fu_start_col = "followup_start",
                          fu_end_col = "followup_end",
                          death_col = NULL) {

  if (!is.character(event_type) || length(event_type) != 1L || is.na(event_type) || trimws(event_type) == "") {
    stop("ps_spec_event(): event_type must be a non-empty character scalar.", call. = FALSE)
  }
  event_type <- as.character(event_type)

  if (!is.null(name)) {
    if (!is.character(name) || length(name) != 1L || is.na(name) || trimws(name) == "") {
      stop("ps_spec_event(): name must be NULL or a non-empty character scalar.", call. = FALSE)
    }
    name <- as.character(name)
  }

  t0_strategy <- match.arg(t0_strategy)
  fixed_t0 <- as.numeric(fixed_t0)
  if (!is.finite(fixed_t0)) stop("ps_spec_event(): fixed_t0 must be finite.", call. = FALSE)

  if (!is.character(fu_start_col) || length(fu_start_col) != 1L || is.na(fu_start_col) || trimws(fu_start_col) == "") {
    stop("ps_spec_event(): fu_start_col must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.character(fu_end_col) || length(fu_end_col) != 1L || is.na(fu_end_col) || trimws(fu_end_col) == "") {
    stop("ps_spec_event(): fu_end_col must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.null(death_col)) {
    if (!is.character(death_col) || length(death_col) != 1L || is.na(death_col) || trimws(death_col) == "") {
      stop("ps_spec_event(): death_col must be NULL or a non-empty character scalar.", call. = FALSE)
    }
    death_col <- as.character(death_col)
  }

  spec <- list(
    name = name,
    task = "event",
    fun = "ps_build_ttv_event",
    args = list(
      event_type = event_type,
      t0_strategy = t0_strategy,
      fixed_t0 = fixed_t0,
      fu_start_col = fu_start_col,
      fu_end_col = fu_end_col,
      death_col = death_col
    )
  )

  class(spec) <- c("ps_spec_event", "ps_spec")
  spec
}


print.ps_spec_state <- function(x, ...) {
  cat("<ps_spec_state>
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

print.ps_spec_event <- function(x, ...) {
  cat("<ps_spec_event>
")
  if (!is.null(x$name) && nzchar(x$name)) cat("name:       ", x$name, "
", sep = "")
  cat("event_type: ", x$event_type, "
", sep = "")
  invisible(x)
}

.ps_assert_spec <- function(x, where = "") {
  if (!inherits(x, "ps_spec")) {
    msg <- if (nzchar(where)) sprintf("%s: `specs` must contain ps_spec objects (use ps_spec_state()/ps_spec_event()).", where) else
      "`specs` must contain ps_spec objects (use ps_spec_state()/ps_spec_event())."
    stop(msg, call. = FALSE)
  }
  TRUE
}
