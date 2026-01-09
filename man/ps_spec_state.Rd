\name{ps_spec_state}
\alias{ps_spec_state}
\title{Construct a validated state-model spec}
\description{
Construct a classed spec object that declares the outcome group, outcome variables,
and predictor variables for a state transition dataset. The spec is validated
against a \code{patientSimCore} schema at construction time.
}
\usage{
ps_spec_state(
  schema,
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
  count_vars = NULL
)
}
\arguments{
  \item{schema}{A patientSimCore schema (named list).}
  \item{outcome_group}{Character scalar. Observation group used to build anchor intervals.}
  \item{outcome_vars}{Character vector. Outcome variables.}
  \item{predictor_vars}{Character vector. Predictor variables.}
  \item{name}{Optional human-readable name.}
  \item{lookback}{Lookback window (numeric scalar). Passed to \code{ps_build_ttv_state()}.}
  \item{staleness}{Staleness window (numeric scalar). Passed to \code{ps_build_ttv_state()}.}
  \item{keep_provenance}{Logical. Passed to \code{ps_build_ttv_state()}.}
  \item{row_policy}{One of \code{"return_all"} or \code{"drop_incomplete"}.}
  \item{derived_vars}{Optional character vector of derived variable names.}
  \item{derived_provider}{Optional provider string. Passed to \code{ps_build_ttv_state()}.}
  \item{derived_context}{Optional list. Passed to \code{ps_build_ttv_state()}.}
  \item{derived_on_missing}{One of \code{"na"} or \code{"error"}.}
  \item{keep_derived_provenance}{Logical. Passed to \code{ps_build_ttv_state()}.}
  \item{count_no_history}{One of \code{"na"} or \code{"zero"}.}
  \item{count_vars}{Optional character vector of variables for which count-derived vars may be requested.}
}
\value{
A spec object with class \code{"ps_spec_state", "ps_spec"}.
}
