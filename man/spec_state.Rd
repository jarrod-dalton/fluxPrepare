\name{spec_state}
\alias{spec_state}
\title{Construct a validated state-model spec}
\usage{
spec_state(
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
\item{schema}{A fluxCore schema (named list).}
\item{outcome_group}{Character scalar defining the observation group for interval anchors.}
\item{outcome_vars}{Character vector of outcome variables.}
\item{predictor_vars}{Character vector of predictor variables.}
\item{name}{Optional human-readable name.}
\item{lookback}{Lookback window passed to \code{build_ttv_state()}.}
\item{staleness}{Staleness window passed to \code{build_ttv_state()}.}
\item{keep_provenance}{Logical passed to \code{build_ttv_state()}.}
\item{row_policy}{One of \code{"return_all"} or \code{"drop_incomplete"}.}
\item{derived_vars}{Optional character vector of derived variable names.}
\item{derived_provider}{Optional provider identifier.}
\item{derived_context}{Optional list passed through to \code{build_ttv_state()}.}
\item{derived_on_missing}{One of \code{"na"} or \code{"error"}.}
\item{keep_derived_provenance}{Logical passed to \code{build_ttv_state()}.}
\item{count_no_history}{One of \code{"na"} or \code{"zero"}.}
\item{count_vars}{Optional character vector of count-like variables.}
}
\value{A spec object with class \code{c("spec_state", "flux_spec")}.}
\description{Constructs a classed spec object for one-step state-transition datasets, validated against a fluxCore schema.}
