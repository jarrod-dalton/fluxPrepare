\name{build_ttv_state}
\alias{build_ttv_state}
\title{Build one-step TTV dataset for a state transition model}
\usage{
build_ttv_state(
  observations,
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
  seed = NULL
)
}
\arguments{
\item{observations}{Canonical observation store from \code{prepare_observations()}.}
\item{splits}{Canonical split table from \code{prepare_splits()}.}
\item{ctx}{Optional context list used for calendar-time follow-up coercion.}
\item{outcome_group}{Observation group used to define consecutive anchor intervals.}
\item{outcome_vars}{Outcome variable names evaluated at interval end.}
\item{predictor_vars}{Predictor variable names reconstructed at interval start.}
\item{followup}{Optional follow-up table used to censor intervals.}
\item{fu_start_col}{Follow-up start column name in \code{followup}.}
\item{fu_end_col}{Follow-up end column name in \code{followup}.}
\item{death_col}{Optional death time column name in \code{followup}.}
\item{lookback}{Lookback passed to \code{reconstruct_state_at()}.}
\item{staleness}{Staleness passed to \code{reconstruct_state_at()}.}
\item{keep_provenance}{Logical; include reconstruction provenance columns.}
\item{row_policy}{One of \code{"return_all"} or \code{"drop_incomplete"}.}
\item{derived_vars}{Optional derived variables to compute at anchors.}
\item{derived_provider}{Optional derived provider.}
\item{derived_context}{Optional context passed to the derived provider.}
\item{derived_on_missing}{One of \code{"na"} or \code{"error"}.}
\item{keep_derived_provenance}{Logical; include derived availability columns.}
\item{count_no_history}{How to handle count-like derived vars with no history: \code{"na"} or \code{"zero"}.}
\item{count_vars}{Optional count-like derived variables.}
\item{max_intervals_per_entity}{Optional per-entity cap on sampled intervals.}
\item{seed}{Optional integer seed used when sampling intervals.}
}
\value{A data.frame with interval bounds, \code{deltat}, predictors at \code{t0}, outcomes at \code{t1}, and optional provenance.}
\description{Constructs a one-step interval dataset for state-transition modeling using as-of reconstruction at interval start.}
