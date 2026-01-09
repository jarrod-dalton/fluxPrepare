\name{ps_build_ttv_state}
\alias{ps_build_ttv_state}
\title{Build one-step TTV dataset for a state transition model}
\description{Construct interval-based TTV datasets for predicting state variables at the end of an interval, conditional on predictors at the start.}
\usage{ps_build_ttv_state(observations, splits, ctx = NULL, outcome_group, outcome_vars, predictor_vars, followup = NULL, fu_start_col = "followup_start", fu_end_col = "followup_end", death_col = NULL, lookback = Inf, staleness = Inf, keep_provenance = TRUE, row_policy = c("return_all", "drop_incomplete"), derived_vars = NULL, derived_provider = NULL, derived_context = NULL, derived_on_missing = c("na", "error"), keep_derived_provenance = FALSE, count_no_history = c("na", "zero"), count_vars = NULL, max_intervals_per_patient = NULL, seed = NULL)}
\arguments{
\item{observations}{Canonical observation store from \code{ps_prepare_observations()}.}
\item{splits}{Canonical split table from \code{ps_prepare_splits()}.}
\item{ctx}{Optional context list. If provided, calendar-time columns in \code{followup} (Date/POSIXct) are converted to numeric model time using \code{ctx$time$unit/origin/zone}.}
\item{outcome_group}{Observation group used to define consecutive anchor intervals.}
\item{outcome_vars}{Outcome variable names (evaluated at interval end).}
\item{predictor_vars}{Predictor variable names (reconstructed at interval start).}
\item{followup}{Optional follow-up table (see \code{ps_build_ttv_event()} for required columns). Used to censor intervals beyond follow-up.}
\item{fu_start_col}{Follow-up start column name in \code{followup}.}
\item{fu_end_col}{Follow-up end column name in \code{followup}.}
\item{death_col}{Optional death time column name in \code{followup}.}
\item{lookback}{Lookback window passed to \code{ps_reconstruct_state_at()}.}
\item{staleness}{Staleness window(s) passed to \code{ps_reconstruct_state_at()}.}
\item{keep_provenance}{Logical; include provenance columns.}
\item{row_policy}{One of \code{"return_all"} or \code{"drop_incomplete"}.}
\item{derived_vars}{Optional derived variables to compute at anchors via patientSimCore.}
\item{derived_provider}{Optional derived provider (advanced).}
\item{derived_context}{Optional context passed to derived provider (advanced).}
\item{derived_on_missing}{One of \code{"na"} or \code{"error"}.}
\item{keep_derived_provenance}{Logical; keep derived provenance.}
\item{count_no_history}{How to handle count variables with no history: \code{"na"} or \code{"zero"}.}
\item{count_vars}{Optional count variables to compute.}
\item{max_intervals_per_patient}{Optional cap on number of intervals per patient.}
\item{seed}{Optional integer seed.}
}
\details{Intervals are defined by consecutive observation times within \code{outcome_group}. Predictors are reconstructed at \code{t0} using LOCF with optional lookback/staleness guardrails.}
\value{A data.frame with interval bounds (t0, t1), \code{deltat}, predictors at t0, outcomes at t1 (possibly censored), and split/provenance columns.}
\keyword{internal}
