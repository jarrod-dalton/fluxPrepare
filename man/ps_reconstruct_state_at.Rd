\name{ps_reconstruct_state_at}
\alias{ps_reconstruct_state_at}
\title{Reconstruct predictor state at anchor times}
\description{Reconstruct predictor values at specified anchor times using LOCF and optional staleness/lookback guardrails.}
\usage{ps_reconstruct_state_at(anchors, observations, vars, id_col = "patient_id", time_col = "t0", lookback = Inf, staleness = Inf, keep_provenance = TRUE, row_policy = c("return_all", "drop_incomplete"), derived_vars = NULL, derived_provider = NULL, derived_context = NULL, derived_on_missing = c("na", "error"), keep_derived_provenance = FALSE, count_no_history = c("na", "zero"), count_vars = NULL)}
\arguments{
\item{anchors}{Data frame of anchor times with columns \code{patient_id} and \code{t0}. \code{t0} may be numeric model time, Date, or POSIXct.}
\item{observations}{Canonical observation store as returned by \code{ps_prepare_observations()}.}
\item{ctx}{Optional context list. If provided, calendar-time values in \code{anchors$t0} or \code{observations$time} are converted to numeric model time using \code{ctx$time$unit/origin/zone}.}
\item{vars}{Character vector of variables to reconstruct.}
\item{id_col}{Patient id column name in \code{anchors}.}
\item{time_col}{Anchor time column name in \code{anchors}.}
\item{lookback}{Lookback window.}
\item{staleness}{Staleness window(s).}
\item{keep_provenance}{Logical; keep provenance columns.}
\item{seed}{Optional integer seed.}
}
\value{A data.frame with one row per anchor time and reconstructed values (and provenance columns when requested).}
\keyword{internal}
