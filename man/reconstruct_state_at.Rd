\name{reconstruct_state_at}
\alias{reconstruct_state_at}
\title{Reconstruct as-of state at anchor times}
\usage{reconstruct_state_at(anchors, observations, vars, id_col = "patient_id", time_col = "t0", lookback = Inf, staleness = Inf, keep_provenance = TRUE, row_policy = c("return_all", "drop_incomplete"), derived_vars = NULL, derived_provider = NULL, derived_context = NULL, derived_on_missing = c("na", "error"), keep_derived_provenance = FALSE, count_no_history = c("na", "zero"), count_vars = NULL, ctx = NULL)}
\arguments{
\item{anchors}{A data.frame with patient id and anchor time.}
\item{observations}{Canonical observation store from \code{prepare_observations()}.}
\item{vars}{Character vector of variable names to reconstruct.}
\item{id_col}{Patient id column in \code{anchors}.}
\item{time_col}{Anchor time column in \code{anchors}.}
\item{lookback}{Numeric lookback window.}
\item{staleness}{Numeric maximum age; scalar or named per-variable vector.}
\item{keep_provenance}{If TRUE, add per-variable provenance columns.}
\item{row_policy}{One of \code{"return_all"} or \code{"drop_incomplete"}.}
\item{derived_vars}{Optional derived-variable names to compute at anchors.}
\item{derived_provider}{Optional provider required when \code{derived_vars} is set.}
\item{derived_context}{Optional context list for derived computation.}
\item{derived_on_missing}{One of \code{"na"} or \code{"error"}.}
\item{keep_derived_provenance}{If TRUE, include derived availability columns.}
\item{count_no_history}{How to handle count-like derived vars with no history: \code{"na"} or \code{"zero"}.}
\item{count_vars}{Optional subset of derived vars treated as count-like.}
\item{ctx}{Optional context used when anchor/observation times are \code{Date}/\code{POSIXct}.}
}
\value{A data.frame with reconstructed values at anchors and optional provenance columns.}
\description{Reconstructs requested variables as the most recent observed non-missing values at or before each anchor time, subject to lookback and staleness constraints.}
