\name{add_derived_at}
\alias{add_derived_at}
\title{Add derived variables at anchor times}
\usage{add_derived_at(state_at, anchors, derived_vars, provider, context = list(), derived_on_missing = c("na", "error"), keep_derived_provenance = FALSE, count_no_history = c("na", "zero"), count_vars = NULL)}
\arguments{
\item{state_at}{Data.frame produced by \code{reconstruct_state_at()}.}
\item{anchors}{Data.frame with \code{patient_id} and \code{t0}.}
\item{derived_vars}{Character vector of derived variable names to compute.}
\item{provider}{Derived-variable provider (for example from \code{core_derived_provider()}).}
\item{context}{List containing at least \code{observations}, and optionally \code{events}.}
\item{derived_on_missing}{\code{"na"} to fill missing derived values with \code{NA}; \code{"error"} to stop.}
\item{keep_derived_provenance}{If TRUE, include availability columns for derived vars.}
\item{count_no_history}{If \code{"zero"}, convert \code{NA} to 0 for variables in \code{count_vars}.}
\item{count_vars}{Optional character vector of derived vars treated as count-like.}
}
\value{\code{state_at} with derived-variable columns appended.}
\description{Computes and appends derived variables at anchor times using a provider's \code{$compute()} method.}
