\name{ps_add_derived_at}
\alias{ps_add_derived_at}
\title{Add derived variables to an anchor-state table}
\usage{ps_add_derived_at(state_at, anchors, derived_vars, derived_provider, derived_context = NULL, derived_on_missing = c("na", "error"), keep_derived_provenance = FALSE, count_no_history = c("na", "zero"), ...)}
\arguments{
  \item{state_at}{Output of \code{ps_reconstruct_state_at()} or an equivalent anchor-state table.}
  \item{anchors}{Anchor table with \code{patient_id} and \code{t0}.}
  \item{derived_vars}{Character vector of derived-variable names to evaluate.}
  \item{derived_provider}{Provider used to compute derived values.}
  \item{derived_context}{Optional context passed to the provider.}
  \item{derived_on_missing}{Behavior when derived vars cannot be computed; either \code{"na"} or \code{"error"}.}
  \item{keep_derived_provenance}{Whether to include derived provenance columns.}
  \item{count_no_history}{How to handle count-like derived vars when there is no history; \code{"zero"} coerces to 0.}
  \item{...}{Reserved for future arguments.}
}
\value{A data frame with derived-variable columns (and optional provenance columns) appended.}
\description{
Convenience wrapper to compute and append derived variables at anchor times.
}
