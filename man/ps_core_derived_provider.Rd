\name{ps_core_derived_provider}
\alias{ps_core_derived_provider}
\title{Create a patientSimCore-backed derived-variable provider}
\usage{ps_core_derived_provider(schema, derived_var_fns)}
\arguments{
  \item{schema}{A patientSimCore schema list (state variable definitions).}
  \item{derived_var_fns}{Named list of patientSimCore derived-variable functions, e.g. created with \code{patientSimCore::derive()} or \code{patientSimCore::lag_of()}.}
}
\value{A derived-variable provider object suitable for use with \code{ps_reconstruct_state_at()} and \code{ps_build_ttv_state()}.}
\description{
Builds an adapter that constructs a Core \code{Patient} from pre-anchor history, evaluates schema-derived variables, and returns derived values at anchors.
}
