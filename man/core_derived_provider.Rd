\name{core_derived_provider}
\alias{core_derived_provider}
\title{Create a patientSimCore-backed derived-variable provider}
\usage{core_derived_provider(schema, derived_var_fns)}
\arguments{
\item{schema}{A patientSimCore schema list.}
\item{derived_var_fns}{Named list of patientSimCore-compatible derived-variable functions.}
}
\value{A provider object suitable for use with \code{reconstruct_state_at()} and \code{build_ttv_state()}.}
\description{Builds an adapter that reconstructs minimal Core Patient histories and evaluates derived variables at anchor times.}
