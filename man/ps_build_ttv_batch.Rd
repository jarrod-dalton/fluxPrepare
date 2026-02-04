\name{ps_build_ttv_batch}
\alias{ps_build_ttv_batch}
\title{Build multiple TTV datasets from validated specs}
\description{Build multiple TTV datasets from a list of validated specs, optionally writing them to disk with metadata.}
\usage{ps_build_ttv_batch(specs, splits, ctx = NULL, events = NULL, observations = NULL, followup = NULL, out_dir, format = c("qs", "csv", "parquet", "rds"), overwrite = FALSE, seed = NULL, strict = TRUE, compress = TRUE, manifest_name = "ttv_manifest", chunk = list(method = "none", shuffle = TRUE))}
\arguments{
\item{specs}{List of validated spec objects created by \code{ps_spec_state()} and/or \code{ps_spec_event()}.}
\item{splits}{Canonical split table from \code{prepare_splits()}.}
\item{ctx}{Optional context list. Passed through to builders for calendar-time conversions (primarily follow-up).}
\item{events}{Optional canonical event stream from \code{prepare_events()} (required for event specs).}
\item{observations}{Optional canonical observation store from \code{prepare_observations()} (required for state specs).}
\item{followup}{Optional follow-up table used for censoring.}
\item{out_dir}{Optional output directory.}
\item{overwrite}{Logical; overwrite existing outputs.}
\item{strict}{Logical; stop on first error.}
\item{format}{Output format.}
\item{compress}{Compression toggle where supported.}
\item{seed}{Optional integer seed.}
}
\value{A data.frame summarizing build status for each spec, including dataset paths and row counts.}
\keyword{internal}
