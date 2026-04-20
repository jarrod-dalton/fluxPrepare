\name{build_ttv_batch}
\alias{build_ttv_batch}
\title{Batch-build TTV datasets to disk}
\usage{
build_ttv_batch(
  specs,
  splits,
  ctx = NULL,
  events = NULL,
  observations = NULL,
  followup = NULL,
  out_dir,
  format = c("qs", "csv", "parquet", "rds"),
  overwrite = FALSE,
  seed = NULL,
  strict = TRUE,
  compress = TRUE,
  manifest_name = "ttv_manifest",
  chunk = list(method = "none", shuffle = TRUE)
)
}
\arguments{
\item{specs}{List of spec objects created by \code{spec_state()} and/or \code{spec_event()}.}
\item{splits}{Output of \code{prepare_splits()}.}
\item{ctx}{Optional context list passed through to builders.}
\item{events}{Canonical event stream from \code{prepare_events()} (required for event tasks).}
\item{observations}{Canonical observation store from \code{prepare_observations()} (required for state tasks).}
\item{followup}{Optional follow-up table.}
\item{out_dir}{Directory to write outputs.}
\item{format}{Output format: \code{"qs"}, \code{"csv"}, \code{"parquet"}, or \code{"rds"}.}
\item{overwrite}{If FALSE, error when output files already exist.}
\item{seed}{Optional integer seed used for chunking and sampling.}
\item{strict}{If TRUE, stop on first error; if FALSE, continue and record errors in the manifest.}
\item{compress}{If TRUE, compress outputs where supported.}
\item{manifest_name}{Base filename for manifest outputs.}
\item{chunk}{Optional list controlling entity chunking. Supported fields:
\code{method} (\code{"none"}, \code{"n_chunks"}, \code{"chunk_size"}, or \code{"pct"}),
\code{n_chunks},
\code{chunk_size},
\code{pct},
and \code{shuffle} (logical).}
}
\value{A \code{data.frame} manifest with class \code{"flux_manifest"}.}
\description{Builds one dataset per spec (and optional entity chunk), writes datasets and metadata to disk, and returns a manifest of outputs.}
