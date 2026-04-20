\name{chunk_entities}
\alias{chunk_entities}
\title{Entity chunking helper}
\usage{chunk_entities(splits, chunk = list(method = "none", shuffle = TRUE), seed = NULL)}
\arguments{
\item{splits}{Split table with \code{entity_id}.}
\item{chunk}{List of chunking parameters with fields:
\code{method} (\code{"none"}, \code{"n_chunks"}, \code{"chunk_size"}, or \code{"pct"}),
\code{n_chunks} (for \code{"n_chunks"}),
\code{chunk_size} (for \code{"chunk_size"}),
\code{pct} (for \code{"pct"}),
and optional \code{shuffle} (logical, default \code{TRUE}).}
\item{seed}{Optional seed for reproducible shuffling.}
}
\value{A list of character vectors of \code{entity_id} values.}
\description{Splits a cohort into deterministic chunks for disk-backed batch building.}
