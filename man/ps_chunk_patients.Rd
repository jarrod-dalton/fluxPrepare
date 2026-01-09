\name{ps_chunk_patients}
\alias{ps_chunk_patients}
\title{Chunk patient IDs for batch processing}
\usage{ps_chunk_patients(patient_ids, n_chunks = NULL, chunk_size = NULL, equal_percent = FALSE)}
\arguments{
  \item{patient_ids}{Character vector of patient IDs.}
  \item{n_chunks}{Number of chunks to create.}
  \item{chunk_size}{Fixed number of patients per chunk.}
  \item{equal_percent}{If \code{TRUE}, create chunks representing approximately equal proportions of the cohort.}
}
\value{A list of patient-ID vectors, one per chunk.}
\description{
Splits a cohort into deterministic chunks for disk-backed batch building.
}
