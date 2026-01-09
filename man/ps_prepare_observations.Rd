\name{ps_prepare_observations}
\alias{ps_prepare_observations}
\title{Prepare a canonical long-format observation table}
\usage{
ps_prepare_observations(tables,
  specs,
  keep_source = TRUE,
  ctx = NULL,
  sort = TRUE)
}
\arguments{
  \item{tables}{Named list of data.frames, one per source table.}
  \item{specs}{Named list of per-table specs keyed by table name. Each spec must contain \code{id_col}, \code{time_col}, and \code{vars}, and may optionally contain \code{group}.}
  \item{keep_source}{Logical; if TRUE, include \code{source_table} column.}
  \item{ctx}{Optional model context. Required when any \code{time_col} is \code{Date} or \code{POSIXct} so that calendar time can be mapped to numeric model time via \code{ctx$time$unit/origin/zone}.}
  \item{sort}{Logical; if TRUE, sort observations within patient by time (then group).}
}
\value{A data.frame with at least: \code{patient_id}, \code{time}, \code{group}, declared variable columns, and optional provenance columns.}
\description{\code{ps_prepare_observations()} binds one or more raw measurement tables into a single long-format observation table, with unified columns and time handling for downstream phases. Calendar time inputs are converted to numeric model time using \code{patientSimCore::ps_time_to_model()} when \code{ctx} is provided.}
