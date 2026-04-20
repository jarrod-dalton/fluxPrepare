\name{prepare_observations}
\alias{prepare_observations}
\title{Prepare canonical observation tables}
\usage{prepare_observations(tables, specs, keep_source = TRUE, ctx = NULL, sort = TRUE)}
\arguments{
\item{tables}{Named list of data.frames.}
\item{specs}{Named list describing, for each table, \code{id_col}, \code{time_col}, \code{vars}, and optional \code{group}.}
\item{keep_source}{Logical; if TRUE, include \code{source_table} column.}
\item{ctx}{Optional model context. Required when times are \code{Date}/\code{POSIXct} to convert to numeric model time.}
\item{sort}{Logical; if TRUE, sort observations within entity by time (then group).}
}
\value{A data.frame with at least: \code{entity_id}, \code{time}, \code{group}, declared variable columns, and optional source column.}
\description{Normalizes a list of wide-ish observational tables into a single canonical observation store.}
