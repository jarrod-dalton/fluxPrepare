\name{prepare_events}
\alias{prepare_events}
\title{Prepare a canonical event stream}
\usage{
prepare_events(
  events,
  id_col = "entity_id",
  time_col = "time",
  type_col = "event_type",
  table_event_type = NULL,
  ctx = NULL,
  sort = TRUE
)
}
\arguments{
\item{events}{Either a data.frame of events, or a named list of event data.frames.}
\item{id_col}{Entity id column name (applied to all tables).}
\item{time_col}{Time column name (applied to all tables).}
\item{type_col}{Event type column name for single-table input. Ignored when \code{events} is a list.}
\item{table_event_type}{Optional named character vector mapping list element names to event types.}
\item{ctx}{Optional model context. Required when times are \code{Date}/\code{POSIXct} to convert to numeric model time.}
\item{sort}{Logical; if TRUE, sort events within entity by time (then event_type).}
}
\value{A data.frame with columns: \code{entity_id}, \code{time}, \code{event_type}, and \code{source_table}.}
\description{Normalizes one or more raw event tables into a single canonical event stream for downstream phases.}
