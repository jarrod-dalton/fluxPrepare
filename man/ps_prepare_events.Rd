\name{prepare_events}
\alias{prepare_events}
\title{Prepare a canonical event stream}
\usage{
prepare_events(events,
  id_col = "patient_id",
  time_col = "time",
  type_col = "event_type",
  table_event_type = NULL,
  ctx = NULL,
  sort = TRUE)
}
\arguments{
  \item{events}{Either a data.frame of events, or a named list of event data.frames.}
  \item{id_col}{Patient id column name (applied to all tables).}
  \item{time_col}{Time column name (applied to all tables). Can be numeric, \code{Date}, or \code{POSIXct}.}
  \item{type_col}{Event type column name for single-table input. Ignored when \code{events} is a list.}
  \item{table_event_type}{Optional named character vector mapping list element names to event types.}
  \item{ctx}{Optional model context. Required when \code{time_col} is \code{Date} or \code{POSIXct} so that calendar time can be mapped to numeric model time via \code{ctx$time$unit/origin/zone}.}
  \item{sort}{Logical; if TRUE, sort events within patient by time (then event_type).}
}
\value{A data.frame with columns: \code{patient_id}, \code{time}, \code{event_type}, and \code{source_table}.}
\description{\code{prepare_events()} normalizes one or more raw event tables into a single canonical event stream for downstream phases. Calendar time inputs are converted to numeric model time using \code{patientSimCore::ps_time_to_model()} when \code{ctx} is provided.}
