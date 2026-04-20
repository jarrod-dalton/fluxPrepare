\name{build_ttv_event}
\alias{build_ttv_event}
\title{Build one-step TTV dataset for an event model}
\usage{
build_ttv_event(
  events,
  splits,
  ctx = NULL,
  event_type,
  t0_strategy = c("followup_start", "first_event", "fixed"),
  fixed_t0 = 0,
  followup = NULL,
  fu_start_col = "followup_start",
  fu_end_col = "followup_end",
  death_col = NULL
)
}
\arguments{
\item{events}{Canonical event stream from \code{prepare_events()}.}
\item{splits}{Canonical split table from \code{prepare_splits()}.}
\item{ctx}{Optional context list used for calendar-time follow-up coercion.}
\item{event_type}{Target event type (character scalar).}
\item{t0_strategy}{Start-time policy for \code{t0}.}
\item{fixed_t0}{Numeric model time used when \code{t0_strategy = "fixed"}.}
\item{followup}{Optional follow-up table used for censoring.}
\item{fu_start_col}{Follow-up start column name in \code{followup}.}
\item{fu_end_col}{Follow-up end column name in \code{followup}.}
\item{death_col}{Optional death time column name in \code{followup}.}
}
\value{A data.frame with columns \code{entity_id}, \code{split}, \code{t0}, \code{t1}, \code{deltat}, \code{event_occurred}, \code{time_to_event}, and \code{censoring_time}.}
\description{Constructs a one-step interval dataset for a specified event type with explicit censoring semantics.}
