\name{ps_build_ttv_event}
\alias{ps_build_ttv_event}
\title{Build one-step TTV dataset for an event model}
\description{Construct an interval-based train/test/validation dataset for a specified event type, with censoring from patient follow-up.}
\usage{ps_build_ttv_event(events, splits, ctx = NULL, event_type, t0_strategy = c("followup_start", "first_event", "fixed"), fixed_t0 = 0, followup = NULL, fu_start_col = "followup_start", fu_end_col = "followup_end", death_col = NULL)}
\arguments{
\item{events}{Canonical event stream from \code{ps_prepare_events()}. Must include columns \code{patient_id}, \code{time}, \code{event_type}.}
\item{splits}{Canonical split table from \code{ps_prepare_splits()}.}
\item{ctx}{Optional context list. If provided, calendar-time columns in \code{followup} (Date/POSIXct) are converted to numeric model time using \code{ctx$time$unit/origin/zone}.}
\item{event_type}{Target event type (character scalar).}
\item{t0_strategy}{Start-time policy for \code{t0}.}
\item{t0_fixed}{When \code{t0_strategy="fixed"}, the numeric model time to use as \code{t0}.}
\item{followup}{Follow-up table with columns \code{patient_id}, \code{followup_start}, \code{followup_end}. Optional \code{death_time} column if \code{death_col} is provided.}
\item{fu_start_col}{Follow-up start column name in \code{followup}.}
\item{fu_end_col}{Follow-up end column name in \code{followup}.}
\item{death_col}{Optional death time column name in \code{followup}. If provided, censoring time is \code{pmin(followup_end, death_time)}.}
\item{keep_provenance}{Logical; include provenance columns in output when available.}
\item{seed}{Optional integer seed for deterministic tie-breaking where applicable.}
}
\details{Each row describes an interval (t0, t1] for one patient. If the target event occurs before censoring, \code{t1} is the event time and \code{event_occurred=1}. Otherwise \code{t1} is the censoring time and \code{event_occurred=0}.}
\value{A data.frame with interval bounds (t0, t1), \code{deltat}, \code{event_occurred}, and split/provenance columns.}
\keyword{internal}
