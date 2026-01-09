\name{ps_spec_event}
\alias{ps_spec_event}
\title{Construct a validated event-model spec}
\description{
Construct a classed spec object for one-step event-time datasets. Event specs
declare the event type and baseline policy (t0 strategy). Event types are defined
by the user-provided event stream, so schema validation is not performed.
}
\usage{
ps_spec_event(
  event_type,
  name = NULL,
  t0_strategy = c("followup_start", "first_event", "fixed"),
  fixed_t0 = 0,
  fu_start_col = "followup_start",
  fu_end_col = "followup_end",
  death_col = NULL
)
}
\arguments{
\item{event_type}{Character scalar specifying the event type to model.}
\item{name}{Optional human-readable name.}
\item{t0_strategy}{How to define baseline t0 for each patient.}
\item{fixed_t0}{Numeric scalar used when \code{t0_strategy = "fixed"}.}
\item{fu_start_col}{Follow-up start column name.}
\item{fu_end_col}{Follow-up end column name.}
\item{death_col}{Optional death time column name.}
}
\value{
A spec object with class \code{"ps_spec_event"} and \code{"ps_spec"}.
}