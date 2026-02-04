\name{ps_spec_event_process}
\alias{ps_spec_event_process}
\title{Construct an event-process spec for start-stop TTV building}
\usage{
ps_spec_event_process(event_types, name = NULL, split_on_groups = NULL, min_dt = 0,
  t0_strategy = c("followup_start", "first_event", "fixed"), fixed_t0 = 0,
  fu_start_col = "followup_start", fu_end_col = "followup_end", death_col = NULL)
}
\arguments{
\item{event_types}{Character vector of event types that constitute the process (causes).}
\item{name}{Optional human-readable name.}
\item{split_on_groups}{Optional character vector of observation groups that can trigger interval splitting. If NULL, no splitting is performed.}
\item{min_dt}{Minimum spacing between split times (non-negative numeric scalar).}
\item{t0_strategy}{How to define t0 for each patient. One of \code{"followup_start"}, \code{"first_event"}, or \code{"fixed"}.}
\item{fixed_t0}{Numeric scalar used when \code{t0_strategy = "fixed"}.}
\item{fu_start_col}{Follow-up start column name.}
\item{fu_end_col}{Follow-up end column name.}
\item{death_col}{Optional death time column name.}
}
\description{
Defines an event process (set of causes) and a splitting policy for constructing counting-process (start-stop) TTV datasets.
}
\value{
A spec object with class \code{c("ps_spec_event_process", "ps_spec")}.
}
