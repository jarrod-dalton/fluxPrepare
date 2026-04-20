\name{spec_event_process}
\alias{spec_event_process}
\title{Construct an event-process spec for start-stop TTV building}
\usage{
spec_event_process(
  event_types,
  name = NULL,
  split_on_groups = NULL,
  segment_on_vars = NULL,
  segment_rules = NULL,
  candidate_times = c("groups", "vars", "groups_or_vars"),
  min_dt = 0,
  t0_strategy = c("followup_start", "first_event", "fixed"),
  fixed_t0 = 0,
  fu_start_col = "followup_start",
  fu_end_col = "followup_end",
  death_col = NULL
)
}
\arguments{
\item{event_types}{Character vector of event types that constitute the process.}
\item{name}{Optional human-readable name.}
\item{split_on_groups}{Optional observation groups that can trigger interval splitting.}
\item{segment_on_vars}{Optional observation variables used for segmentation.}
\item{segment_rules}{Optional segmentation rules object/list.}
\item{candidate_times}{One of \code{"groups"}, \code{"vars"}, or \code{"groups_or_vars"}.}
\item{min_dt}{Minimum spacing between split times (non-negative numeric scalar).}
\item{t0_strategy}{How to define \code{t0} for each entity.}
\item{fixed_t0}{Numeric scalar used when \code{t0_strategy = "fixed"}.}
\item{fu_start_col}{Follow-up start column name.}
\item{fu_end_col}{Follow-up end column name.}
\item{death_col}{Optional death time column name.}
}
\value{A spec object with class \code{c("spec_event_process", "flux_spec")}.}
\description{Defines an event process and segmentation policy for constructing counting-process (start-stop) TTV datasets.}
