\name{ps_build_ttv_event_process}
\alias{ps_build_ttv_event_process}
\title{Build start-stop TTV dataset for an event process}
\usage{
ps_build_ttv_event_process(events, observations, splits, spec, followup = NULL, ctx = NULL)
}
\arguments{
\item{events}{A canonical event stream as returned by \code{prepare_events()}.}
\item{observations}{A canonical observation store as returned by \code{prepare_observations()}.}
\item{splits}{A patient split table as returned by \code{prepare_splits()}.}
\item{spec}{A \code{ps_spec_event_process} object created by \code{ps_spec_event_process()}.}
\item{followup}{Optional follow-up table; see \code{ps_build_ttv_event()} for requirements.}
\item{ctx}{Optional patientSimCore context.}
}
\description{
Constructs a counting-process (start-stop) TTV dataset suitable for cause-specific Cox models and parametric multistate models.
}
\value{
A data.frame with one or more rows per patient, including start/stop times and an event indicator for the terminal interval.
}
