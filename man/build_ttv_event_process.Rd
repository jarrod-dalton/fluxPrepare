\name{build_ttv_event_process}
\alias{build_ttv_event_process}
\title{Build start-stop TTV dataset for an event process}
\usage{build_ttv_event_process(events, observations, splits, spec, followup = NULL, ctx = NULL)}
\arguments{
\item{events}{Canonical event stream from \code{prepare_events()}.}
\item{observations}{Canonical observation store from \code{prepare_observations()}.}
\item{splits}{Canonical split table from \code{prepare_splits()}.}
\item{spec}{A \code{spec_event_process()} object.}
\item{followup}{Optional follow-up table used for censoring.}
\item{ctx}{Optional patientSimCore context.}
}
\value{A start-stop data.frame with one or more rows per patient, including interval bounds and terminal event indicators.}
\description{Constructs a counting-process (start-stop) TTV dataset for event-process modeling.}
