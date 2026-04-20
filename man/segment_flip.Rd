\name{segment_flip}
\alias{segment_flip}
\title{Segmentation rules from value flips}
\usage{segment_flip(...)}
\arguments{
\item{...}{One or more variable names to monitor for value changes.}
}
\value{A list with class \code{"segment_rules"} containing \code{$flip}.}
\description{
Constructs segmentation rules that trigger interval splitting when a monitored
variable changes value between candidate times.
}
