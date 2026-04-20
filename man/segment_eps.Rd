\name{segment_eps}
\alias{segment_eps}
\title{Segmentation rules from absolute-change thresholds}
\usage{segment_eps(...)}
\arguments{
\item{...}{Named non-negative numeric thresholds, one per variable.}
}
\value{A list with class \code{"segment_rules"} containing \code{$eps}.}
\description{
Constructs segmentation rules that trigger interval splitting when absolute
change in a variable exceeds the provided threshold.
}
