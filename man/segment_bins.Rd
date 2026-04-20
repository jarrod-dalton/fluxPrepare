\name{segment_bins}
\alias{segment_bins}
\title{Segmentation rules from bin cutpoints}
\usage{segment_bins(...)}
\arguments{
\item{...}{Named numeric vectors of strictly increasing cutpoints, one per variable.
Endpoints may include \code{-Inf} and \code{Inf}.}
}
\value{A list with class \code{"segment_rules"} containing \code{$bins}.}
\description{
Constructs segmentation rules that trigger interval splitting when a variable
crosses a specified bin boundary.
}
