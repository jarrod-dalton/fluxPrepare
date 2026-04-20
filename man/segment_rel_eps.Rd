\name{segment_rel_eps}
\alias{segment_rel_eps}
\title{Segmentation rules from relative-change thresholds}
\usage{segment_rel_eps(...)}
\arguments{
\item{...}{Named non-negative numeric relative thresholds, one per variable.}
}
\value{A list with class \code{"segment_rules"} containing \code{$rel_eps}.}
\description{
Constructs segmentation rules that trigger interval splitting when relative
change in a variable exceeds the provided threshold.
}
