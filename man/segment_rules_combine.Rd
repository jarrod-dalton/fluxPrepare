\name{segment_rules_combine}
\alias{segment_rules_combine}
\title{Combine segmentation rule objects}
\usage{segment_rules_combine(...)}
\arguments{
\item{...}{One or more objects inheriting from \code{"segment_rules"}.}
}
\value{A merged \code{"segment_rules"} object.}
\description{
Combines multiple segmentation-rule objects into a single rule set. Named rule
entries are merged by variable; flip-variable sets are unioned.
}
