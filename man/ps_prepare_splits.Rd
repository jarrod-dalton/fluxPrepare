\name{ps_prepare_splits}
\alias{ps_prepare_splits}
\title{Canonicalize patient-level train/test/validation splits}
\usage{ps_prepare_splits(splits, split_levels = c("train", "test", "validation"))}
\arguments{
  \item{splits}{Data frame with at least \code{patient_id} and \code{split}.}
  \item{split_levels}{Allowed split labels.}
}
\value{A validated data frame with standardized columns.}
\description{
Enforces patient-level assignment to a single split and validates split labels.
}
