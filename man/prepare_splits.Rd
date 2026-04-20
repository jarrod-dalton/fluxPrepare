\name{prepare_splits}
\alias{prepare_splits}
\title{Prepare an entity-level split table}
\usage{
prepare_splits(
  df,
  id_col = "entity_id",
  split_col = "split",
  allowed = c("train", "test", "validation")
)
}
\arguments{
\item{df}{A data.frame containing at least entity id and split assignment.}
\item{id_col}{Name of the entity id column.}
\item{split_col}{Name of the split column.}
\item{allowed}{Character vector of allowed split labels (case-insensitive).}
}
\value{A data.frame with columns \code{entity_id} and \code{split}.}
\description{Validates and normalizes an entity-level assignment table for train/test/validation splitting. Splits are enforced at the entity level to prevent within-entity leakage.}
