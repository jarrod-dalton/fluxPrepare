\name{prepare_splits}
\alias{prepare_splits}
\title{Prepare a patient-level split table}
\usage{prepare_splits(df, id_col = "patient_id", split_col = "split", allowed = c("train", "test", "validation"))}
\arguments{
\item{df}{A data.frame containing at least patient id and split assignment.}
\item{id_col}{Name of the patient id column.}
\item{split_col}{Name of the split column.}
\item{allowed}{Character vector of allowed split labels (case-insensitive).}
}
\value{A data.frame with columns \code{patient_id} and \code{split}.}
\description{Validates and normalizes a patient-level assignment table for train/test/validation splitting. Splits are enforced at the patient level to prevent within-patient leakage.}
