% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster.R
\name{cluster_outputs_by_journals}
\alias{cluster_outputs_by_journals}
\title{Group entities into journals according to identifiers}
\usage{
cluster_outputs_by_journals(data, use_volume_title = TRUE)
}
\arguments{
\item{use_volume_title}{Use the volume titles in grouping process? See Details}
}
\value{
The input data frame, with an added column \code{journal_id} assigning each
submission to a group representing a journal (or book). Any submissions that
could not be assigned to a group will have \code{journal_id = NA}.
}
\description{
This is an algorithm that identifies unique journals according to ISBNs,
ISSNs, DOIs and titles that overlap.
}
\details{
Some fields, such as Physics, feature journals with ambiguous volume titles,
for example Physical Review A, B, C, D, E, Physics Review Letters and their
various abbreviations. To avoid these journals mistakenly being aggregated,
we recommend setting \code{use_volume_title} to \code{FALSE}.
}
\note{
The current implementation is not 100% safe, as there is the possibility that
row IDs become large enough to be confused with ISSNs. We can fix this with
the addition of a non-numeric prefix to row ids.
}
\examples{
# Not done yet. Need to reconcile column names.

}
