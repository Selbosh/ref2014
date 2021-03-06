% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.R
\name{aggregate_outputs}
\alias{aggregate_outputs}
\title{Aggregate outputs that are not in the top \code{n} journals}
\usage{
aggregate_outputs(outputs, top_journals = get_top_journals(outputs))
}
\arguments{
\item{outputs}{A data frame like that produced by \code{\link{tidy_outputs}}}

\item{top_journals}{A data frame like that produced by \code{\link{get_top_journals}}}
}
\value{
A data frame similar to \code{outputs} but with lower-ranked journals
aggregated classes 'Other journals', 'Conference proceedings' or 'Other
outputs' according to their output type.
}
\description{
Choose the top \code{n} journals according to the number of submissions in
the REF. The remaining journals are aggregated into a 'super-journal' called
'Other'.
}
\details{
To preserve data integrity, each journal is given a \code{new_id}, which in
the case of the top \code{n} journals is identical to their original
\code{journal_id}, in the case of conference proceedings is set equal to
\code{0}, in the case of \code{n+1} or less popular journals, is set equal to
\code{-1} and in the case of other non-journal outputs it set equal to
\code{-2}. These codes have no particular significance other than avoiding
clashes with the automatically-assigned \code{journal_id}s assigned by the
\code{\link{cluster_outputs_by_journals}} algorithm.
}
\examples{
math_outputs <- subset(tidy_outputs(), uoa_name == 'Mathematical Sciences')
math_outputs <- cluster_outputs_by_journals(math_outputs)
aggregate_outputs(math_outputs)

}
\seealso{
\code{\link{get_top_journals}}
}
