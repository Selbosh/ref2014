% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.R
\name{tidy_results}
\alias{tidy_results}
\title{Tidy REF results and filter by Unit of Assessment}
\usage{
tidy_results(number = NULL, name = NULL, grep = NULL, ...)
}
\arguments{
\item{number}{an integer from 1 to 36. See \code{\link{UoA}} for a key}

\item{name}{a string. See \code{\link{UoA}} for possible names}

\item{grep}{a regular expression representing (part of) a UoA name}

\item{...}{arguments passed to \code{\link{grepl}}}
}
\value{
A \code{\link[tibble]{tibble}} containing the following columns:
\itemize{
\item name of institution
\item UoA name
\item UoA number
\item type of profile (Overall, Environment, Impact, Outputs)
\item number of full-time equivalent (FTE) staff in the submission
\item percentage of submission rated 4\*, 3\*, 2\*, 1\* or unclassified
}
}
\description{
This is a convenience function for filtering REF2014 results from a
particular Unit of Assessment (UoA; i.e. subject area) by its numeric ID,
by exact name or by a partial regular expression.
}
\details{
As well as filtering the rows of the \code{\link{results}} dataset, this
function also selects only the most useful columns and renames the column to
`snake_case`, which is slightly more convenient for programming.
}
\note{
This function does not check how many UoAs are selected. It could be zero,
one or all of them.
}
\examples{
# All of the following are equivalent:
tidy_results(18)
tidy_results(name = 'Economics & Econometrics')
tidy_results(grep = 'con')

}
