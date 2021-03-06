% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nested-functions.R
\name{nested_rug_values}
\alias{nested_rug_values}
\title{Values for rug plot in nested form}
\usage{
nested_rug_values(object, terms = NULL, data = NULL)
}
\arguments{
\item{object}{a fitted GAM model}

\item{terms}{a vector of terms to include original data for. Passed to
argument \code{terms} of \code{\link[mgcv:predict.gam]{mgcv::predict.gam()}}].}

\item{data}{optional data frame}
}
\value{
A nested tibble (data frame) with one row per smooth term.
}
\description{
Extracts original data for smooth terms, formats them in long/tidy
format, then nests the data column(s) such that the result
is a nested data frame with one row per smooth.
}
\keyword{internal}
