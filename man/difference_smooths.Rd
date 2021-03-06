% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smooth-differences.R
\name{difference_smooths}
\alias{difference_smooths}
\alias{difference_smooths.gam}
\title{Differences of factor smooth interactions}
\usage{
difference_smooths(model, ...)

\method{difference_smooths}{gam}(
  model,
  smooth,
  n = 100,
  ci_level = 0.95,
  newdata = NULL,
  partial_match = TRUE,
  unconditional = FALSE,
  frequentist = FALSE,
  ...
)
}
\arguments{
\item{model}{A fitted model.}

\item{...}{arguments passed to other methods.}

\item{smooth}{character; which smooth to compute differences for.}

\item{n}{numeric; the number of points at which to evaluate the difference
between pairs of smooths.}

\item{ci_level}{numeric between 0 and 1; the coverage of credible interval.}

\item{newdata}{data frame of locations at which to evaluate the difference
between smooths.}

\item{partial_match}{logical; should \code{smooth} match partially against
\code{smooths}? If \code{partial_match = TRUE}, \code{smooth} must only be a single
string, a character vector of length 1. Unlike similar functions, the
default here is \code{TRUE} because the intention is that users will be matching
against factor-by smooth labels.}

\item{unconditional}{logical; account for smoothness selection in the model?}

\item{frequentist}{logical; use the frequentist covariance matrix?}
}
\description{
Differences of factor smooth interactions
}
\examples{

load_mgcv()
\dontshow{
op <- options(digits = 3, cli.unicode = FALSE)
}
df <- data_sim("eg4", seed = 42)
m <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = df, method = "REML")

difference_smooths(m, smooth = "s(x2)")
\dontshow{options(op)}
}
