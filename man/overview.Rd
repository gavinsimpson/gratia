% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/overview.R
\name{overview}
\alias{overview}
\alias{overview.gam}
\title{Provides an overview of a model and the terms in that model}
\usage{
overview(model, ...)

\method{overview}{gam}(
  model,
  parametric = TRUE,
  random_effects = TRUE,
  dispersion = NULL,
  frequentist = FALSE,
  accuracy = 0.001,
  digits = 3,
  stars = FALSE,
  ...
)
}
\arguments{
\item{model}{a fitted model object to overview.}

\item{...}{arguments passed to other methods.}

\item{parametric}{logical; include the model parametric terms in the
overview?}

\item{random_effects}{tests of fully penalized smooth terms (those with a
zero-dimensional null space, e.g. random effects) are computationally
expensive and for large data sets producing these p values can take a
very long time. If \code{random_effects = FALSE}, the tests of the expensive
terms will be skipped.}

\item{dispersion}{numeric; a known value for the dispersion parameter. The
default \code{NULL} implies that the estimated value or the default value (1
for the Poisson distribution for example) where this is specified is used
instead.}

\item{frequentist}{logical; by default the Bayesian estimated covariance
matrix of the parameter estimates is used to calculate p values for
parametric terms. If \code{frequentist = FALSE}, the frequentist covariance
matrix of the parameter estimates is used.}

\item{accuracy}{numeric; accuracy with which to report p values, with p
values below this value displayed as \code{"< accuracy"}.}

\item{digits}{numeric; the number of significant digits to be used.}

\item{stars}{logical; should significance stars be added to the output?}
}
\description{
Provides an overview of a model and the terms in that model
}
\examples{

load_mgcv()
\dontshow{
op <- options(pillar.sigfig = 3, cli.unicode = FALSE)
}
df <- data_sim(n = 400, seed = 2)
m <- gam(y ~ x3 + s(x0) + s(x1, bs = "bs") + s(x2, bs = "ts"),
  data = df, method = "REML"
)
overview(m)
\dontshow{
options(op)
}
}
