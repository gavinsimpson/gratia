% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate-methods.R
\name{simulate.gam}
\alias{simulate.gam}
\alias{simulate.gamm}
\alias{simulate.scam}
\title{Simulate from the posterior distribution of a GAM}
\usage{
\method{simulate}{gam}(
  object,
  nsim = 1,
  seed = NULL,
  data = newdata,
  weights = NULL,
  ...,
  newdata = NULL
)

\method{simulate}{gamm}(
  object,
  nsim = 1,
  seed = NULL,
  data = newdata,
  weights = NULL,
  ...,
  newdata = NULL
)

\method{simulate}{scam}(
  object,
  nsim = 1,
  seed = NULL,
  data = newdata,
  weights = NULL,
  ...,
  newdata = NULL
)
}
\arguments{
\item{object}{a fitted GAM, typically the result of a call to \link[mgcv:gam]{mgcv::gam}`
or \code{\link[mgcv:gamm]{mgcv::gamm()}}.}

\item{nsim}{numeric; the number of posterior simulations to return.}

\item{seed}{numeric; a random seed for the simulations.}

\item{data}{data frame; new observations at which the posterior draws
from the model should be evaluated. If not supplied, the data used to fit
the model will be used for \code{newdata}, if available in \code{object}.}

\item{weights}{numeric; a vector of prior weights. If \code{newdata} is null
then defaults to \code{object[["prior.weights"]]}, otherwise a vector of ones.}

\item{...}{arguments passed to methods. \code{simulate.gam()} and
\code{simulate.scam()} pass \code{...} on to \code{predict.gam()}. As such you can pass
additional arguments such as \code{terms}, \code{exclude}, to select which model
terms are included in the predictions. This may be useful, for example,
for excluding the effects of random effect terms.}

\item{newdata}{Deprecated. Use \code{data} instead.}
}
\value{
(Currently) A data frame with \code{nsim} columns.
}
\description{
Simulations from the posterior distribution of a fitted GAM model involve
computing predicted values for the observation data for which simulated
data are required, then generating random draws from the probability
distribution used when fitting the model.
}
\details{
For \code{simulate.gam()} to function, the \code{family} component of the fitted
model must contain, or be updateable to contain, the required random
number generator. See \code{\link[mgcv:fix.family.link]{mgcv::fix.family.rd()}}.
}
\examples{
load_mgcv()
dat <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

sims <- simulate(m1, nsim = 5, seed = 42)
head(sims)
}
\author{
Gavin L. Simpson
}
