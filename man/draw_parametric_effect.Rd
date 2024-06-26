% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parametric-effects.R
\name{draw_parametric_effect}
\alias{draw_parametric_effect}
\title{Internal function to draw an individual parametric effect}
\usage{
draw_parametric_effect(
  object,
  ci_level = 0.95,
  ci_col = "black",
  ci_alpha = 0.2,
  line_col = "black",
  constant = NULL,
  fun = NULL,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  rug = TRUE,
  position = "identity",
  ylim = NULL,
  angle = NULL,
  factor_levels = NULL,
  ...
)
}
\arguments{
\item{object}{a fitted GAM, the result of a call to \code{\link[mgcv:gam]{mgcv::gam()}}.}

\item{ci_level}{numeric between 0 and 1; the coverage of credible interval.}

\item{ci_col}{colour specification for the confidence/credible intervals
band. Affects the fill of the interval.}

\item{ci_alpha}{numeric; alpha transparency for confidence or simultaneous
interval.}

\item{constant}{numeric; a constant to add to the estimated values of the
smooth. \code{constant}, if supplied, will be added to the estimated value
before the confidence band is computed.}

\item{fun}{function; a function that will be applied to the estimated values
and confidence interval before plotting. Can be a function or the name of a
function. Function \code{fun} will be applied after adding any \code{constant}, if
provided.}

\item{xlab}{character or expression; the label for the x axis. If not
supplied, a suitable label will be generated from \code{object}.}

\item{ylab}{character or expression; the label for the y axis. If not
supplied, a suitable label will be generated from \code{object}.}

\item{title}{character or expression; the title for the plot. See
\code{\link[ggplot2:labs]{ggplot2::labs()}}.}

\item{subtitle}{character or expression; the subtitle for the plot. See
\code{\link[ggplot2:labs]{ggplot2::labs()}}.}

\item{caption}{character or expression; the plot caption. See
\code{\link[ggplot2:labs]{ggplot2::labs()}}.}

\item{rug}{logical; draw a rug plot at the bottom of each plot for 1-D
smooths or plot locations of data for higher dimensions.}

\item{position}{Position adjustment, either as a string, or the result of a
call to a position adjustment function.}

\item{angle}{numeric; the angle at which the x axis tick labels are to be
drawn passed to the \code{angle} argument of \code{\link[ggplot2:guide_axis]{ggplot2::guide_axis()}}.}

\item{factor_levels}{list; a named list of factor levels}

\item{...}{additional arguments passed to \code{\link[patchwork:wrap_plots]{patchwork::wrap_plots()}}.}
}
\description{
Internal function to draw an individual parametric effect
}
\keyword{internal}
