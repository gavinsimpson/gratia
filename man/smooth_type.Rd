% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smooth-type.R
\name{smooth_type}
\alias{smooth_type}
\alias{smooth_type.default}
\alias{smooth_type.tprs.smooth}
\alias{smooth_type.ts.smooth}
\alias{smooth_type.cr.smooth}
\alias{smooth_type.cs.smooth}
\alias{smooth_type.cyclic.smooth}
\alias{smooth_type.pspline.smooth}
\alias{smooth_type.cpspline.smooth}
\alias{smooth_type.Bspline.smooth}
\alias{smooth_type.duchon.spline}
\alias{smooth_type.fs.interaction}
\alias{smooth_type.sz.interaction}
\alias{smooth_type.gp.smooth}
\alias{smooth_type.mrf.smooth}
\alias{smooth_type.random.effect}
\alias{smooth_type.sw}
\alias{smooth_type.sf}
\alias{smooth_type.soap.film}
\alias{smooth_type.t2.smooth}
\alias{smooth_type.sos.smooth}
\alias{smooth_type.tensor.smooth}
\alias{smooth_type.mpi.smooth}
\alias{smooth_type.mpd.smooth}
\alias{smooth_type.cx.smooth}
\alias{smooth_type.cv.smooth}
\alias{smooth_type.micx.smooth}
\alias{smooth_type.micv.smooth}
\alias{smooth_type.mdcx.smooth}
\alias{smooth_type.mdcv.smooth}
\alias{smooth_type.miso.smooth}
\alias{smooth_type.mifo.smooth}
\title{Determine the type of smooth and return it n a human readable form}
\usage{
smooth_type(smooth)

\method{smooth_type}{default}(smooth)

\method{smooth_type}{tprs.smooth}(smooth)

\method{smooth_type}{ts.smooth}(smooth)

\method{smooth_type}{cr.smooth}(smooth)

\method{smooth_type}{cs.smooth}(smooth)

\method{smooth_type}{cyclic.smooth}(smooth)

\method{smooth_type}{pspline.smooth}(smooth)

\method{smooth_type}{cpspline.smooth}(smooth)

\method{smooth_type}{Bspline.smooth}(smooth)

\method{smooth_type}{duchon.spline}(smooth)

\method{smooth_type}{fs.interaction}(smooth)

\method{smooth_type}{sz.interaction}(smooth)

\method{smooth_type}{gp.smooth}(smooth)

\method{smooth_type}{mrf.smooth}(smooth)

\method{smooth_type}{random.effect}(smooth)

\method{smooth_type}{sw}(smooth)

\method{smooth_type}{sf}(smooth)

\method{smooth_type}{soap.film}(smooth)

\method{smooth_type}{t2.smooth}(smooth)

\method{smooth_type}{sos.smooth}(smooth)

\method{smooth_type}{tensor.smooth}(smooth)

\method{smooth_type}{mpi.smooth}(smooth)

\method{smooth_type}{mpd.smooth}(smooth)

\method{smooth_type}{cx.smooth}(smooth)

\method{smooth_type}{cv.smooth}(smooth)

\method{smooth_type}{micx.smooth}(smooth)

\method{smooth_type}{micv.smooth}(smooth)

\method{smooth_type}{mdcx.smooth}(smooth)

\method{smooth_type}{mdcv.smooth}(smooth)

\method{smooth_type}{miso.smooth}(smooth)

\method{smooth_type}{mifo.smooth}(smooth)
}
\arguments{
\item{smooth}{an object inheriting from class \code{mgcv.smooth}.}
}
\description{
Determine the type of smooth and return it n a human readable form
}
