#' Determine the type of smooth and return it in a human readable form
#'
#' Soap-film boundary and wiggly components use the `sf.film` and `sf.wiggly`
#' classes introduced in mgcv 1.9-4.
#'
#' @details SCAM smooth types use compact constraint descriptions: `inc` for
#'   increasing, `dec` for decreasing, and `pos` for positive. For tensor
#'   smooths, the two descriptions follow the covariate order in the smooth
#'   specification; `free` denotes an unconstrained direction. `Tensor int.`
#'   identifies an interaction. Explicit `By` bases share the type label of
#'   the corresponding ordinary basis.
#'
#' @param smooth an object inheriting from class `mgcv.smooth`.
#'
#' @export
`smooth_type` <- function(smooth) {
  UseMethod("smooth_type")
}

#' @export
#' @rdname smooth_type
`smooth_type.default` <- function(smooth) {
  stop("Unknown type of smooth")
}

#' @export
#' @rdname smooth_type
`smooth_type.tprs.smooth` <- function(smooth) {
  sm_type <- "TPRS"
  sm_dim <- smooth_dim(smooth)
  if (sm_dim > 1L) {
    sm_type <- paste0(sm_type, " (", sm_dim, "d)")
  }
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.ts.smooth` <- function(smooth) {
  sm_type <- "TPRS (shrink)"
  sm_dim <- smooth_dim(smooth)
  if (sm_dim > 1L) {
    sm_type <- paste0(sm_type, " (", sm_dim, "d)")
  }
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.cr.smooth` <- function(smooth) {
  sm_type <- "CRS"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.cs.smooth` <- function(smooth) {
  sm_type <- "CRS (shrink)"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.cyclic.smooth` <- function(smooth) {
  sm_type <- "Cyclic CRS"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.pspline.smooth` <- function(smooth) {
  sm_type <- "P spline"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.cpspline.smooth` <- function(smooth) {
  sm_type <- "Cyclic P spline"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.Bspline.smooth` <- function(smooth) {
  sm_type <- "B spline"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.duchon.spline` <- function(smooth) {
  sm_type <- "Duchon spline"
  sm_dim <- smooth_dim(smooth)
  if (sm_dim > 1L) {
    sm_type <- paste0(sm_type, " (", sm_dim, "d)")
  }
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.fs.interaction` <- function(smooth) {
  sm_type <- "Factor smooth"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.sz.interaction` <- function(smooth) {
  sm_type <- "Constr. factor smooth"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.gp.smooth` <- function(smooth) {
  sm_type <- "GP"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.mrf.smooth` <- function(smooth) {
  sm_type <- "MRF"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.random.effect` <- function(smooth) {
  sm_type <- "Random effect"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.sf.wiggly` <- function(smooth) {
  sm_type <- "Soap film (wiggly)"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.sf.film` <- function(smooth) {
  sm_type <- "Soap film (boundary)"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.soap.film` <- function(smooth) {
  sm_type <- "Soap film"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.t2.smooth` <- function(smooth) {
  sm_type <- if (identical(smooth_dim(smooth), 1L)) {
    "1d Tensor product (T2)"
  } else {
    "Tensor product (T2)"
  }
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.sos.smooth` <- function(smooth) {
  sm_type <- "SOS"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.tensor.smooth` <- function(smooth) {
  inter <- smooth[["inter"]]
  sm_dim <- smooth_dim(smooth)
  sm_type <- if (isTRUE(inter)) {
    "Tensor product int."
  } else {
    "Tensor product"
  }
  if (identical(sm_dim, 1L)) {
    sm_type <- paste("1d", sm_type)
  }
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.mpi.smooth` <- function(smooth) {
  sm_type <- "P spline (inc)"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.mpd.smooth` <- function(smooth) {
  sm_type <- "P spline (dec)"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.cx.smooth` <- function(smooth) {
  sm_type <- "P spline (convex)"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.cv.smooth` <- function(smooth) {
  sm_type <- "P spline (concave)"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.micx.smooth` <- function(smooth) {
  sm_type <- "P spline (inc, convex)"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.micv.smooth` <- function(smooth) {
  sm_type <- "P spline (inc, concave)"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.mdcx.smooth` <- function(smooth) {
  sm_type <- "P spline (dec, convex)"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.mdcv.smooth` <- function(smooth) {
  sm_type <- "P spline (dec, concave)"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.miso.smooth` <- function(smooth) {
  sm_type <- "P spline (inc, left=0)"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.mifo.smooth` <- function(smooth) {
  sm_type <- "P spline (inc, right=0)"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.po.smooth` <- function(smooth) {
  "P spline (pos)"
}

#' @export
#' @rdname smooth_type
`smooth_type.ipo.smooth` <- function(smooth) {
  "P spline (pos, inc)"
}

#' @export
#' @rdname smooth_type
`smooth_type.dpo.smooth` <- function(smooth) {
  "P spline (pos, dec)"
}

#' @export
#' @rdname smooth_type
`smooth_type.cpopspline.smooth` <- function(smooth) {
  "Cyclic P spline (pos)"
}

#' @export
#' @rdname smooth_type
`smooth_type.lmpi.smooth` <- function(smooth) {
  "P spline (local inc)"
}

#' @export
#' @rdname smooth_type
`smooth_type.lipl.smooth` <- function(smooth) {
  "P spline (inc then flat)"
}

#' @export
#' @rdname smooth_type
`smooth_type.tedmi.smooth` <- function(smooth) {
  "Tensor (inc, inc)"
}

#' @export
#' @rdname smooth_type
`smooth_type.tedmd.smooth` <- function(smooth) {
  "Tensor (dec, dec)"
}

#' @export
#' @rdname smooth_type
`smooth_type.temicx.smooth` <- function(smooth) {
  "Tensor (inc, convex)"
}

#' @export
#' @rdname smooth_type
`smooth_type.temicv.smooth` <- function(smooth) {
  "Tensor (inc, concave)"
}

#' @export
#' @rdname smooth_type
`smooth_type.tedecx.smooth` <- function(smooth) {
  "Tensor (dec, convex)"
}

#' @export
#' @rdname smooth_type
`smooth_type.tedecv.smooth` <- function(smooth) {
  "Tensor (dec, concave)"
}

#' @export
#' @rdname smooth_type
`smooth_type.tecxcx.smooth` <- function(smooth) {
  "Tensor (convex, convex)"
}

#' @export
#' @rdname smooth_type
`smooth_type.tecvcv.smooth` <- function(smooth) {
  "Tensor (concave, concave)"
}

#' @export
#' @rdname smooth_type
`smooth_type.tecxcv.smooth` <- function(smooth) {
  "Tensor (convex, concave)"
}

#' @export
#' @rdname smooth_type
`smooth_type.tesmi1.smooth` <- function(smooth) {
  "Tensor (inc, free)"
}

#' @export
#' @rdname smooth_type
`smooth_type.tesmi2.smooth` <- function(smooth) {
  "Tensor (free, inc)"
}

#' @export
#' @rdname smooth_type
`smooth_type.tesmd1.smooth` <- function(smooth) {
  "Tensor (dec, free)"
}

#' @export
#' @rdname smooth_type
`smooth_type.tesmd2.smooth` <- function(smooth) {
  "Tensor (free, dec)"
}

#' @export
#' @rdname smooth_type
`smooth_type.tescx.smooth` <- function(smooth) {
  "Tensor (free, convex)"
}

#' @export
#' @rdname smooth_type
`smooth_type.tescv.smooth` <- function(smooth) {
  "Tensor (free, concave)"
}

#' @export
#' @rdname smooth_type
`smooth_type.tismi.smooth` <- function(smooth) {
  "Tensor int. (inc, free)"
}

#' @export
#' @rdname smooth_type
`smooth_type.tismd.smooth` <- function(smooth) {
  "Tensor int. (dec, free)"
}

#' @export
#' @rdname smooth_type
`smooth_type.mpiBy.smooth` <- smooth_type.mpi.smooth

#' @export
#' @rdname smooth_type
`smooth_type.mpdBy.smooth` <- smooth_type.mpd.smooth

#' @export
#' @rdname smooth_type
`smooth_type.cxBy.smooth` <- smooth_type.cx.smooth

#' @export
#' @rdname smooth_type
`smooth_type.cvBy.smooth` <- smooth_type.cv.smooth

#' @export
#' @rdname smooth_type
`smooth_type.micxBy.smooth` <- smooth_type.micx.smooth

#' @export
#' @rdname smooth_type
`smooth_type.micvBy.smooth` <- smooth_type.micv.smooth

#' @export
#' @rdname smooth_type
`smooth_type.mdcxBy.smooth` <- smooth_type.mdcx.smooth

#' @export
#' @rdname smooth_type
`smooth_type.mdcvBy.smooth` <- smooth_type.mdcv.smooth
