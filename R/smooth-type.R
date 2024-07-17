#' Determine the type of smooth and return it n a human readable form
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
`smooth_type.sw` <- function(smooth) {
  sm_type <- "Soap film (wiggly)"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.sf` <- function(smooth) {
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
  sm_type <- "Mono inc P spline"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.mpd.smooth` <- function(smooth) {
  sm_type <- "Mono dec P spline"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.cx.smooth` <- function(smooth) {
  sm_type <- "Convex P spline"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.cv.smooth` <- function(smooth) {
  sm_type <- "Concave P spline"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.micx.smooth` <- function(smooth) {
  sm_type <- "Mono inc conv P spline"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.micv.smooth` <- function(smooth) {
  sm_type <- "Mono inc conc P spline"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.mdcx.smooth` <- function(smooth) {
  sm_type <- "Mono dec conv P spline"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.mdcv.smooth` <- function(smooth) {
  sm_type <- "Mono dec conc P spline"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.miso.smooth` <- function(smooth) {
  sm_type <- "Mono inc 0 start P spline"
  sm_type
}

#' @export
#' @rdname smooth_type
`smooth_type.mifo.smooth` <- function(smooth) {
  sm_type <- "Mono inc 0 end P spline"
  sm_type
}
