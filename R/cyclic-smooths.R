# Fitted cycle boundaries describe the complete period, including portions not
# represented by observations or by a supplied evaluation grid.
cyclic_smooth_info <- function(smooth) {
  variables <- unique(smooth_variable(smooth))
  if (length(variables) != 1L || !identical(as.integer(smooth_dim(smooth)), 1L)) {
    return(NULL)
  }
  margin <- smooth
  if (!is.null(smooth$margin)) {
    if (length(smooth$margin) != 1L) return(NULL)
    margin <- smooth$margin[[1L]]
  }
  knots <- if (inherits(margin, "cyclic.smooth")) {
    margin$xp
  } else if (inherits(margin, c("cpspline.smooth", "cpopspline.smooth"))) {
    margin$knots
  } else {
    return(NULL)
  }
  boundaries <- if (is.numeric(knots) && length(knots) >= 2L &&
      all(is.finite(knots))) range(knots) else c(NA_real_, NA_real_)
  list(variable = variables[[1L]], boundaries = boundaries, type = smooth_type(smooth))
}

smooth_cyclic_info <- function(smooths, ids = NULL) {
  info <- lapply(smooths, cyclic_smooth_info)
  if (!is.null(ids)) {
    for (i in seq_along(info)) {
      if (!is.null(info[[i]])) info[[i]]$id <- ids[[i]]
    }
  }
  names(info) <- vapply(smooths, smooth_label, character(1))
  info[!vapply(info, is.null, logical(1))]
}
