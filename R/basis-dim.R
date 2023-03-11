#' Extract basis dimension of a smooth
#'
#' @param object A fitted GAM(M). Currently [mgcv::gam()] (and anything that
#' inherits from the `"gam"` class, e.g. [mgcv::bam()]) and [mgcv::gamm()] are
#' supported.
#' @param ... Arguments passed to other methods.
#'
#' @examples
#'
#' load_mgcv()
#'
#' df <- data_sim("eg1", n = 200, seed = 1)
#' m <- bam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df)
#'
#' basis_dim(m)
`basis_dim` <- function(object, ...) {
    UseMethod("basis_dim")
}

#' @export
#' @rdname basis_dim
`basis_dim.mgcv.smooth` <- function(object, ...) {
     check_is_mgcv_smooth(object)
     object[["df"]]
}

#' @export
#' @rdname basis_dim
`basis_dim.gam` <- function(object, ...) {
    dims <- vapply(object$smooth, basis_dim, double(1))
    dims <- setNames(dims, smooths(object))
    dims
}

#' @export
#' @rdname basis_dim
`basis_dim.gamm` <- function(object, ...) {
    dims <- vapply(object$gam$smooth, basis_dim, double(1))
    dims <- setNames(dims, smooths(object$gam))
    dims
}
