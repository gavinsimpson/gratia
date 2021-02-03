##' Effective degrees of freedom for smooth terms
##'
##' Extracts the effective degrees of freedom (EDF) for model smooth terms
##'
##' @details Wood (2017; pp. 252) describes an alternative EDF describes an
##'   alternative EDF form for the entire model
##'   \deqn{\mathrm{EDF} = 2\mathrm{tr}(\mathbf{F}) -
##'   \mathrm{tr}(\mathbf{FF}),}{EDF = 2 * tr(F) - tr(F),} where
##'   \eqn{\mathrm{tr}} is the matrix trace and \eqn{\mathbf{F}}{F} is a matrix
##'   mapping un-penalized coefficient estimates to the penalized coefficient
##'   estimates.  The trace of \eqn{\mathbf{F}}{F} is effectively the average
##'   shrinkage of the coefficients multipled by the number of coefficients
##'   (Wood, 2017). Smooth-specific EDFs then are obtained by summing up the
##'   relevent elements of \eqn{\mathrm{diag}(2\mathbf{F} - \mathbf{FF})}.
##'
##' @param object a fitted model from which to extract smooth-specific EDFs.
##' @param ... arguments passed to methods.
##'
##' @export
##'
##' @examples
##' load_mgcv()
##' df <- data_sim("eg1", n = 400, seed = 42)
##' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
##' edf(m)
##' edf(m, smooth = c("s(x0)", "s(x2)"))
`edf` <- function(object, ...) {
    UseMethod("edf")
}

##' @param smooth character; a vector of smooth terms whose EDFs will be
##'   extracted. If `NULL`, the default, EDFs for all smooths will be returned.
##' @param alternative logical; return the alternative form for model EDFs of
##'   Wood (2017; pp. 252).
##' 
##' @export
##' @importFrom tibble tibble
##' @rdname edf
`edf.gam` <- function(object, smooth = NULL, alternative = FALSE,
                      ...) {
    ## if particular smooths selected
    sm_ids <- if (!is.null(smooth)) {
        which_smooths(object, smooth) # which smooths match 'smooth'
    } else {
        seq_len(n_smooths(object))
    }
    n_sm <- length(sm_ids) # how many smooths?
    ## extract the EDF:
    ## - object$edf is the standard EDF for a GAM in mgcv, reported in
    ##     summary(model) output
    ## - object$edf1 is the alternative EDF metioned in Wood's GAM book,
    ##     2ed pp 252: 2*trace(F) - trace(F%*%F)
    ##     This doesn't seem to be used at all in `summary.gam()` so not
    ##     sure why Simon extracts it
    take <- if(isTRUE(alternative)) {
        "edf1"
    } else {
        "edf"
    }
    edf_vec <- object[[take]]
    edf_out <- numeric(length = n_sm)
    sm_labs <- smooths(object)[sm_ids]
    for (i in seq_along(edf_out)) {
        paras <- smooth_coefs(object[["smooth"]][[sm_ids[i]]])
        edf_out[i] <- sum(edf_vec[paras])
    }
    edf_out <- tibble(smooth = sm_labs, edf = edf_out)
    edf_out
}
