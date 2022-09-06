#' Effective degrees of freedom for smooths and GAMs
#'
#' Extracts the effective degrees of freedom (EDF) for model smooth terms or
#' overall EDF for fitted GAMs
#'
#' @details Multiple formulations for the effective degrees of freedom are
#'   available. The additional uncertainty due to selection of smoothness
#'   parameters can be taken into account when computing the EDF of smooths.
#'   This form of the EDF is available with `type = "unconditional"`.
#'
#'   Wood (2017; pp. 252) describes an alternative EDF for the  model
#'   \deqn{\mathrm{EDF} = 2\mathrm{tr}(\mathbf{F}) -
#'   \mathrm{tr}(\mathbf{FF}),}{EDF = 2 * tr(F) - tr(F),} where
#'   \eqn{\mathrm{tr}} is the matrix trace and \eqn{\mathbf{F}}{F} is a matrix
#'   mapping un-penalized coefficient estimates to the penalized coefficient
#'   estimates.  The trace of \eqn{\mathbf{F}}{F} is effectively the average
#'   shrinkage of the coefficients multipled by the number of coefficients
#'   (Wood, 2017). Smooth-specific EDFs then are obtained by summing up the
#'   relevent elements of \eqn{\mathrm{diag}(2\mathbf{F} - \mathbf{FF})}.
#'
#' @param object a fitted model from which to extract smooth-specific EDFs.
#' @param smooth character; a vector of smooth terms whose EDFs will be
#'   extracted. If `NULL`, the default, EDFs for all smooths will be returned.
#' @param type character: which type of EDF to return. `"default"` returns the
#'   standard EDF; `"unconditional"` selects the EDF corrected for smoothness
#'   parameter selection, if available; `"alternative"` returns the alternative
#'   formulation for EDF from Wood (2017, pp. 252)
#' @param ... arguments passed to methods.
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{op <- options(cli.unicode = FALSE, digits = 6)}
#' df <- data_sim("eg1", n = 400, seed = 42)
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
#'
#' # extract the EDFs for all smooths
#' edf(m)
#'
#' # or selected smooths
#' edf(m, smooth = c("s(x0)", "s(x2)"))
#'
#' # accounting for smoothness parameter uncertainty
#' edf(m, type = "unconditional")
#'
#' # over EDF of the model, including the intercept
#' model_edf(m)
#'
#' # can get model EDF for multiple models
#' m2 <- gam(y ~ s(x0) + s(x1) + s(x3), data = df, method = "REML")
#' model_edf(m, m2)
#' \dontshow{options(op)}
`edf` <- function(object, ...) {
    UseMethod("edf")
}

#' @export
#' @importFrom tibble tibble
#' @rdname edf
`edf.gam` <- function(object, smooth = NULL,
                      type = c("default", "unconditional", "alternative"),
                      ...) {
    ## which type of EDF?
    type <- match.arg(type)
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
    ## - object$edf1 is the alternative EDF mentioned in Wood's GAM book,
    ##     2ed pp 252: 2*trace(F) - trace(F%*%F)
    ##     This doesn't seem to be used at all in `summary.gam()` so not
    ##     sure why Simon extracts it
    ## - object$edf2 is an EDF that accounts for smoothness parameter
    ##     uncertainty; only available for REML and ML fits
    edf_vec <- extract_edf(object, type, sum = FALSE)
    edf_out <- numeric(length = n_sm)
    sm_labs <- smooths(object)[sm_ids]
    for (i in seq_along(edf_out)) {
        paras <- smooth_coef_indices(object[["smooth"]][[sm_ids[i]]])
        edf_out[i] <- sum(edf_vec[paras])
    }
    edf_out <- tibble(smooth = sm_labs, edf = edf_out)
    edf_out
}

#' @rdname edf
#' @importFrom rlang enexpr ensyms expr_text
#' @importFrom tibble tibble
#' @export
`model_edf` <- function(object, ...,
                        type = c("default", "unconditional", "alternative")) {
    ## grab ...
    model_names <- c(expr_text(enexpr(object)),
                               unname(vapply(ensyms(...), expr_text,
                                             character(1))))
    dots <- list(...)
    ## combine model and others into a list
    models <- append(list(object), dots)

    ## match type
    type <- match.arg(type)

    ## loop over models and extract the requested EDF
    edfs <- vapply(models, FUN = extract_edf, FUN.VALUE = numeric(1L),
                   sum = TRUE, type = type)

    ## prepare output tibble
    tibble(model = model_names, edf = edfs)
}

`extract_edf` <- function(object, type = "default", sum = FALSE, ...) {
    # if inherits from class gamm, then subset
    if (is_gamm(object) || is_gamm4(object)) {
        object <- object[["gam"]]
    }
    ## extract the EDF:
    ## - object$edf is the standard EDF for a GAM in mgcv, reported in
    ##     summary(model) output
    ## - object$edf1 is the alternative EDF metioned in Wood's GAM book,
    ##     2ed pp 252: 2*trace(F) - trace(F%*%F)
    ##     This doesn't seem to be used at all in `summary.gam()` so not
    ##     sure why Simon extracts it
    ## - object$edf2 is an EDF that acocunts for smoothness parameter
    ##     uncertainty; only available for REML and ML fits
    take <- switch(type,
                   default = "edf",
                   unconditional = "edf2",
                   alternative = "edf1")
    edf_vec <- object[[take]]
    ## if edf2 is NULL, revert to edf
    if (is.null(edf_vec)) {
        edf_vec <- object[["edf"]]
        warning("Smoothness parameter uncertainty unavailable;",
                " using `type = \"default\"`")
    }
    if (sum) {
        edf_vec <- sum(edf_vec)
    }
    edf_vec
}
