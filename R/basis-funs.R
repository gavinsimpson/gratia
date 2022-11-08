#' Basis expansions for smooths
#'
#' Creates a basis expansion from a definition of a smoother using the syntax
#'   of *mgcv*'s smooths via [mgcv::s()]., [mgcv::te()], [mgcv::ti()], and
#'   [mgcv::t2()], or from a fitted GAM(M).
#'
#' @param object a smooth specification, the result of a call to one of
#'   [mgcv::s()]., [mgcv::te()], [mgcv::ti()], or [mgcv::t2()], or a fitted
#'   GAM(M) model.
#' @param term character; select smooths in a fitted model
#' @param data a data frame containing the variables used in `smooth`.
#' @param n numeric; the number of points over the range of the covariate at
#'   which to evaluate the smooth.
#' @param n_2d numeric; the number of new observations for each dimension of a
#'   bivariate smooth. Not currently used; `n` is used for both dimensions.
#' @param n_3d numeric; the number of new observations to generate for the third
#'   dimension of a 3D smooth.
#' @param n_4d numeric; the number of new observations to generate for the
#'   dimensions higher than 2 (!) of a *k*D smooth (*k* >= 4). For example, if
#'   the smooth is a 4D smooth, each of dimensions 3 and 4 will get `n_4d`
#'   new observations.
#' @param knots a list or data frame with named components containing
#'   knots locations. Names must match the covariates for which the basis
#'   is required. See [mgcv::smoothCon()].
#' @param constraints logical; should identifiability constraints be applied to
#'   the smooth basis. See argument `absorb.cons` in [mgcv::smoothCon()].
#' @param at a data frame containing values of the smooth covariate(s) at which
#'   the basis should be evaluated.
#' @param ... other arguments passed to [mgcv::smoothCon()].
#'
#' @inheritParams smooth_estimates
#'
#' @return A tibble.
#'
#' @author Gavin L. Simpson
#'
#' @export
#'
#' @importFrom mgcv smoothCon
#' @importFrom dplyr bind_rows
#'
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(digits = 3, cli.unicode = FALSE)
#' }
#' df <- data_sim("eg4", n = 400, seed = 42)
#'
#' bf <- basis(s(x0), data = df)
#' bf <- basis(s(x2, by = fac, bs = "bs"), data = df, constraints = TRUE)
#' \dontshow{
#' options(op)
#' }
`basis` <- function(object, ...) {
    UseMethod("basis")
}

#' @export
#'
#' @rdname basis
#'
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom stats coef
`basis.gam` <- function(object, term = NULL, data = NULL,
                        n = 100, n_2d = 50, n_3d = 16, n_4d = 4,
                        partial_match = FALSE,
                        ...) {
    model_name <- expr_label(substitute(object))
    # if particular smooths selected
    S <- smooths(object) # vector of smooth labels - "s(x)"

    # select smooths
    select <-
        check_user_select_smooths(smooths = S, select = term,
            partial_match = partial_match,
            model_name = model_name)
    smooth_ids <- which(select)

    # extract the mgcv.smooth objects
    smooths <- get_smooths_by_id(object, smooth_ids)

    # if user data supplied, check for and remove response
    if (!is.null(data)) {
        if (!is.data.frame(data)) {
            stop("'data', if supplied, must be a numeric vector or data frame.",
                call. = FALSE)
        }
        check_all_vars(object, data = data, smooths = smooths)
        data <- delete_response(object, data = data)
    }

    bfuns <- map(seq_along(smooths), tidy_basis_wrapper, ids = smooth_ids,
        data = data, smooths = smooths, model = object, n = n,
        n_3d = n_3d, n_4d = n_4d, offset = NULL)

    bfuns <- bind_rows(bfuns)

    ## class this up
    class(bfuns) <- append(class(bfuns), c("basis"),
        after = 0L)

    bfuns
}

#' @export
#' @rdname basis
`basis.scam` <- function(object, term = NULL, data = NULL,
                         n = 100, n_2d = 50, n_3d = 16, n_4d = 4,
                         partial_match = FALSE,
                         ...) {
    model_name <- expr_label(substitute(object))
    # if particular smooths selected
    S <- smooths(object) # vector of smooth labels - "s(x)"

    # select smooths
    select <-
        check_user_select_smooths(smooths = S, select = term,
            partial_match = partial_match,
            model_name = model_name)
    smooth_ids <- which(select)

    # extract the mgcv.smooth objects
    smooths <- get_smooths_by_id(object, smooth_ids)

    # if user data supplied, check for and remove response
    if (!is.null(data)) {
        if (!is.data.frame(data)) {
            stop("'data', if supplied, must be a numeric vector or data frame.",
                call. = FALSE)
        }
        check_all_vars(object, data = data, smooths = smooths)
        data <- delete_response(object, data = data)
    }

    bfuns <- map(seq_along(smooths), tidy_basis_wrapper, ids = smooth_ids,
        data = data, smooths = smooths, model = object, n = n,
        n_3d = n_3d, n_4d = n_4d, offset = NULL)

    bfuns <- bind_rows(bfuns)

    ## class this up
    class(bfuns) <- append(class(bfuns), c("basis"),
        after = 0L)

    bfuns
}

#' @export
#' @rdname basis
`basis.gamm` <- function(object, term = NULL, data = NULL,
                         n = 100, n_2d = 50, n_3d = 16, n_4d = 4,
                         partial_match = FALSE,
                         ...) {
    basis(object[["gam"]], term = term, data = data, n = n,
        n_2d = n_2d, n_3d = n_3d, n_4d = n_4d, partial_match = partial_match,
        ...)
}

#' @export
#' @rdname basis
`basis.list` <- function(object, term = NULL, data = NULL,
                          n = 100, n_2d = 50, n_3d = 16, n_4d = 4,
                          partial_match = FALSE,
                          ...) {
    if (!is_gamm4(object)) {
        stop("'object' is a list but doesn't appear to be a 'gamm4()' model.")
    }
    basis(object[["gam"]], term = term, data = data, n = n,
        n_2d = n_2d, n_3d = n_3d, n_4d = n_4d, partial_match = partial_match,
        ...)
}

# wrapper for calling tidy_basis
`tidy_basis_wrapper` <- function(i, smooths, ids, data = NULL, model,
                                 n = 100, n_2d = NULL, n_3d = NULL,
                                 n_4d = NULL,
                                 offset = NULL) {
    # which coefs belong to this smooth
    take <- smooth_coef_indices(smooths[[i]])
    betas <- coef(model)[take]
    # handle scam models - p.ident is a logical vector with length == NCOL(Xp)
    # where Xp is the Lp matrix of the entire model!
    p_ident <- model$p.ident
    if (! is.null(p_ident)) { # must be a scam model
        # need the 1st element of p_ident == FALSE, & represents the intercept.
        # In scam, intercept gets returned by the Predict.matrix.<basis>.smooth
        # functions (from PredictMat() used in tidy_basis()), and we need to
        # know this to drop it later
        p_ident <- p_ident[c(1L, take)]
    }

    if (is.null(data)) {
        data <- smooth_data(model, ids[i], n = n, n_3d = n_3d, n_4d = n_4d,
            offset = offset)
    }

    tbl <- tidy_basis(smooths[[i]], at = data, coefs = betas,
        p_ident = p_ident)

    ## return
    tbl
}

#' @export
#' @rdname basis
#' @importFrom stringr str_detect fixed
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
`basis.default` <- function(object, data, knots = NULL, constraints = FALSE,
                            at = NULL, ...) {
    # class of object and check for ".smooth.spec"
    cls <- class(object)
    if (str_detect(cls, "smooth.spec", negate = TRUE)) {
        stop("'object' doesn't appear to be a smooth created by {mgcv}.")
    }
    ## call smoothCon to create the basis as specified in `x`
    sm <- smoothCon(object, data = data, knots = knots,
        absorb.cons = constraints, ...)

    ## sm will be a list, even if a single smooth, bc we could have multiple
    ## smoothers in case of factor `by` smooths.
    ## Need to walk the list and convert the design matrix `X` to a tidy form
    if (is.null(at)) {
        at <- data
    }
    bfuns <- map(sm, tidy_basis, data = data, at = at)

    ## rebind
    bfuns <- bind_rows(bfuns)

    ## class
    class(bfuns) <- c("basis", class(bfuns))

    ## store the basis definition as an attribute
    attr(bfuns, "smooth_object") <- deparse(substitute(object))

    ## return
    bfuns
}

#' A tidy basis representation of a smooth object
#'
#' Takes an object of class `mgcv.smooth` and returns a tidy representation
#' of the basis.
#'
#' @param smooth a smooth object of or inheriting from class `"mgcv.smooth"`.
#'   Typically, such objects are returned as part of a fitted GAM or GAMM in
#'   the `$smooth` component of the model object or the `$gam$smooth` component
#'   if the model was fitted by [mgcv::gamm()] or [gamm4::gamm4()].
#' @param data a data frame containing the variables used in `smooth`.
#' @param at a data frame containing values of the smooth covariate(s) at which
#'   the basis should be evaluated.
#' @param coefs numeric; an optional vector of coefficients for the smooth
#' @param p_ident logical vector; only used for handling [scam::scam()] smooths.
#'
#' @return A tibble.
#' @author Gavin L. Simpson
#'
#' @importFrom tibble as_tibble add_column
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr bind_cols everything select matches
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(digits = 3, cli.unicode = FALSE)
#' }
#' df <- data_sim("eg1", n = 400, seed = 42)
#'
#' # fit model
#' m  <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
#'
#' # tidy representaition of a basis for a smooth definition
#' # extract the smooth
#' sm <- get_smooth(m, "s(x2)")
#' # get the tidy basis - need to pass where we want it to be evaluated
#' bf <- tidy_basis(sm, at = df)
#'
#' # can weight the basis by the model coefficients for this smooth
#' bf <- tidy_basis(sm, at = df, coefs = smooth_coefs(sm, model = m))
#' \dontshow{
#' options(op)
#' }
`tidy_basis` <- function(smooth, data = NULL, at = NULL, coefs = NULL,
                         p_ident = NULL) {
    check_is_mgcv_smooth(smooth) # check `smooth` is of the correct type
    if (is_mgcv_smooth(smooth) && is.null(at)) {
        if (!is.null(data)) {
            warning("'smooth' is an \"mgcv.smooth\" but you supplied 'data'\n",
                "'at' needs to be supplied; using 'data' as 'at'.")
            at <- data
        } else {
            stop("When 'smooth' is an \"mgcv.smooth\", 'at' must be supplied.")
        }
    }
    if (is.null(at)) {
        if (is.null(data)) {
            stop("One of 'data' or 'at' must be supplied.")
        }
        tbl <- smooth[["X"]]         # extract the model matrix
    } else {
        tbl <- PredictMat(smooth, data = at)
        data <- at
        if (! is.null(coefs)) {
            nc <- NCOL(tbl)
            if (! is.null(p_ident)) {
                stopifnot(identical(sum(p_ident), length(coefs)))
                tbl <- tbl[, p_ident]
            }
            stopifnot(identical(NCOL(tbl), length(coefs)))
            # weight the basis functions at supplied coefficients
            tbl <- sweep(tbl, MARGIN = 2, STATS = coefs, FUN = "*")
        }
    }
    nfun <- NCOL(tbl)        # the number of basis functions
    colnames(tbl) <- seq_len(nfun)
    tbl <- as_tibble(tbl) # convert to tibbles
    data <- as_tibble(data)
    is_by_fac <- is_factor_by_smooth(smooth) # is this a factor by smooth?

    ## If we have a factor by smooth, the model matrix `X` contains 0 everywhere
    ##   that an observation is not from the level for the selected smooth.
    ## Here we filter out those observations from `X` and the `data`
    if (is_by_fac) {
        by_var <- by_variable(smooth)
        by_lev <- by_level(smooth)
        take <- data[[by_var]] == by_lev
        tbl <- tbl[take, ]
        data <- data[take, ]
    }

    ## Add the data to `tbl`; need it later for plotting etc, but only keep
    ##  the variables involved in this smooth
    sm_data <- data[, smooth_variable(smooth)]
    tbl <- bind_cols(tbl, sm_data)

    ## convert to long-form; select the first nfun cols to be gathered,
    ##   rest should be the variable(s) involved in the smooth
    tbl <- tbl %>%
        pivot_longer(seq_len(nfun), names_to = "bf", values_to = "value") %>%
        mutate(bf = factor(.data[["bf"]], levels = seq_len(nfun)))

    ## reorder cols so we have the basis function & value first, data last
    tbl <- select(tbl, matches("bf"), matches("value"), everything())

    ## Add on an identifier for the smooth
    tbl <- add_column(tbl, smooth = smooth_label(smooth), .before = 1L)

    ## Need a column for by smooths; will be NA for most smooths
    by_var <- rep(NA_character_, length = nrow(tbl))
    if (is_by_smooth(smooth)) {
        by_var <- rep(by_variable(smooth), length = nrow(tbl))
        ## If we have a factor by we need to store the factor. Not needed
        ##   for other by variable smooths.
        if (is_by_fac) {
            tbl <- add_column(tbl, ..xx.. = rep(by_level(smooth), nrow(tbl)))
            names(tbl)[NCOL(tbl)] <- by_variable(smooth)
        }
    }

    ## actually add on the by variable info
    tbl <- add_column(tbl, by_variable = by_var, .after = 1L)

    # add on the smooth type
    sm_type <- rep(smooth_type(smooth), length.out = nrow(tbl))
    tbl <- add_column(tbl, type = sm_type, .after = 1L)

    ## class this up
    class(tbl) <- append(class(tbl), gsub("\\.", "_", class(smooth)),
        after = 0L)

    ## return
    tbl
}
