#' Basis expansions for smooths
#' 
#' Creates a basis expansion from a definition of a smoother using the syntax
#'   of *mgcv*'s smooths via [mgcv::s()]., [mgcv::te()], [mgcv::ti()], and
#'   [mgcv::t2()].
#'
#' @param smooth a smooth specification, the result of a call to one of
#'   [mgcv::s()]., [mgcv::te()], [mgcv::ti()], or [mgcv::t2()].
#' @param data a data frame containing the variables used in `smooth`.
#' @param knots a list or data frame with named components containing
#'   knots locations. Names must match the covariates for which the basis
#'   is required. See [mgcv::smoothCon()].
#' @param constraints logical; should identifiability constraints be applied to
#'   the smooth basis. See argument `absorb.cons` in [mgcv::smoothCon()].
#' @param ... other arguments passed to [mgcv::smoothCon()].
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
#' set.seed(42)
#' op <- options(digits = 3, cli.unicode = FALSE)
#' }
#' df <- gamSim(4, n = 400, verbose = FALSE)
#'
#' bf <- basis(s(x0), data = df)
#' bf <- basis(s(x2, by = fac, bs = 'bs'), data = df, constraints = TRUE)
#' \dontshow{options(op)}
`basis` <- function(smooth, data, knots = NULL, constraints = FALSE,
                    ...) {
    ## call smoothCon to create the basis as specified in `x`
    sm <- smoothCon(smooth, data = data, knots = knots,
                    absorb.cons = constraints, ...)

    ## sm will be a list, even if a single smooth, bc we could have multiple
    ## smoothers in case of factor `by` smooths.
    ## Need to walk the list and convert the design matrix `X` to a tidy form
    bfuns <- lapply(sm, tidy_basis, data = data)

    ## rebind
    bfuns <- bind_rows(bfuns)

    ## class
    class(bfuns) <- c("basis", class(bfuns))

    ## store the basis definition as an attribute
    attr(bfuns, "smooth_object") <- deparse(substitute(smooth))

    ## return
    bfuns
}

#' A tidy basis representation of a smooth object
#'
#' Takes an object of class `mgcv.smooth` and returns a tidy representation
#' of the basis.
#'
#' @param smooth a smooth object.
#' @param data a data frame containing the variables used in `smooth`.
#'
#' @return A tibble.
#' @author Gavin L. Simpson
#'
#' @importFrom tibble as_tibble add_column
#' @importFrom tidyr gather
#' @importFrom dplyr bind_cols everything select matches
`tidy_basis` <- function(smooth, data) {
    check_is_mgcv_smooth(smooth) # check `smooth` is of the correct type
    tbl <- smooth[["X"]]         # extract the model matrix
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
    tbl <- gather(tbl, key = 'bf', value = 'value', seq_len(nfun),
                  factor_key = TRUE)

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

    ## class this up
    class(tbl) <- c(gsub("\\.", "_", class(smooth)), class(tbl))

    ## return
    tbl
}
