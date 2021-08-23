#' Set rows of data to `NA` if the lie too far from a reference set of values
#'
#' @param smooth an mgcv smooth object
#' @param input data frame containing the input observations and the columns to
#'   be set to `NA`
#' @param reference data frame containing the reference values
#' @param cols character vector of columns whose elements will be set to `NA` if
#'   the data lies too far from the reference set
#' @param dist numeric, the distance from the reference set beyond which
#'   elements of `input` will be set to `NA`
#'
#' @export
`too_far_to_na` <- function(smooth, input, reference, cols, dist = NULL) {
    UseMethod("too_far_to_na")
}

#' @export
#' @importFrom dplyr mutate across
#' @importFrom tidyselect all_of
`too_far_to_na.mgcv.smooth` <- function(smooth, input, reference, cols,
                                        dist = NULL) {
    # only for smooths of 2D currently
    sm_dim <- smooth_dim(smooth)
    if (sm_dim < 2L || sm_dim > 3L) {
        return(input)
    }

    # call too_far to identify the observations too far from reference
    input <- .call_too_far(smooth = smooth, input = input,
                           reference = reference, cols = cols, dist = dist)

    # return
    input
}

#' @export
#' @importFrom dplyr mutate across
#' @importFrom tidyselect all_of
`too_far_to_na.t2.smooth` <- function(smooth, input, reference, cols,
                                      dist = NULL) {
    # only for smooths of 2D currently
    sm_dim <- smooth_dim(smooth)
    if (sm_dim < 2L || sm_dim > 3L) {
        return(input)
    }

    # call too_far to identify the observations too far from reference
    input <- .call_too_far(smooth = smooth, input = input,
                           reference = reference, cols = cols, dist = dist)

    # return
    input
}

#' @export
#' @importFrom dplyr mutate across
#' @importFrom tidyselect all_of
`too_far_to_na.tensor.smooth` <- function(smooth, input, reference, cols,
                                          dist = NULL) {
    # only for smooths of 2D currently
    sm_dim <- smooth_dim(smooth)
    if (sm_dim < 2L || sm_dim > 3L) {
        return(input)
    }

    # call too_far to identify the observations too far from reference
    input <- .call_too_far(smooth = smooth, input = input,
                           reference = reference, cols = cols, dist = dist)

    # return
    input
}

#' Exclude values that lie too far from the support of data
#'
#' Identifies pairs of covariate values that lie too far from the original data.
#' The function is currently a basic wrapper around [mgcv::exclude.too.far()].
#'
#' @param x,y numeric; vector of values of the covariates to compare with
#'   the observed data
#' @param ref_1,ref_2 numeric; vectors of covariate values that represent the
#'   reference against which `x1 and `x2` are compared
#' @param dist if supplied, a numeric vector of length 1 representing the
#'   distance from the data beyond which an observation is excluded. For
#'   example, you want to exclude values that lie further from an observation
#'   than 10% of the range of the observed data, use `0.1`.
#'
#' @return Returns a logical vector of the same length as `x1`.
#'
#' @importFrom mgcv exclude.too.far
#' @export
`too_far` <- function(x, y, ref_1, ref_2, dist = NULL) {
    ind <- if (is.null(dist)) {
        # If `NULL` keep everything == vector of TRUEs of correct length
        rep(TRUE, length.out = length(x))
    } else {
        # if `dist` is provided, check it is of the correct kind
        if (!is.numeric(dist) || !identical(length(dist), 1L)) {
            stop("'dist', if provided, must be a single numeric value.",
                 call. = FALSE)
        }
        # call exclude.too.far
        mgcv::exclude.too.far(x, y, ref_1, ref_2, dist = dist)
    }

    # return
    ind
}


#' Sets the elements of vector to `NA`
#'
#' Given a vector `i` indexing the elements of `x`, sets the selected elements
#' of `x` to `NA`.
#' 
#' @param x vector of values
#' @param i vector of values used to subset `x`
#' 
#' @return Returns `x` with possibly some elements set to `NA`
#' 
#' @export
`to_na` <- function(x, i) {
    # check x and i are of the same length
    if (identical(length(x), length(i))) {
        # set the indicated elements of `x` to `NA`
        x[i] <- NA
    } else {
        stop("'x' and 'i' must be the same legnth.")
    }
    x
}

#' Set up and call `too_far` on the supplied input
#'
#' @param smooth an mgcv smooth object
#' @param input data frame containing the input observations and the columns to
#'   be set to `NA`
#' @param reference data frame containing the reference values
#' @param cols character vector of columns whose elements will be set to `NA` if
#'   the data lies too far from the reference set
#' @param dist numeric, the distance from the reference set beyond which
#'   elements of `input` will be set to `NA`
#' 
#' @noRd
#' 
#' @keywords internal
`.call_too_far` <- function(smooth, input, reference, cols, dist) {
    # Handle the case where `input` is a nested tibble & data are in $data
    was_nested <- FALSE
    if (!is.null(input[["data"]]) &&
        (inherits(input, "tbl_df") && is.list(input[["data"]]))) {
        input <- unnest(input, cols = all_of("data"))
        was_nested <- TRUE
    }
    # what variables do we need to work on
    sm_vars <- smooth_variable(smooth)
    # indicator for observations too far from grid
    ind <- too_far(x = input[[sm_vars[1L]]],
                   y = input[[sm_vars[2L]]],
                   ref_1 = reference[[sm_vars[1L]]],
                   ref_2 = reference[[sm_vars[2L]]],
                   dist = dist)
    # set the indicated columns to `NA`
    input <- mutate(input, across(all_of(cols), to_na, i = ind))

    # nest `input` again if it was nested previously
    if (was_nested) {
        input <- nest(input, data = !all_of(c("smooth", "type", "by")))
    }

    # return
    input
}