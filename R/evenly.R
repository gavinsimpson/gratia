#' @title Create a sequence of evenly-spaced values
#'
#' @description For a continuous vector `x`, `evenly` and `seq_min_max()`
#'   create a sequence of `n` evenly-spaced values over the range `min(x)`
#'   -- `max(x)`. For a factor `x`, the function returns `levels(x)`.
#'
#' @param x numeric; vector over which evenly-spaced values are returned
#' @param n numeric; the number of evenly-spaced values to return. A default of
#'   `100` is used for convenience as that what is typically used when
#'   evaluating a smooth.
#' @param by numeric; the increment of the sequence. If specified, argument `n`
#'   is ignored and the sequence returned will be from `min(x)` to `max(x)` in
#'   increments of `by`.
#'
#' @return A numeric vector of length `n`.
#'
#' @export
#'
#' @examples
#' \dontshow{set.seed(1)}
#' x <- rnorm(10)
#' n <- 10L
#' evenly(x, n = n)
`evenly` <- function(x, n = 100, by = NULL) {
    out <- if (is.factor(x)) {
        ## must coerce to factor otherwise Predict.matrix will coerce
        ## and that will end up with levels in the wrong order
        ## need to make this ordered if `x` is ordered
        factor(levels(x), levels = levels(x), ordered = is.ordered(x))
    } else {
        if (is.null(by)) {
            seq(from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE),
                length.out = n)
        } else {
            seq(from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE),
                by = by)
        }
    }
    out
}

#' @rdname evenly
`seq_min_max` <- function(x, n, by = NULL) {
    out <- if (is.factor(x)) {
        ## must coerce to factor otherwise Predict.matrix will coerce
        ## and that will end up with levels in the wrong order
        ## need to make this ordered if `x` is ordered
        factor(levels(x), levels = levels(x), ordered = is.ordered(x))
    } else {
        if (is.null(by)) {
            seq(from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE),
                length.out = n)
        } else {
            seq(from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE),
                by = by)
        }
    }
    out
}

#' @title Create a sequence of evenly-spaced values adjusted to accommodate a
#'   small adjustment
#'
#' @description Creates a sequence of `n` evenly-spaced values over the range
#'   `min(x)` -- `max(x)`, where the minimum and maximum are adjusted such that
#'   they are always contained within the range of `x` when `x` may be shifted
#'   forwards or backwards by an amount related to `eps`. This is particularly
#'   useful in computing derivatives via finite differences where without this
#'   adjustment we may be predicting for values outside the range of the data
#'   and hence the conmstraints of the penalty.
#'
#' @param x numeric; vector over which evenly-spaced values are returned
#' @param n numeric; the number of evenly-spaced values to return
#' @param eps numeric; the finite difference
#' @param order integer; the order of derivative. Either `1` or `2` for first or
#'   second order derivatives
#' @param type character; the type of finite difference used. One of
#'   `"forward"`, `"backward"`, or `"central"`
#'
#' @return A numeric vector of length `n`.
`seq_min_max_eps` <- function(x, n, order,
                              type = c("forward", "backward", "central"), eps) {
    minx <- min(x, na.rm = TRUE)
    maxx <- max(x, na.rm = TRUE)
    heps <- eps / 2
    deps <- eps * 2
    type <- match.arg(type)
    if (isTRUE(all.equal(order, 1L))) {
        minx <- switch(type,
            forward  = minx,
            backward = minx + eps,
            central  = minx + heps)
        maxx <- switch(type,
            forward  = maxx - eps,
            backward = maxx,
            central  = maxx - heps)
    } else {
        minx <- switch(type,
            forward  = minx,
            backward = minx + deps,
            central  = minx + eps)
        maxx <- switch(type,
            forward  = maxx - deps,
            backward = maxx,
            central  = maxx - eps)
    }
    seq(from = minx, to = maxx, length.out = n)
}

#' @title Return the reference or specific level of a factor
#'
#' @description Extracts the reference or a specific level the supplied factor,
#'   returning it as a factor with the same levels as the one supplied.
#'
#' @param fct factor; the factor from which the reference or specific level will
#'   be extracted.
#' @param level character; the specific level to extract in the case of
#'   `level()`.
#'
#' @return A length 1 factor with the same levels as the supplied factor `fct`.
#'
#' @export
#'
#' @examples
#' \dontshow{set.seed(1)}
#' f <- factor(sample(letters[1:5], 100, replace = TRUE))
#'
#' # the reference level
#' ref_level(f)
#'
#' # a specific level
#' level(f, level = "b")
#'
#' # note that the levels will always match the input factor
#' identical(levels(f), levels(ref_level(f)))
#' identical(levels(f), levels(level(f, "c")))
#' @export
`ref_level` <- function(fct) {
    if (!is.factor(fct)) {
        stop("'fct' must be a factor")
    }
    lev <- levels(fct)
    factor(lev[1], levels = lev)
}
#' @export
#' @rdname ref_level
`level` <- function(fct, level) {
    if (!is.factor(fct)) {
        stop("'fct' must be a factor")
    }
    lev <- levels(fct)
    if (!level %in% lev) {
        stop("Level <", level, "> not a valid level of factor")
    }
    factor(level, levels = lev)
}
