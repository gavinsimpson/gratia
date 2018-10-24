##' Add confidence intervals to objects
##'
##' @param object
##' @param ...
`add_confint` <- function(object, ...) {
    UseMethod("add_confint")
}

##' @rdname add_confint
##'
##' @export
`add_confint.default` <- function(object, ...) {
    stop("Don't know how to add confidence intervals to <",
         class(object)[[1L]], ">", call. = FALSE)
}

##' @param level numeric; the coverage of the intervals
##' @param type character; the type of interval to compute. One of `"confidence"`
##'   for point-wise intervals, or `"simultaneous"` for simultaneous intervals.
##'
##' @rdname add_confint
##'
##' @export
##'
##' @importFrom tibble add_column
`add_confint.derivatives` <- function(object,
                                      level = 0.95,
                                      type = c("confidence", "simultaneous"),
                                      ...) {
    type <- match.arg(type)

    if (type == "confidence") {
        crit <- coverage_qnorm(level)
        result <- add_column(object,
                             lower = object[["derivative"]] - (crit * object[["se"]]),
                             upper = object[["derivative"]] + (crit * object[["se"]]))
    }

    if (type == "simultaneous") {
        stop("Simultaneous intervals not yet implemented for <derivatives>",
             call. = FALSE)
    }

    result
}

`coverage_qnorm` <- function(level) {
    if (level <= 0 || level >= 1 ) {
        stop("Invalid 'level': must be 0 < level < 1", call. = FALSE)
    }
    qnorm((1 - level) / 2, lower.tail = FALSE)
}
