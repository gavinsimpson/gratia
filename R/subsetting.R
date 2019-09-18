##' @export
`[.evaluated_smooth` <- function(x, i, j, drop = FALSE) {
    cls <- class(x)
    class(x) <- class(x)[-c(1:2)]
    x <- NextMethod()
    class(x) <- cls
    x
}

##' @export
`[.smooth_samples` <- function(x, i, j, drop = FALSE) {
    cls <- class(x)
    seed <- attr(x, "seed")
    data_names <- attr(x, "data_names")
    class(x) <- class(x)[-c(1:2)]
    x <- NextMethod()
    class(x) <- cls
    take <- unique(x$smooth)
    data_names <- data_names[take]
    attr(x, "seed") <- seed
    attr(x, "data_names") <- data_names
    x
}
