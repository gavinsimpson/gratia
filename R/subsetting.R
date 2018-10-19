##' @export
`[.evaluated_smooth` <- function(x, i, j, drop = FALSE) {
    cls <- class(x)
    class(x) <- class(x)[-c(1:2)]
    x <- NextMethod()
    class(x) <- cls
    x
}

