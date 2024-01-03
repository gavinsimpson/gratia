#' @export
`[.evaluated_smooth` <- function(x, i, j, drop = FALSE) {
    cls <- class(x)
    class(x) <- class(x)[-c(1:2)]
    x <- NextMethod()
    class(x) <- cls
    x
}

#' @export
#' @importFrom rlang has_name
`[.smooth_samples` <- function(x, i, j, drop = FALSE) {
    cls <- class(x)
    seed <- attr(x, "seed")
    data_names <- attr(x, "data_names")
    class(x) <- class(x)[-c(1:2)]
    x <- NextMethod()
    class(x) <- cls
    if (has_name(x, "smooth")) {
        take <- unique(x[["smooth"]])
        data_names <- data_names[take]
    } else {
        data_names <- NA
    }
    attr(x, "seed") <- seed
    attr(x, "data_names") <- data_names
    x
}

#' @export
#' @importFrom rlang has_name
`[.eval_sm` <- function(x, i, j, drop = FALSE) {
    cls <- class(x)
    tensor_term_order <- attr(x, "tensor_term_order")
    class(x) <- class(x)[-c(1:2)]
    x <- NextMethod()
    class(x) <- cls
    if (has_name(x, ".smooth")) {
        take <- unique(x[[".smooth"]])
        tensor_term_order <- tensor_term_order[take]
    } else {
        tensor_term_order <- NA
    }
    attr(x, "tensor_term_order") <- tensor_term_order
    x
}

#' @export
#' @importFrom rlang has_name
`[.smooth_estimates` <- function(x, i, j, drop = FALSE) {
    cls <- class(x)
    tensor_term_order <- attr(x, "tensor_term_order")
    class(x) <- class(x)[-c(1:2)]
    x <- NextMethod()
    class(x) <- cls
    if (has_name(x, ".smooth")) {
        take <- unique(x[[".smooth"]])
        tensor_term_order <- tensor_term_order[take]
    } else {
        tensor_term_order <- NA
    }
    attr(x, "tensor_term_order") <- tensor_term_order
    x
}
