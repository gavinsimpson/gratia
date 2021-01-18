##' Extract and tidy penalty matrices
##'
##' @param object a fitted GAM or a smooth.
##' @param smooth character; vector of smooths to extract penalty matrices for.
##'   If `NULL`, penalty matrices for all smooths in `object` are extracted.
##' @param margins logical; extract the penalty matrices for the tensor
##'   product or the margainl smooths of the tensor product?
##' @param ... additional arguments passed to methods.
##'
##' @author Gavin L. Simpson
##' @export
##'
##' @examples
##' load_mgcv()
##' dat <- data_sim("eg4", n = 400, seed = 42)
##' m <- gam(y ~ s(x0) + s(x1) + s(x2, by = fac),
##'          data = dat, method = "REML")
##' penalty(m)
##'
##' # for a specific smooth
##' penalty(m, smooth = "s(x2):fac1")
`penalty` <- function(object, ...) {
    UseMethod("penalty")
}

##' @export
##' @rdname penalty
`penalty.gam` <- function(object, smooth = NULL, ...) {
    ## are particular smooths selected
    smooth_ids <- if (!is.null(smooth)) {
         which_smooths(object, smooth) # which smooths match 'smooth'
    } else {
        seq_len(n_smooths(object))
    }

    ## extract the mgcv.smooth objects
    smooths <- get_smooths_by_id(object, smooth_ids)

    ## loop over the smooths applying penalty to each
    pen <- lapply(smooths, penalty)
    pen <- bind_rows(pen)
    class(pen) <- c("tidy_penalty", class(pen))
    pen
}

##' @export
##' @rdname penalty
`penalty.mgcv.smooth` <- function(object, ...) {
    ## extract the set of penalty matrices
    S <- object[["S"]] # S is a list even if length(S) == 1
    sp_label <- names(object[["sp"]]) # penalty matrix label
    pen <- vector("list", length = length(S))
    ## loop over penalty matrices & tidy each of them
    for (i in seq_along(pen)) {
        pen[[i]] <- tidy_penalty(S[[i]], sp_label[i])
    }

    ## combine the tidy penalty matrices into a single tibble
    pen <- bind_rows(pen)
    class(pen) <- c("tidy_penalty", class(pen))
    pen
}

##' @export
##' @rdname penalty
`penalty.tensor.smooth` <- function(object, margins = FALSE, ...) {
    .NotYetImplemented()
}

##' @export
##' @rdname penalty
`penalty.t2.smooth` <- function(object, margins = FALSE, ...) {
    .NotYetImplemented()
}

##' @importFrom tibble add_column as_tibble
##' @importFrom tidyr pivot_longer
##' @importFrom dplyr starts_with
`tidy_penalty` <- function(s, label) {
    rownames(s) <- paste0("f", seq_len(nrow(s)))
    colnames(s) <- paste0("f", seq_len(ncol(s)))
    s <- as_tibble(s)
    s <- add_column(s, r = paste0("f", seq_len(nrow(s))), .before = 1L)
    s <- pivot_longer(s, cols = starts_with("f"), names_to = "c",
                        values_to = "value")
    s <- add_column(s, sp = rep(label, nrow(s)), .before = 1L)
    s
}
