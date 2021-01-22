##' Extract and tidy penalty matrices
##'
##' @param object a fitted GAM or a smooth.
##' @param smooth character; vector of smooths to extract penalty matrices for.
##'   If `NULL`, penalty matrices for all smooths in `object` are extracted.
##' @param rescale logical; by default, *mgcv* will scale the penalty matrix for
##'   better performance in [mgcv::gamm()]. If `rescale` is `TRUE`, this scaling
##'   will be undone to put the penalty matrix back on the original scale.
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
`penalty.gam` <- function(object, smooth = NULL, rescale = FALSE, ...) {
    ## are particular smooths selected
    smooth_ids <- if (!is.null(smooth)) {
         which_smooths(object, smooth) # which smooths match 'smooth'
    } else {
        seq_len(n_smooths(object))
    }

    ## extract the mgcv.smooth objects
    smooths <- get_smooths_by_id(object, smooth_ids)

    ## loop over the smooths applying penalty to each
    pen <- lapply(smooths, penalty, rescale = rescale)
    pen <- bind_rows(pen)
    class(pen) <- c("tidy_penalty", class(pen))
    pen
}

##' @export
##' @rdname penalty
`penalty.mgcv.smooth` <- function(object, rescale = FALSE, ...) {
    ## smooth label
    sm_lab <- smooth_label(object)
    ## type of smooth
    sm_type <- smooth_type(object)
    ## extract the set of penalty matrices
    S <- object[["S"]] # S is a list even if length(S) == 1
    sp_label <- names(object[["sp"]]) # penalty matrix label
    pen <- vector("list", length = length(S))
    ## loop over penalty matrices & tidy each of them
    pen_seq <- seq_along(pen)
    for (i in pen_seq) {
        pen[[i]] <- tidy_penalty(S[[i]], smooth = sm_lab,
                                 type = sm_type,
                                 label = sp_label[i])
    }

    ## should the default penalty rescaling for gamm performance be reversed?
    if (rescale) {
        for (i in pen_seq) {
            S_scale <- object[["S.scale"]][i]
            if (is.null(S_scale)) {
                S_scale <- 1
            }
            pen[[i]][["value"]] <- pen[[i]][["value"]] * S_scale
        }
    }

    ## combine the tidy penalty matrices into a single tibble
    pen <- bind_rows(pen)
    class(pen) <- c("penalty_df", class(pen))
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
`tidy_penalty` <- function(s, smooth, type, label) {
    rownames(s) <- paste0("f", seq_len(nrow(s)))
    colnames(s) <- paste0("f", seq_len(ncol(s)))
    s <- as_tibble(s)
    s <- add_column(s, row = paste0("f", seq_len(nrow(s))), .before = 1L)
    s <- pivot_longer(s, cols = starts_with("f"), names_to = "col",
                      values_to = "value")
    ns <- nrow(s)
    s <- add_column(s,
                    smooth  = rep(smooth, ns),
                    type    = rep(type, ns),
                    penalty = rep(label, ns), .before = 1L)
    s
}
