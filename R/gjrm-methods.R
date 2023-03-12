#' Plot smooths of a GAMLSS model estimated by `GJRM::gamlss`
#'
#' Provides a [gratia::draw()] method for GAMLSS (distributional GAMs) fitted
#' by [GJRM::gamlss()].
#'
#' @param object a model, fitted by [GJRM::gamlss()]
#' @param ... arguments passed to [gratia::draw.gam()]
#'
#' @inheritParams draw.gam
#'
#' @importFrom purrr map_dbl map
#' @importFrom patchwork plot_layout
#' @export
#'
#' @note Plots of smooths are not labelled with the linear predictor to which
#'   they belong.
#'
#' @examples
#' if (require("GJRM", quietly = TRUE)) {
#'     # follow example from ?GJRM::gamlss
#'     load_mgcv()
#'     library("GJRM")
#'     set.seed(0)
#'     n <- 100
#'     x1 <- round(runif(n))
#'     x2 <- runif(n)
#'     x3 <- runif(n)
#'     f1 <- function(x) cos(pi*2*x) + sin(pi*x)
#'     y1 <- -1.55 + 2*x1 + f1(x2) + rnorm(n)
#'     dataSim <- data.frame(y1, x1, x2, x3)
#'
#'     eq_mu <- y1 ~ x1 + s(x2)
#'     eq_s  <-    ~ s(x3, k = 6)
#'     fl    <- list(eq_mu, eq_s)
#'     m <- gamlss(fl, data = dataSim)
#'
#'     draw(m)
#' }
`draw.gamlss` <- function(object,
                          scales = c("free", "fixed"),
                          ncol = NULL, nrow = NULL, guides = "keep",
                          widths = NULL, heights = NULL, ...) {
    # these models have up to 9 GAMs, one per potential parameter
    # actually I don't think gamlss models can but in general models from GJRM
    # can have this many etas
    # these objects are standard mgcv GAMs
    # find which gams models aren't mgcv gams
    models <- paste0("gam", 1:9)

    # which models are gams, so we can ignore them
    take <- vapply(object[models], FUN = inherits, FUN.VALUE = logical(1),
        "gam")
    eta <- which(take)

    # iterate over the models as use draw.gam but we need to copy coefs and
    # the VCOV over from object to the GAM model object first
    # note this doesn't wrap the plots
    plts <- map(seq_along(models[take]), .f = draw_gamlss_eta,
        model_names = models[take], eta, object)

    # return
    n_plots <- map_dbl(object[models[take]], .f = n_smooths) |>
        sum()
    if (is.null(ncol) && is.null(nrow)) {
        ncol <- ceiling(sqrt(n_plots))
        nrow <- ceiling(n_plots / ncol)
    }
    if (n_plots > 1L && is.null(widths)) {
        # it doesn't matter about the widths if only one plot, but if we have
        # more than one plot and the user didn't change `widths`, then we will
        # force a value of 1 to give all plots the same relative width
        widths <- 1
    }

    # need to append the elements of plts to get a single list where each
    # element is a ggplot object
    plts <- do.call("append", plts)

    if (identical(scales, "fixed")) {
        #x_lim <- get_xlim_from_plots(plts)
        #x_lim <- exec("range", !!!x_lim)

        y_lim <- get_ylim_from_plots(plts)
        y_lim <- exec("range", !!!y_lim)
        plts <- lapply(plts, \(p) p + lims(y = y_lim))
    }
    wrap_plots(plts, byrow = TRUE, nrow = nrow, ncol = ncol, heights = heights,
        widths = widths)
}

`draw_gamlss_eta` <- function(j, model_names, eta, object, ...) {
    ind <- gjrm_gamlss_ind(object, eta[j])
    m <- modify_gamlss_gam(object, model_names[j], ind)
    draw(m, ..., wrap = FALSE)
}

`gjrm_gamlss_ind` <- function(object, eta) {
    # return the indices of terms from the gamlss needed depending on which
    # linear predictor indicated by eta

    # check eta is numeric
    eta <- as.integer(eta)

    # some constants
    is_ordcon <- !is.null(object$VC$K1) & is.null(object$VC$K2)
    is_ordord <- !is.null(object$VC$K1) & !is.null(object$VC$K2)
    if (is_ordcon) {
        shift1 <- object$VC$K1 - 2
        shift2 <- shift3 <- shift1 + 1
    } else if (is_ordord) {
        shift1 <- shift2 <- object$VC$K1 + object$VC$K2 - 3
        shift3 <- shift1 + 1
    } else {
        shift1 <- shift2 <- shift3 <- 0
    }
    # create indices
    ind <- switch(eta,
        eta1 = with(object, (shift1 + 1):(X1.d2 + shift3)),
        eta2 = with(object, (X1.d2 + 1 + shift2):(X1.d2 + X2.d2 + shift3)),
        eta3 = with(object, (X1.d2 + X2.d2 + 1 + shift3):(X1.d2 + X2.d2 +
            X3.d2 + shift3)),
        eta4 = with(object, (X1.d2 + X2.d2 + X3.d2 + 1 + shift3):(X1.d2 +
            X2.d2 + X3.d2 + X4.d2 + shift3)),
        eta5 = with(object, (X1.d2 + X2.d2 + X3.d2 + X4.d2 + 1 +
            shift3):(X1.d2 + X2.d2 + X3.d2 + X4.d2 + X5.d2 + shift3)),
        eta6 = with(object, (X1.d2 + X2.d2 + X3.d2 + X4.d2 + X5.d2 + 1 +
            shift3):(X1.d2 + X2.d2 + X3.d2 + X4.d2 + X5.d2 + X6.d2 + shift3)),
        eta7 = with(object, (X1.d2 + X2.d2 + X3.d2 + X4.d2 + X5.d2 + X6.d2 + 1 +
            shift3):(X1.d2 + X2.d2 + X3.d2 + X4.d2 + X5.d2 + X6.d2 + X7.d2 +
            shift3)),
        eta8 = with(object, (X1.d2 + X2.d2 + X3.d2 + X4.d2 + X5.d2 + X6.d2 +
            X7.d2 + 1 + shift3):(X1.d2 + X2.d2 + X3.d2 + X4.d2 + X5.d2 + X6.d2 +
            X7.d2 + X8.d2 + shift3)),
        eta9 = with(object, (X1.d2 + X2.d2 + X3.d2 + X4.d2 + X5.d2 + X6.d2 +
            X7.d2 + X8.d2 + 1 + shift3):(X1.d2 + X2.d2 + X3.d2 + X4.d2 +
            X5.d2 + X6.d2 + X7.d2 + X8.d2 + X9.d2 + shift3))
    )
    ind
}

# i are the indices of the parameters etc
# object is the gamlss object
# model is the name "gamX" with X and integer 1:9
`modify_gamlss_gam` <- function(object, model, i) {
    # exract the GAM
    gam_obj <- object[[model]]

    # modify the GAM
    gam_obj$coefficients <- object$coefficients[i]
    gam_obj$Vp <- object$Vb[i, i]
    gam_obj$Vp.t <- object$Vb.t[i, i]
    gam_obj$sig2 <- 1
    gam_obj$edf <- diag(object$F)[i]
    gam_obj$scale.estimated <- FALSE
    gam_obj$call$data <- object$call$data

    # return the GAM
    gam_obj
}
