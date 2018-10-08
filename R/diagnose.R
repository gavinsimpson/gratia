## Functions diagnose problems with fitted GAMs

##' @title Quantile-quantile plot of model residuals
##'
##' @param model a fitted model. Currently only class `"gam"`.
##' @param ... arguments passed ot other methods.
##'
##' @export
`qq_plot` <- function(model, ...) {
    UseMethod("qq_plot")
}

##' @export
`qq_plot.default` <- function(model, ...) {
    stop("Unable to produce a Q-Q plot for <",
         class(model)[[1L]], ">",
         call. = FALSE)           # don't show the call, simpler error
}

##' @param method character; method used to generate theoretical quantiles.
##' @param type character; type of residuals to use. Only `"deviance"`,
##'   `"response"`, and `"pearson"` residuals are allowed.
##' @param n_uniform numeric; number of times to randomize uniform quantiles
##'   in the direct computation method (`method = "direct"`).
##' @param xlab character or expression; the label for the y axis. If not
##'   supplied, a suitable label will be generated
##' @param ylab character or expression; the label for the y axis. If not
##'   supplied, a suitable label will be generated
##'
##' @inheritParams draw.evaluated_smooth
##'
##' @rdname qq_plot
##'
##' @importFrom ggplot2 ggplot geom_point geom_abline labs aes_string
##' @importFrom tools toTitleCase
##' @importFrom stats residuals
##' @export
`qq_plot.gam` <- function(model,
                          method = c("direct", "simulate", "normal"),
                          type = c("deviance","response","pearson"),
                          n_uniform = 10,
                          ylab = NULL, xlab = NULL,
                          title = NULL, subtitle = NULL, caption = NULL,
                          ...) {
    method <- match.arg(method)         # what method for the QQ plot?

    ## for now, bail if method not "uniform"
    if (!method %in% c("direct", "normal")) {
        stop("QQ plot method <", method, "> not yet available.",
             call. = FALSE)
    }

    type <- match.arg(type)       # what type of residuals
    r <- residuals(model, type = type)  # model residuals

    ## generate theoretical quantiles
    rq <- switch(method,
                 direct = qq_uniform(model, n = n_uniform, type = type),
                 simulate = qq_simulate(),
                 normal = qq_normal(model))

    ## add labels if not supplied
    if (is.null(ylab)) {
        ylab <- paste(toTitleCase(type), "residuals")
    }

    if (is.null(xlab)) {
        xlab <- "Theoretical quantiles"
    }

    if (is.null(title)) {
        title <- "QQ plot of residuals"
    }

    if (is.null(subtitle)) {
        subtitle <- paste("Method:", method)
    }

    ## put into a data frame
    df <- data.frame(theoretical = sort(rq), residuals = sort(r))

    ## base plot
    plt <- ggplot(df, aes_string(x = "theoretical", y = "residuals"))

    ## add reference line
    plt <- plt + geom_abline(slope = 1, intercept = 0, col = "red")

    ## add point layer
    plt <- plt + geom_point()

    ## add labels
    plt <- plt + labs(title = title, subtitle = subtitle, caption = caption,
                      y = ylab, x = xlab)

    ## return
    plt
}

##
`qq_simulate` <- function(model, type = c("deviance","response","pearson")) {

}

##' @importFrom stats ppoints qnorm
`qq_normal` <- function(model, type = c("deviance","response","pearson")) {
    type <- match.arg(type)
    r <- residuals(model, type = type)
    nr <- length(r)
    ord <- order(order(r))

    out <- qnorm(ppoints(nr))[ord]
    out
}

##' @importFrom mgcv fix.family.qf
##' @importFrom stats residuals fitted family weights na.action
`qq_uniform` <- function(model, n = 10, type = c("deviance","response","pearson")) {
    type <- match.arg(type)
    family <- family(model)                 # extract family
    family <- fix.family.qf(family)         # add quantile fun to family
    dev_resid_fun <- family[["dev.resids"]] # deviance residuals function
    var_fun <- family[["var"]]              # variance function
    q_fun <- family[["qf"]]
    if (is.null(q_fun)) {
        stop("Quantile function for family <", family[["family"]], "> not available.")
    }
    r <- residuals(model, type = type)
    fit <- fitted(model)
    weights <- weights(model, type = "prior")
    sigma2 <- model[["sig2"]]
    if (is.null(sigma2)) {
        sigma2 <- summary(model)$dispersion # rather than call summary, do only what it does?
    }
    na_action <- na.action(model)
    nr <- length(r)                     # number of residuals
    unif <- (seq_len(nr) - 0.5) / nr

    out <- matrix(0, ncol = n, nrow = nr)
    for (i in seq_len(n)) {
        unif <- sample(unif, nr)
        out[, i] <- qq_uniform_quantiles(unif, q_fun,
                                         fit = fit,
                                         weights = weights,
                                         sigma2 = sigma2,
                                         dev_resid_fun = dev_resid_fun,
                                         var_fun = var_fun,
                                         type = type,
                                         na_action = na_action)
    }

    out <- rowMeans(out)
    out <- sort(out)
    out
}

`qq_uniform_quantiles` <- function(qs, q_fun, fit, weights, sigma2, dev_resid_fun,
                                   var_fun, type, na_action) {
    ## permute the uniforms
    ## qs <- sample(qs)
    ## generate quantiles for uniforms from q_fun
    qq <- q_fun(qs, fit, weights, sigma2)
    ## new residuals
    r <- compute_residuals(qq, fit = fit, weights = weights, type = type,
                           dev_resid_fun = dev_resid_fun, var_fun = var_fun,
                           na_action = na_action)
    ## sort residuals & return
    sort(r)
}

##' @importFrom stats naresid
`compute_residuals` <- function(y, fit, weights,
                                type = c("deviance","response","pearson"),
                                dev_resid_fun, var_fun, na_action) {
    type <- match.arg(type)

    r <- switch(type,
                deviance = deviance_residuals(y, fit, weights, dev_resid_fun),
                response = response_residuals(y, fit),
                pearson  = pearson_residuals(y, fit, weights, var_fun)
                )
    ## apply any na.action
    naresid(na_action, r)
}

`response_residuals` <- function(y, fit) {
    y - fit
}

`deviance_residuals` <- function(y, fit, weights, dev_resid_fun) {
    ## compute deviance residuals
    r <- dev_resid_fun(y, fit, weights)
    ## sign of residuals is typically an attribute
    posneg <- attr(r, "sign")
    ## ...but may be missing for some families
    if (is.null(posneg)) {
        posneg <- sign(y - fit)
    }
    ## colculate the deviance residuals
    sqrt(pmax(r, 0)) * posneg
}

`pearson_residuals` <- function(y, fit, weights, var_fun) {
    ## if no variance function then bail out
    if (is.null(var_fun)) {
        stop("Pearson residuals are not available for this family.")
    }
    ## compute pearson residuals
    (y - fit) * sqrt(weights) / sqrt(var_fun(fit))
}
