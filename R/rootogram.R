## Add rootogram support for suitable distributions

##' Computes data used to draw a rootogram diagnostic plot
##'
##' @param object an R object
##' @param max_count integer; the largest count to consider
##' @param breaks for continuous responses, how to group the response. Can be
##'   anything that is acceptable as the `breaks` argument of
##'   [graphics::hist.default()]
##' @param ... arguments passed to other methods
##'
##' @export
##' @examples
##' load_mgcv()
##' df <- data_sim("eg1", n = 1000, dist = "poisson", scale = 0.1, seed = 6)
##'
##' # A poisson example
##' m <- gam(y ~ s(x0, bs = "cr") + s(x1, bs = "cr") + s(x2, bs = "cr") +
##'          s(x3, bs = "cr"), family = nb, data = df, method = "REML")
##' rg <- rootogram(m)
##' rg
##' draw(rg) # plot the rootogram
##'
##' # A Gaussian example
##' df <- data_sim("eg1", dist = "normal", seed = 2)
##' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
##' draw(rootogram(m, breaks = "FD"), type = "suspended")
`rootogram` <- function(object, ...) {
    UseMethod("rootogram")
}

##' @rdname rootogram
##' @importFrom stringr str_detect str_to_title
##' @export
`rootogram.gam` <- function(object, max_count = NULL, breaks = "Sturges", ...) {
    ## limit this to a few distributions at the moment
    distrs <- c("poisson", "Negative Binomial", "gaussian")
    fname <- family_name(object)

    ## check model family is supported
    supported <- stringr::str_detect(fname, distrs)
    if (!any(supported)) {
        stop("Only <", paste(str_to_title(distrs), collapse = ", "),
             "> models supported.", call. = FALSE)
    }

    df <- switch(distrs[supported],
                 poisson = expected_poisson(object, max_count = max_count),
                 `Negative Binomial` = expected_neg_bin(object,
                                                        max_count = max_count),
                 gaussian = expected_gaussian(object, breaks = breaks))
    class(df) <- append(class(df), "rootogram", after = 0L)
    attr(df, "distribution") <- stringr::str_to_title(distrs[supported])
    df
}

##' @importFrom purrr map_dfc
##' @importFrom stats dpois na.omit
##' @importFrom tibble tibble
`expected_poisson` <- function(model, max_count = NULL) {
    mf <- model.frame(model)
    y <- model.response(mf)
    if (is.null(max_count)) {
        max_count <- rootogram_max_count(y)
    }
    i <- seq(0, max_count, by = 1L)
    names(i) <- as.character(i)
    obs <- table(factor(y, levels = i))
    lambda <- predict(model, newdata = mf, na.action = na.omit)
    expected <- purrr::map_dfc(i, .f = dpois, lambda = lambda)
    expected <- colSums(expected)
    tibble(i = i, observed = as.integer(obs[seq_along(i)]), expected = expected)
}

##' @importFrom purrr map_dfc
##' @importFrom stats dnbinom na.omit
##' @importFrom tibble tibble
`expected_neg_bin` <- function(model, max_count = NULL) {
    mf <- model.frame(model)
    y <- model.response(mf)
    if (is.null(max_count)) {
        max_count <- rootogram_max_count(y)
    }
    i <- seq(0, max_count, by = 1L)
    names(i) <- as.character(i)
    obs <- table(factor(y, levels = i))
    mu <- predict(model, newdata = mf, na.action = na.omit)
    theta <- nb_theta(model)
    expected <- purrr::map_dfc(i, .f = dnbinom, mu = mu, size = theta)
    expected <- colSums(expected)
    tibble(i = i, observed = as.integer(obs[seq_along(i)]), expected = expected)
}

##' @importFrom purrr map_dfc
##' @importFrom stats pnorm na.omit
##' @importFrom graphics hist
##' @importFrom tibble tibble
`expected_gaussian` <- function(model, breaks = NULL) {
    if (is.null(breaks)) {
        breaks <- "Sturges" ## give same breaks as those of hist()
    }
    mf <- model.frame(model)
    y <- model.response(mf)
    mu <- predict(model, newdata = mf, na.action = na.omit)
    h <- hist(y, breaks = breaks, plot = FALSE)
    i <- h$breaks
    names(i) <- as.character(i)
    obs <- h$counts
    mid <- h$mids
    sigma <- model$sig2
    pdf <- purrr::map_dfc(i, .f = pnorm, mean = mu, sd = sigma)
    nc <- length(i)
    expected <- pdf[, seq(1, nc - 1, by = 1L)]
    expected[] <- pdf[, seq(2, nc, by = 1L)] - pdf[, seq(1, nc - 1, by = 1L)]
    expected <- colSums(expected)
    df <- tibble::tibble(i = i[-nc], observed = obs, expected = expected)
    attr(df, "sigma") <- sigma
    attr(df, "mid") <- mid
    attr(df, "width") <- diff(h$breaks)
    df
}

## try to match countreg::rootogram
`rootogram_max_count` <- function(y) {
    max(1.5 * max(y), 20L)
}

##' Draw the computed rootogram
##'
##' @param type character
##' @param sqrt logical
##' @param ref_line logical
##' @param warn_limits logical
##' @param xlab,ylab character; labels for the x and y axis of the rootogram.
##'   May be missing (`NULL`), in which case suitable labels will be used.
##'
##' @inheritParams draw
##'
##' @return A 'ggplot' object.
##'
##' @export
##' @importFrom rlang .data
##' @importFrom dplyr mutate
##' @importFrom ggplot2 ggplot aes_string geom_rect geom_point geom_line labs geom_hline
`draw.rootogram` <- function(object,
                             type = c("hanging", "standing", "suspended"),
                             sqrt = TRUE,
                             ref_line = TRUE,
                             warn_limits = TRUE,
                             ylab = NULL, xlab = NULL, ...) {
    type <- match.arg(type)

    y_label <- "Frequency"
    if (as.logical(sqrt)) {
        object <- mutate(object,
                         observed = sqrt(.data$observed),
                         expected = sqrt(.data$expected))

        y_label <- expression(sqrt(Frequency))
    }

    ## assign the y label if ylab not supplied
    if (is.null(ylab)) {
        ylab <- y_label
    }
    ## ...and same for the x axis
    if (is.null(xlab)) {
        xlab <- "Response"
    }

    ## bin width
    distr <- attr(object, "distribution")
    ## bins for counts are 1 unit wide so make bins 5% narrower on either side
    width <- if (distr %in% c("Poisson", "Negative Binomial")) {
        0.45
    } else {
        ## but if a continuous distrib then we capture the width attribute and
        ## scale it by 90 (dividing by 2 as we +/- half the width to observed
        ## value)
        w <- attr(object, "width")
        (w * 0.9) / 2
    }

    object <- mutate(object,
                     x_low = .data$i - width,
                     x_high = .data$i + width)

    nr <- nrow(object)
    object <- if (type == "hanging") {
        mutate(object,
               y_bot = .data$expected - .data$observed,
               y_top = .data$y_bot + .data$observed)
    } else if (type == "suspended") {
        mutate(object,
               y_bot = rep(0, nr),
               y_top = .data$expected - .data$observed)
    } else {
        mutate(object,
               y_bot = rep(0, nr),
               y_top = .data$y_bot + .data$observed)
    }

    plt <- ggplot(object) +
        geom_rect(aes_string(xmin = "x_low", xmax = "x_high",
                             ymin = "y_bot", ymax = "y_top"),
                  fill = "grey", col = NA) +
        geom_line(aes_string(x = "i", y = "expected"), col = "steelblue") +
        geom_point(aes_string(x = "i", y = "expected"), col = "steelblue")

    if (as.logical(ref_line) & type != "standing") {
        plt <- plt + geom_hline(yintercept = 0)
    }

    if (as.logical(warn_limits) & type != "standing") {
        plt <- plt + geom_hline(yintercept = c(-1, 1), lty = "dashed")
    }

    plt <- plt + labs(y = ylab, x = xlab)

    plt
}