## Add rootogram support for suitable distributions

#' Rootograms to assess goodness of model fit
#'
#' A rootogram is a model diagnostic tool that assesses the goodness of fit of
#' a statistical model. The observed values of the response are compared with
#' those expected from the fitted model. For discrete, count responses, the
#' frequency of each count (0, 1, 2, etc) in the observed data and expected
#' from the conditional distribution of the response implied by the model are
#' compared. For continuous variables, the observed and expected frequencies
#' are obtained by grouping the data into bins. The rootogram is drawn using
#' [ggplot2::ggplot()] graphics. The design closely follows Kleiber & Zeileis
#' (2016).
#'
#' @param object an R object
#' @param max_count integer; the largest count to consider
#' @param breaks for continuous responses, how to group the response. Can be
#'   anything that is acceptable as the `breaks` argument of
#'   [graphics::hist.default()]
#' @param ... arguments passed to other methods
#'
#' @references Kleiber, C., Zeileis, A., (2016) Visualizing Count Data
#'   Regressions Using Rootograms. *Am. Stat.* **70**, 296–303.
#'   \doi{10.1080/00031305.2016.1173590}
#'
#' @export
#' @examples
#' load_mgcv()
#' \dontshow{op <- options(cli.unicode = FALSE, digits = 6)}
#' df <- data_sim("eg1", n = 1000, dist = "poisson", scale = 0.1, seed = 6)
#'
#' # A poisson example
#' m <- gam(y ~ s(x0, bs = "cr") + s(x1, bs = "cr") + s(x2, bs = "cr") +
#'          s(x3, bs = "cr"), family = poisson(), data = df, method = "REML")
#' rg <- rootogram(m)
#' rg
#' draw(rg) # plot the rootogram
#'
#' # A Gaussian example
#' df <- data_sim("eg1", dist = "normal", seed = 2)
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
#' draw(rootogram(m, breaks = "FD"), type = "suspended")
#' \dontshow{options(op)}
`rootogram` <- function(object, ...) {
    UseMethod("rootogram")
}

#' @rdname rootogram
#' @importFrom stringr str_detect str_to_title
#' @export
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
                 poisson = fitted_poisson(object, max_count = max_count),
                 `Negative Binomial` = fitted_neg_bin(object,
                                                      max_count = max_count),
                 gaussian = fitted_gaussian(object, breaks = breaks))
    class(df) <- append(class(df), "rootogram", after = 0L)
    attr(df, "distribution") <- stringr::str_to_title(distrs[supported])
    df
}

#' @importFrom purrr map_dfc
#' @importFrom stats dpois na.omit
#' @importFrom tibble tibble
#' @keywords internal
`fitted_poisson` <- function(model, max_count = NULL) {
    mf <- model.frame(model)
    y <- model.response(mf)
    if (is.null(max_count)) {
        max_count <- rootogram_max_count(y)
    }
    bin <- seq(0, max_count, by = 1L)
    names(bin) <- as.character(bin)
    obs <- table(factor(y, levels = bin))
    lambda <- predict(model, newdata = mf, na.action = na.omit,
                      type = "response")
    fitted <- purrr::map_dfc(bin, .f = dpois, lambda = lambda)
    fitted <- colSums(fitted)
    tibble(bin = bin,
           observed = as.integer(obs[seq_along(bin)]),
           fitted = fitted)
}

#' @importFrom purrr map_dfc
#' @importFrom stats dnbinom na.omit
#' @importFrom tibble tibble
#' @keywords internal
`fitted_neg_bin` <- function(model, max_count = NULL) {
    mf <- model.frame(model)
    y <- model.response(mf)
    if (is.null(max_count)) {
        max_count <- rootogram_max_count(y)
    }
    bin <- seq(0, max_count, by = 1L)
    names(bin) <- as.character(bin)
    obs <- table(factor(y, levels = bin))
    mu <- predict(model, newdata = mf, na.action = na.omit,
                  type = "response")
    theta <- nb_theta(model)
    fitted <- purrr::map_dfc(bin, .f = dnbinom, mu = mu, size = theta)
    fitted <- colSums(fitted)
    df <- tibble(bin = bin,
                 observed = as.integer(obs[seq_along(bin)]),
                 fitted = fitted)
    attr(df, "theta") <- theta
    df
}

#' @importFrom purrr map_dfc
#' @importFrom stats pnorm na.omit
#' @importFrom graphics hist
#' @importFrom tibble tibble
#' @keywords internal
`fitted_gaussian` <- function(model, breaks = NULL) {
    if (is.null(breaks)) {
        breaks <- "Sturges" ## give same breaks as those of hist()
    }
    mf <- model.frame(model)
    y <- model.response(mf)
    mu <- predict(model, newdata = mf, na.action = na.omit)
    h <- hist(y, breaks = breaks, plot = FALSE)
    bin <- h$breaks
    names(bin) <- as.character(bin)
    obs <- h$counts
    mid <- h$mids
    sigma <- model$sig2
    pdf <- purrr::map_dfc(bin, .f = pnorm, mean = mu, sd = sigma)
    nc <- length(bin)
    fitted <- pdf[, seq(1, nc - 1, by = 1L)]
    fitted[] <- pdf[, seq(2, nc, by = 1L)] - pdf[, seq(1, nc - 1, by = 1L)]
    fitted <- colSums(fitted)
    df <- tibble::tibble(bin = bin[-nc],
                         observed = obs,
                         fitted = fitted)
    attr(df, "sigma") <- sigma
    attr(df, "mid") <- mid
    attr(df, "width") <- diff(h$breaks)
    df
}

## try to match countreg::rootogram
#' @noRd
#' @keywords internal
`rootogram_max_count` <- function(y) {
    max(1.5 * max(y), 20L)
}

#' Draw a rootogram
#'
#' A rootogram is a model diagnostic tool that assesses the goodness of fit of
#' a statistical model. The observed values of the response are compared with
#' those expected from the fitted model. For discrete, count responses, the
#' frequency of each count (0, 1, 2, etc) in the observed data and expected
#' from the conditional distribution of the response implied by the model are
#' compared. For continuous variables, the observed and expected frequencies
#' are obtained by grouping the data into bins. The rootogram is drawn using
#' [ggplot2::ggplot()] graphics. The design closely follows Kleiber & Zeileis
#' (2016).
#'
#' @param type character; the type of rootogram to draw.
#' @param sqrt logical; show the observed and fitted frequencies
#' @param ref_line logical; draw a reference line at zero?
#' @param warn_limits logical; draw Tukey's warning limit lines at +/- 1?
#' @param fitted_colour,bar_colour,bar_fill,ref_line_colour,warn_line_colour
#'   colours used to draw the respective element of the rootogram.
#' @param xlab,ylab character; labels for the x and y axis of the rootogram.
#'   May be missing (`NULL`), in which case suitable labels will be used.
#''
#' @inheritParams draw
#'
#' @references Kleiber, C., Zeileis, A., (2016) Visualizing Count Data
#'   Regressions Using Rootograms. *Am. Stat.* **70**, 296–303.
#'   \doi{10.1080/00031305.2016.1173590}
#'
#' @return A 'ggplot' object.
#'
#' @family draw methods
#' @seealso [rootogram()] to compute the data for the rootogram.
#'
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot aes geom_rect geom_point geom_line labs geom_hline
#'
#' @examples
#' load_mgcv()
#' df <- data_sim("eg1", n = 1000, dist = "poisson", scale = 0.1, seed = 6)
#'
#' # A poisson example
#' m <- gam(y ~ s(x0, bs = "cr") + s(x1, bs = "cr") + s(x2, bs = "cr") +
#'          s(x3, bs = "cr"), family = poisson(), data = df, method = "REML")
#' rg <- rootogram(m)
#'
#' # plot the rootogram
#' draw(rg)
#'
#' # change the type of rootogram
#' draw(rg, type = "suspended")
`draw.rootogram` <- function(object,
                             type = c("hanging", "standing", "suspended"),
                             sqrt = TRUE,
                             ref_line = TRUE,
                             warn_limits = TRUE,
                             fitted_colour = "steelblue",
                             bar_colour = NA, #"grey",
                             bar_fill = "grey",
                             ref_line_colour = "black",
                             warn_line_colour = "black",
                             ylab = NULL, xlab = NULL, ...) {
    type <- match.arg(type)

    ## Create a suitable axis label for y axis if none supplied
    if (is.null(ylab)) {
        ylab <- if (as.logical(sqrt)) {
            object <- mutate(object,
                             observed = sqrt(.data$observed),
                             fitted = sqrt(.data$fitted))
            expression(sqrt(Frequency))
        } else {
            "Frequency"
        }
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
                     x_low = .data$bin - width,
                     x_high = .data$bin + width)

    nr <- nrow(object)
    object <- if (type == "hanging") {
        mutate(object,
               y_bot = .data$fitted - .data$observed,
               y_top = .data$y_bot + .data$observed)
    } else if (type == "suspended") {
        mutate(object,
               y_bot = rep(0, nr),
               y_top = .data$fitted - .data$observed)
    } else {
        mutate(object,
               y_bot = rep(0, nr),
               y_top = .data$y_bot + .data$observed)
    }

    plt <- ggplot(object) +
        geom_rect(aes(xmin = .data$x_low,
                      xmax = .data$x_high,
                      ymin = .data$y_bot,
                      ymax = .data$y_top),
                  fill = bar_fill, col = bar_colour) +
        geom_line(aes(x = .data$bin,
                      y = .data$fitted),
                  colour = fitted_colour, linewidth = 1) +
        geom_point(aes(x = .data$bin,
                       y = .data$fitted),
                   colour = fitted_colour, size = 2.5)

    if (as.logical(ref_line) & type != "standing") {
        plt <- plt + geom_hline(yintercept = 0, colour = ref_line_colour)
    }

    if (as.logical(warn_limits) & type != "standing") {
        plt <- plt + geom_hline(yintercept = c(-1, 1), lty = "dashed",
                                colour = warn_line_colour)
    }

    plt <- plt + labs(y = ylab, x = xlab)

    plt
}
