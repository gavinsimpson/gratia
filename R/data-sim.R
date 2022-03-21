#' Simulate example data for fitting GAMs
#'
#' A tidy reimplementation of the functions implemented in [mgcv::gamSim()]
#' that can be used to fit GAMs. An new feature is that the sampling
#' distribution can be applied to all the example types.
#'
#' @param model character; either `"egX"` where `X` is an integer `1:7`, or
#'   the name of a model. See Details for possible options.
#' @param n numeric; the number of observations to simulate.
#' @param dist character; a sampling distribution for the response
#'   variable.
#' @param scale numeric; the level of noise to use.
#' @param theta numeric; the dispersion parameter \eqn{\theta} to use. The
#'   default is entirely arbitrary, chosen only to provide simulated data that
#'   exhibits extra dispersion beyond that assumed by under a Poisson.
#' @param seed numeric; the seed for the random number generator. Passed to
#'   [base::set.seed()].
#'
#' @export
#'
#' @examples
#' \dontshow{
#' set.seed(1)
#' op <- options(digits = 5, cli.unicode = FALSE)
#' }
#' data_sim("eg1")
#' \dontshow{options(op)}
`data_sim` <- function(model = "eg1", n = 400, scale = 2, theta = 3,
                       dist = c("normal", "poisson", "binary",
                                "negbin", "tweedie"),
                       seed = NULL) {
    ## sort out the seed
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        runif(1)
    }
    if (is.null(seed)) {
        RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    }
    else {
        R.seed <- get(".Random.seed", envir = .GlobalEnv)
        set.seed(seed)
        RNGstate <- structure(seed, kind = as.list(RNGkind()))
        on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }

    ## check dist is OK
    dist <- match.arg(dist)

    sim_fun <- switch(dist,
                      normal  = sim_normal,
                      poisson = sim_poisson,
                      binary  = sim_binary,
                      negbin  = sim_nb,
                      tweedie = sim_tweedie)

    model_fun <- switch(model,
                        eg1 = four_term_additive_model,
                        eg2 = bivariate_model,
                        eg3 = continuous_by_model,
                        eg4 = factor_by_model,
                        eg5 = additive_plus_factor_model,
                        eg6 = four_term_plus_ranef_model,
                        eg7 = correlated_four_term_additive_model)

    model_fun(n = n, sim_fun = sim_fun, scale = scale, theta = theta)
}

#' @importFrom stats rnorm
`sim_normal` <- function(x, scale = 2, ...) {
    tibble(y = x + rnorm(length(x), mean = 0, sd = scale),
           f = x)
}

#' @importFrom stats rpois
`sim_poisson` <- function(x, scale = 2, ...) {
    lam <- exp(x * scale)
    tibble(y = rpois(rep(1, length(x)), lam), f = log(lam))
}

#' @importFrom stats rbinom binomial
`sim_binary` <- function(x, scale = 2, ...) {
    ilink <- inv_link(binomial())
    x <- (x - 5) * scale
    p <- ilink(x)
    tibble(y = rbinom(p, 1, p), f = x)
}

#' @importFrom stats rnbinom
`sim_nb` <- function(x, scale = 2, theta = 3, ...) {
    lam <- exp(x * scale)
    tibble(y = rnbinom(rep(1, length(x)), mu = lam, size = theta),
           f = log(lam))
}

`sim_tweedie` <- function(x, scale = 2, power = 1, ...) {
    .NotYetImplemented()
}

## Gu Wabha functions
#' Gu and Wabha test functions
#'
#' @param x numeric; vector of points to evaluate the function at, on interval
#'   (0,1)
#'
#' @rdname gw_functions
#' @export
#'
#' @examples
#' \dontshow{op <- options(digits = 4)}
#' x <- seq(0, 1, length = 6)
#' gw_f0(x)
#' gw_f1(x)
#' gw_f2(x)
#' gw_f3(x) # should be constant 0
#' \dontshow{options(op)}
gw_f0 <- function(x) {
    2 * sin(pi * x)
}

#' @rdname gw_functions
#' @export
gw_f1 <- function(x) {
    exp(2 * x)
}

#' @rdname gw_functions
#' @export
gw_f2 <- function(x) {
    0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10 * x)^3 * (1 - x)^10
}

#' @rdname gw_functions
#' @export
gw_f3 <- function(x) { # a null function with zero effect
    0 * x
}

## bivariate function
bivariate <- function(x, z, sx = 0.3, sz = 0.4) {
    (pi^sx * sz) * (1.2 * exp(-(x - 0.2)^2/sx^2 - (z - 0.3)^2/sz^2) +
                    0.8 * exp(-(x - 0.7)^2/sx^2 - (z - 0.8)^2/sz^2))
}

#' @importFrom tibble tibble
#' @importFrom dplyr mutate bind_cols
#' @importFrom rlang .data
`four_term_additive_model` <- function(n, sim_fun = sim_normal, scale = 2,
                                       theta = 3) {
    data <- tibble(x0 = runif(n, 0, 1), x1 = runif(n, 0, 1),
                   x2 = runif(n, 0, 1), x3 = runif(n, 0, 1))
    data <- mutate(data,
                   f0 = gw_f0(.data$x0), f1 = gw_f1(.data$x1),
                   f2 = gw_f2(.data$x2), f3 = gw_f3(.data$x3))
    data2 <- sim_fun(x = data$f0 + data$f1 + data$f2, scale, theta = theta)
    data <- bind_cols(data2, data)
    data[c("y", "x0", "x1", "x2", "x3", "f", "f0", "f1", "f2", "f3")]
}

#' @importFrom tibble tibble
#' @importFrom dplyr mutate bind_cols
#' @importFrom rlang .data
`correlated_four_term_additive_model` <- function(n, sim_fun = sim_normal,
                                                  scale = 2, theta = 3) {
    data <- tibble(x0 = runif(n, 0, 1), x2 = runif(n, 0, 1))
    data <- mutate(data,
                   x1 = .data$x0 * 0.7 + runif(n, 0, 0.3),
                   x3 = .data$x2 * 0.9 + runif(n, 0, 0.1))
    data <- mutate(data,
                   f0 = gw_f0(.data$x0), f1 = gw_f1(.data$x1),
                   f2 = gw_f2(.data$x2), f3 = gw_f3(.data$x0))
    data2 <- sim_fun(x = data$f0 + data$f1 + data$f2, scale = scale,
                     theta = theta)
    data <- bind_cols(data2, data)
    data[c("y", "x0", "x1", "x2", "x3", "f", "f0", "f1", "f2", "f3")]
}

#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom rlang .data
`bivariate_model` <- function(n, sim_fun = sim_normal, scale = 2, theta = 3) {
    data <- tibble(x = runif(n), z = runif(n))
    data2 <- sim_fun(x = bivariate(data$x, data$z), scale = scale,
                     theta = theta)
    data <- bind_cols(data2, data)
    data
}

#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols 
`continuous_by_model` <- function(n, sim_fun = sim_normal, scale = 2,
                                  theta = 3) {
    data <- tibble(x1 = runif(n, 0, 1), x2 = sort(runif(n, 0, 1)))
    `f_fun` <- function(x) {
        0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10 * x)^3 * (1 - x)^10
    }
    data2 <- sim_fun(x = f_fun(data$x2) * data$x1, scale = scale, theta = theta)
    data <- bind_cols(data2, data)
    data[c("y", "x1", "x2", "f")]
}

#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom rlang .data
`factor_by_model` <- function(n, sim_fun = sim_normal, scale = 2, theta = 3) {
    data <- tibble(x0 = runif(n, 0, 1), x1 = runif(n, 0, 1),
                   x2 = sort(runif(n, 0, 1)))
    data <- mutate(data,
                   f1 = 2 * sin(pi * .data$x2), f2 = exp(2 * .data$x2) -
                     3.75887,
                   f3 = 0.2 * .data$x2^11 * (10 * (1 - .data$x2))^6 +
                       10 * (10 * .data$x2)^3 * (1 - .data$x2)^10,
                   fac = as.factor(sample(1:3, n, replace = TRUE)))
    y <- data$f1 * as.numeric(data$fac == 1) +
        data$f2 * as.numeric(data$fac == 2) +
        data$f3 * as.numeric(data$fac == 3)
    data2 <- sim_fun(y, scale = scale, theta = theta)
    data <- bind_cols(data2, data)
    data[c("y", "x0", "x1", "x2", "fac", "f", "f1", "f2", "f3")]
}

#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom rlang .data
`additive_plus_factor_model` <- function(n, sim_fun = sim_normal, scale = 2,
                                         theta = 3) {
    data <- tibble(x0 = rep(1:4, n / 4), x1 = runif(n, 0, 1),
                   x2 = runif(n, 0, 1), x3 = runif(n, 0, 1))
    data <- mutate(data,
                   f0 = 2 * .data$x0,
                   f1 = exp(2 * .data$x1),
                   f2 = 0.2 * .data$x2^11 * (10 * (1 - .data$x2))^6 + 10 *
                       (10 * .data$x2)^3 * (1 - .data$x2)^10,
                   f3 = 0 * .data$x3)
    y <- data$f0 + data$f1 + data$f2
    data2 <- sim_fun(y, scale = scale, theta = theta)
    data <- mutate(data, x0 = as.factor(.data$x0))
    data <- bind_cols(data2, data)
    data[c("y", "x0", "x1", "x2", "x3", "f", "f0", "f1", "f2", "f3")]
}

#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom rlang .data
`four_term_plus_ranef_model` <- function(n, sim_fun = sim_normal, scale = 2,
                                         theta = 3) {
    data <- four_term_additive_model(n = n, sim_fun = sim_fun, scale = 0)
    data <- mutate(data, fac = rep(1:4, n / 4))
    data <- mutate(data,
                   f = .data$f + .data$fac * 3,
                   fac = as.factor(.data$fac))
    data2 <- sim_fun(data$f, scale = scale, theta = theta)
    data <- mutate(data, y = data2$y)
    data[c("y", "x0", "x1", "x2", "x3", "fac", "f", "f0", "f1", "f2", "f3")]
}

#' Generate reference simulations for testing
#'
#' @param scale numeric; the noise level.
#' @param seed numeric; the seed to use for simulating data.
#'
#' @return A named list of tibbles containing
#' 
#' @importFrom tidyr expand_grid
#' @importFrom purrr pmap
#' @noRd
`create_reference_simulations` <- function(scale = 0.2, n = 100, seed = 42,
                                           theta = 4) {
    `data_sim_wrap` <- function(model, dist, scale, theta, n, seed, ...) {
        data_sim(model, dist = dist, scale = scale, theta = theta,
                 n = n, seed = seed, ...)
    }
    params <- expand_grid(model = paste0("eg", 1:7),
                          dist  = c("normal", "poisson", "binary",
                                    "negbin"),
                          scale = rep(scale, length.out = 1),
                          n = rep(n, length.out = 1),
                          seed = rep(seed, length.out = 1),
                          theta = rep(theta, length.out = 1))
    out <- pmap(params, .f = data_sim_wrap)
    nms <- unlist(pmap(params[1:2], paste, sep = "-"))
    names(out) <- nms
    out
}
