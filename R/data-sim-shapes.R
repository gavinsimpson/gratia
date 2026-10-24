#' Reference functions for shape-constrained GAMs
#'
#' [data_sim()] supplies analytic reference functions for [mgcv::scasm()].
#' All covariates are uniform on `[0, 1]`. Univariate templates return `f1`, the
#' true function, and its analytic derivatives `d1` and `d2`. The response
#' contains sampling variation; the derivative columns describe the truth.
#'
#' @section Univariate templates:
#' | Model suffix after `sc_` | Function | `xt` in `s(x, bs = "sc", xt = ...)` |
#' |:-------------------------|:---------|:--------------------------------------|
#' | `increasing` | `plogis(8 * (x - 0.5))` | `"m+"` |
#' | `decreasing` | `1 - plogis(8 * (x - 0.5))` | `"m-"` |
#' | `convex` | `(x - 0.5)^2` | `"c+"` |
#' | `concave` | `1 - (x - 0.5)^2` | `"c-"` |
#' | `positive` | `0.2 + exp(-16 * (x - 0.5)^2)` | `"+"`, with no intercept |
#' | `increasing_convex` | `exp(2 * x) / exp(2)` | `c("m+", "c+")` |
#' | `increasing_concave` | `log1p(4 * x) / log(5)` | `c("m+", "c-")` |
#' | `decreasing_convex` | `exp(-2 * x)` | `c("m-", "c+")` |
#' | `decreasing_concave` | `1 - x^2` | `c("m-", "c-")` |
#'
#' Remove the intercept for the positivity recipe so the constrained smooth
#' represents the whole function. An unconstrained intercept combined with a
#' centred smooth does not express the same positivity requirement.
#'
#' @section Several terms and distribution parameters:
#' `sc_additive` adapts mgcv's example. Its raw terms are `gw_f0(x0)`,
#' `2 * plogis(20 * (x1 - 0.4))`, `gw_f2(x2)` and `10 * (x3 - 0.55)^4`.
#' The true predictor is their sum minus 1, divided by 4. The second term is
#' increasing and the fourth is convex. Use `predictors = function(d) d$f`
#' with `family = poisson()` to sample mgcv's Poisson example.
#'
#' `sc_tensor` uses `f = x + z + x * z`, increasing in both covariates. Its
#' `dx` and `dz` columns give the true partial derivatives.
#'
#' `sc_location_scale` uses log mean `sin(pi * x) / 2` and log dispersion
#' `-2 + z`, returned in `f` and `f_scale`. It defaults to `gammals()` and
#' records derivatives in `d1`, `d2` and `d_scale`. Constraints act on the
#' specified predictors; in particular the concavity concerns the log mean.
#'
#' The recipes use the default `bs = 0` for fitting without bootstrap
#' replicates. To obtain native constraint-respecting bootstrap coefficient
#' draws, fit with a positive `bs`, such as `bs = 100`.
#'
#' @name data_sim_shapes
#' @seealso [data_sim()], [data_sim_families], [mgcv::scasm()]
#' @examples
#' load_mgcv()
#' d <- data_sim("sc_increasing", n = 200, seed = 2)
#' m <- scasm(y ~ s(x, bs = "sc", xt = "m+", k = 8), data = d)
#'
#' d <- data_sim("sc_positive", n = 200, seed = 2)
#' m <- scasm(y ~ s(x, bs = "sc", xt = "+", k = 8) - 1, data = d)
#'
#' \donttest{
#' d <- data_sim("sc_additive", n = 240, family = poisson(), seed = 2,
#'   predictors = function(d) d$f)
#' m <- scasm(y ~ s(x0, k = 6) + s(x1, bs = "sc", xt = "m+", k = 6) +
#'   s(x2, k = 6) + s(x3, bs = "sc", xt = "c+", k = 6),
#'   family = poisson(), data = d)
#'
#' d <- data_sim("sc_tensor", seed = 2)
#' m <- scasm(y ~ te(x, z, bs = c("sc", "sc"), xt = list("m+", "m+"),
#'   k = c(6, 6)), data = d)
#'
#' d <- data_sim("sc_location_scale", seed = 2)
#' m <- scasm(list(y ~ s(x, bs = "sc", xt = "c-"),
#'   ~s(z, bs = "sc", xt = "m+")), family = gammals(), data = d)
#' }
NULL

# Analytic reference functions on `[0, 1]`. Constraints concern the true term,
# not noisy observations. Derivatives are returned for independent validation.
data_sim_shapes <- function() {
  paste0("sc_", c("increasing", "decreasing", "convex", "concave", "positive",
    "increasing_convex", "increasing_concave", "decreasing_convex", "decreasing_concave",
    "additive", "tensor", "location_scale"))
}

sim_shape_function <- function(x, shape) {
  p <- stats::plogis(8 * (x - 0.5))
  z <- switch(shape,
    increasing = list(p, 8 * p * (1 - p), 64 * p * (1 - p) * (1 - 2 * p)),
    decreasing = list(1 - p, -8 * p * (1 - p), -64 * p * (1 - p) * (1 - 2 * p)),
    convex = list((x - 0.5)^2, 2 * (x - 0.5), rep(2, length(x))),
    concave = list(1 - (x - 0.5)^2, -2 * (x - 0.5), rep(-2, length(x))),
    positive = list(0.2 + exp(-16 * (x - 0.5)^2),
      -32 * (x - 0.5) * exp(-16 * (x - 0.5)^2),
      (1024 * (x - 0.5)^2 - 32) * exp(-16 * (x - 0.5)^2)),
    increasing_convex = list(exp(2 * x) / exp(2), 2 * exp(2 * x) / exp(2), 4 * exp(2 * x) / exp(2)),
    increasing_concave = list(log1p(4 * x) / log(5), 4 / ((1 + 4 * x) * log(5)), -16 / ((1 + 4 * x)^2 * log(5))),
    decreasing_convex = list(exp(-2 * x), -2 * exp(-2 * x), 4 * exp(-2 * x)),
    decreasing_concave = list(1 - x^2, -2 * x, rep(-2, length(x))))
  if (is.null(z)) stop("Unknown reference shape.", call. = FALSE)
  stats::setNames(z, c("f", "d1", "d2"))
}

data_sim_shape <- function(model, n) {
  shape <- sub("^sc_", "", model)
  if (shape == "additive") {
    data <- tibble(x0 = stats::runif(n), x1 = stats::runif(n),
      x2 = stats::runif(n), x3 = stats::runif(n))
    data$f0 <- gw_f0(data$x0)
    data$f1 <- 2 * stats::plogis(20 * (data$x1 - 0.4))
    data$f2 <- gw_f2(data$x2)
    data$f3 <- 10 * (data$x3 - 0.55)^4
    data$f <- (data$f0 + data$f1 + data$f2 + data$f3 - 1) / 4
  } else if (shape == "tensor") {
    data <- tibble(x = stats::runif(n), z = stats::runif(n))
    data$f <- data$x + data$z + data$x * data$z
    data$dx <- 1 + data$z
    data$dz <- 1 + data$x
  } else if (shape == "location_scale") {
    data <- tibble(x = stats::runif(n), z = stats::runif(n))
    data$f <- sin(pi * data$x) / 2
    data$f_scale <- -2 + data$z
    data$d1 <- pi * cos(pi * data$x) / 2
    data$d2 <- -pi^2 * sin(pi * data$x) / 2
    data$d_scale <- rep(1, n)
  } else {
    data <- tibble(x = stats::runif(n))
    z <- sim_shape_function(data$x, shape)
    data$f <- data$f1 <- z$f
    data$d1 <- z$d1; data$d2 <- z$d2
  }
  data$y <- data$f
  data[c("y", setdiff(names(data), "y"))]
}
