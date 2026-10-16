#' Surrogate residuals for ordered categorical GAMs
#'
#' Draw continuous residuals for models fitted with [mgcv::ocat()], using the
#' latent-variable method of Liu and Zhang (2018). No additional package is
#' required.
#'
#' @inheritParams quantile_residuals
#' @param conditioning character or `NULL`; only `"conditional"` is supported.
#'   The default, `NULL`, holds fitted random effects fixed in the same way.
#' @return A numeric vector with one residual per observation. With `na.exclude`,
#'   excluded fitting observations are restored as `NA`; with `na.omit`, only
#'   observations used for fitting are returned.
#'
#' @details
#' An ordered categorical model represents category `j` by an interval between
#' two thresholds on a continuous latent scale. For each observation, a surrogate
#' response is drawn from the fitted logistic distribution restricted to its
#' observed category's interval. The residual subtracts the fitted latent mean
#' from this surrogate response.
#'
#' Equivalently, draw a probability uniformly between the fitted probabilities
#' `Pr(Y < y)` and `Pr(Y <= y)`, then apply [stats::qlogis()]. Under the assumed
#' model with known parameters, the residuals have a logistic distribution with
#' location zero and scale one: mean zero and variance `pi^2 / 3`. Fitted
#' parameters affect this reference approximation. The identity link reported by
#' `ocat()` describes the latent mean; its latent distribution is logistic.
#'
#' This calculation shares the randomised probabilities used by
#' [quantile_residuals()], which instead applies `qnorm()` for normal scores.
#' Both tails are calculated using log probabilities to retain accuracy when
#' ordinary probabilities round to zero or one. Fitting weights do not change
#' the scale of the latent logistic distribution.
#'
#' Supported fits are `gam` and `bam` models with a single `ocat()` family.
#' Fitted random effects are held fixed. Grouped families and other ordinal
#' model classes are not supported. Use `type = "surrogate"` in [appraise()],
#' [qq_plot()], [worm_plot()] and other residual diagnostic plots. QQ and worm
#' plots require `method = NULL` and use logistic reference quantiles.
#'
#' Each call draws one residual per observation. Set `seed` for reproducibility;
#' repeating plots with different seeds helps assess whether a pattern persists
#' across residual draws. An explicit seed preserves the caller's random-number
#' state. With `seed = NULL`, sampling uses and advances the current state.
#'
#' @references
#' Liu, D. and Zhang, H. (2018). Residuals and Diagnostics for Ordinal Regression
#' Models: A Surrogate Approach. Journal of the American Statistical Association,
#' 113(522), 845--854. \doi{10.1080/01621459.2017.1292915}.
#'
#' @seealso [quantile_residuals()], [appraise()], [diagnostic_support]
#' @export
#' @examples
#' library("mgcv")
#' set.seed(359)
#' dat <- data.frame(x = runif(200, -2, 2))
#' latent <- sin(dat$x) + rlogis(nrow(dat))
#' dat$y <- findInterval(latent, c(-1, 0, 2)) + 1L
#' fit <- gam(y ~ s(x), data = dat, family = ocat(R = 4), method = "REML")
#' head(surrogate_residuals(fit, seed = 42))
#' appraise(fit, type = "surrogate", seed = 42)
surrogate_residuals <- function(model, seed = NULL, ...) {
  UseMethod("surrogate_residuals")
}

#' @rdname surrogate_residuals
#' @export
surrogate_residuals.gam <- function(model, seed = NULL, conditioning = NULL, ...) {
  check_surrogate_model(model)
  check_distribution_conditioning(conditioning)
  used <- model_used_rows(model)
  r <- distribution_quantile_residuals(response_distribution(used), used$y,
    type = "quantile", seed = seed, quantile_function = stats::qlogis)
  restore_model_rows(r, model)
}

#' @rdname surrogate_residuals
#' @export
surrogate_residuals.default <- function(model, seed = NULL, ...) {
  stop("Surrogate residuals are available only for gam and bam models fitted ",
    "with mgcv::ocat().", call. = FALSE)
}

check_surrogate_model <- function(model) {
  if (inherits(model, "gratia_diagnostics")) model <- model$source
  if (!inherits(model, "gam") ||
      !grepl("^ordered_categorical", family_type(model))) {
    surrogate_residuals.default(model)
  }
  invisible(model)
}
