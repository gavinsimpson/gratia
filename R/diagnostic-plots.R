## Functions to diagnose problems with fitted GAMs

#' Quantile-quantile plot of model residuals
#'
#' Quantile-quantile plots (QQ-plots) for GAMs using the reference quantiles of
#' Augustin *et al* (2012).
#'
#' @param model a fitted model. Currently models inheriting from class `"gam"`,
#'   as well as classes `"glm"` and `"lm"` from calls to [stats::glm] or
#'   [stats::lm] are supported.
#' @param method character; method used to generate theoretical quantiles.
#'   The default is `"uniform"`, which generates reference quantiles using
#'   random draws from a uniform distribution and the inverse cumulative
#'   distribution function (CDF) of the fitted values. The reference quantiles
#'   are averaged over `n_uniform` draws. `"simulate"` generates reference
#'   quantiles by simulating new response data from the model at the observed
#'   values of the covariates, which are then residualised to generate reference
#'   quantiles, using `n_simulate` simulated data sets. `"normal"` generates
#'   reference quantiles using the standard normal distribution. `"uniform"` is
#'   more computationally efficient, but `"simulate"` allows reference bands to
#'   be drawn on the QQ-plot. `"normal"` should be avoided but is used as a fall
#'   back if a random number generator (`"simulate"`) or the inverse of the CDF
#'   are not available from the `family` used during model fitting
#'   (`"uniform"``).
#'
#'   Note that `method = "direct"` is deprecated in favour of
#'   `method = "uniform"`.
#' @param type character; type of residuals to use. Only `"deviance"`,
#'   `"response"`, and `"pearson"` residuals are allowed.
#' @param n_uniform numeric; number of times to randomize uniform quantiles
#'   in the direct computation method (`method = "uniform"`).
#' @param n_simulate numeric; number of data sets to simulate from the estimated
#'   model when using the simulation method (`method = "simulate"`).
#' @param seed numeric; the random number seed to use for `method = "simulate"`
#'   and `method = "uniform"`.
#' @param level numeric; the coverage level for reference intervals. Must be
#'   strictly `0 < level < 1`. Only used with `method = "simulate"`.
#' @param xlab character or expression; the label for the y axis. If not
#'   supplied, a suitable label will be generated.
#' @param ylab character or expression; the label for the y axis. If not
#'   supplied, a suitable label will be generated.
#' @param title character or expression; the title for the plot. See
#'   [ggplot2::labs()]. May be a vector, one per penalty.
#' @param subtitle character or expression; the subtitle for the plot. See
#'   [ggplot2::labs()]. May be a vector, one per penalty.
#' @param caption character or expression; the plot caption. See
#'   [ggplot2::labs()]. May be a vector, one per penalty.
#' @param ci_col fill colour for the reference interval when
#'   `method = "simulate"`.
#' @param ci_alpha alpha transparency for the reference
#'   interval when `method = "simulate"`.
#' @param point_col colour of points on the QQ plot.
#' @param point_alpha alpha transparency of points on the QQ plot.
#' @param line_col colour used to draw the reference line.
#' @param ... arguments passed to other methods.
#'
#' @note The wording used in [mgcv::qq.gam()] uses *direct* in reference to the
#'   simulated residuals method (`method = "simulated"`). To avoid confusion,
#'   `method = "direct"` is deprecated in favour of `method = "uniform"`.
#'
#' @seealso [mgcv::qq.gam] for more details on the methods used.
#'
#' @references
#'
#' The underlying methodology used when `method` is `"simulate"` or `"uniform"`
#' is described in Augustin *et al* (2012):
#'
#' Augustin, N.H., Sauleau, E.-A., Wood, S.N., (2012) On quantile quantile plots
#' for generalized linear models. *Computational Statistics and Data Analysis*
#' **56**, 2404-2409 \doi{doi:10.1016/j.csda.2012.01.026}.
#'
#'
#' @export
`qq_plot` <- function(model, ...) {
  UseMethod("qq_plot")
}

#' @rdname qq_plot
#' @export
`qq_plot.default` <- function(model, ...) {
  stop("Unable to produce a Q-Q plot for <",
    class(model)[[1L]], ">",
    call. = FALSE
  ) # don't show the call, simpler error
}

#' @rdname qq_plot
#'
#' @importFrom ggplot2 ggplot geom_point geom_abline geom_ribbon labs aes
#' @importFrom tools toTitleCase
#' @importFrom stats residuals IQR median
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' ## simulate binomial data...
#' dat <- data_sim("eg1", n = 200, dist = "binary", scale = .33, seed = 0)
#' p <- binomial()$linkinv(dat$f) # binomial p
#' n <- sample(c(1, 3), 200, replace = TRUE) # binomial n
#' dat <- transform(dat, y = rbinom(n, n, p), n = n)
#' m <- gam(y / n ~ s(x0) + s(x1) + s(x2) + s(x3),
#'   family = binomial, data = dat, weights = n,
#'   method = "REML"
#' )
#'
#' ## Q-Q plot; default using direct randomization of uniform quantiles
#' qq_plot(m)
#'
#' ## Alternatively use simulate new data from the model, which
#' ## allows construction of reference intervals for the Q-Q plot
#' qq_plot(m,
#'   method = "simulate",
#'   seed = 42,
#'   point_col = "steelblue",
#'   point_alpha = 0.4
#' )
#'
#' ## ... or use the usual normality assumption
#' qq_plot(m, method = "normal")
`qq_plot.gam` <- function(model,
  method = c("uniform", "simulate", "normal", "direct"),
  type = c("deviance", "response", "pearson"),
  n_uniform = 10,
  n_simulate = 50,
  seed = NULL,
  level = 0.9,
  ylab = NULL,
  xlab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ci_col = "black",
  ci_alpha = 0.2,
  point_col = "black",
  point_alpha = 1,
  line_col = "red", ...
) {
  # sort out seed
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    runif(1)
  }
  if (is.null(seed)) {
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  } else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }

  # figure out method stuff
  method <- match.arg(method) # what method for the QQ plot?
  if (identical(method, "direct")) {
    message("`method = \"direct\"` is deprecated, use `\"uniform\"`")
    method <- "uniform"
  }
  ## check if we can do the method
  ff_qf <- fix.family.qf(family(model))[["qf"]]
  if (identical(method, "uniform") && is.null(ff_qf)) {
    method <- "simulate"
  }
  ff_rd <- fix_family_rd(family(model))[["rd"]]
  if (identical(method, "simulate") && is.null(ff_rd)) {
    method <- "normal"
  }
  if (level <= 0 || level >= 1) {
    stop("Level must be 0 < level < 1. Supplied level <", level, ">",
      call. = FALSE
    )
  }
  type <- match.arg(type) # what type of residuals
  ## r <- residuals(model, type = type)  # model residuals
  ## generate theoretical quantiles
  df <- switch(method,
    uniform = qq_uniform(model, n = n_uniform, type = type),
    simulate = qq_simulate(model,
      n = n_simulate, type = type,
      level = level
    ),
    normal = qq_normal(model, type = type, level = level)
  )
  df <- as_tibble(df)
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
  ## base plot
  plt <- ggplot(df, aes(
    x = .data$theoretical,
    y = .data$residuals
  ))
  ## add reference line
  qq_intercept <- 0
  qq_slope <- 1
  if (method == "normal") {
    qq_intercept <- median(df[["residuals"]])
    qq_slope <- IQR(df[["residuals"]]) / 1.349
    ## R's qq.line() does this, which seems the same as above
    ## probs <- c(0.25, 0.75)
    ## qq_y <- quantile(df[["residuals"]], probs = probs,
    ##                  names = FALSE, qtype = 7)
    ## qq_x <- qnorm(probs)
    ## qq_slope <- diff(qq_y) / diff(qq_x)
    ## qq_intercept <- qq_y[1L] - qq_slope * qq_x[1L]
  }

  plt <- plt + geom_abline(
    slope = qq_slope, intercept = qq_intercept,
    col = line_col
  )
  ## add reference interval
  if (isTRUE(method %in% c("simulate", "normal"))) {
    plt <- plt + geom_ribbon(
      aes(
        ymin = .data$lower,
        ymax = .data$upper,
        x = .data$theoretical
      ),
      inherit.aes = FALSE,
      alpha = ci_alpha, fill = ci_col
    )
  }
  ## add point layer
  plt <- plt + geom_point(colour = point_col, alpha = point_alpha)
  ## add labels
  plt <- plt + labs(
    title = title, subtitle = subtitle, caption = caption,
    y = ylab, x = xlab
  )
  ## return
  plt
}

#' @export
#' @rdname qq_plot
`qq_plot.glm` <- function(model, ...) {
  if (is.null(model[["sig2"]])) {
    model[["sig2"]] <- summary(model)$dispersion
  }
  qq_plot.gam(model, ...)
}

#' @export
#' @rdname qq_plot
#' @importFrom stats df.residual
`qq_plot.lm` <- function(model, ...) {
  r <- residuals(model)
  r.df <- df.residual(model)
  model[["sig2"]] <- sum((r - mean(r))^2) / r.df
  if (is.null(weights(model))) {
    model$prior.weights <- rep(1, nrow(model.frame(model)))
  }
  if (is.null(model[["linear.predictors"]])) {
    model[["linear.predictors"]] <- model[["fitted.values"]]
  }
  qq_plot.gam(model, ...)
}

#' @importFrom mgcv fix.family.rd
#' @importFrom stats weights
`qq_simulate` <- function(model, n = 50,
                          type = c("deviance", "response", "pearson"),
                          level = 0.9, detrend = FALSE) {
  type <- match.arg(type)
  family <- family(model)
  family <- fix_family_rd(family)
  rd_fun <- family[["rd"]]
  alpha <- (1 - level) / 2

  if (is.null(rd_fun)) {
    stop(
      "Random deviate function for family <", family[["family"]],
      "> not available."
    )
  }

  dev_resid_fun <- family[["residuals"]]
  # If dev_resid_fun is NULL it means it is one of the standard families so
  # copy over the the dev.resids object from the family instead
  if (is.null(dev_resid_fun)) {
    dev_resid_fun <- family[["dev.resids"]]
    if (is.null(dev_resid_fun)) {
      stop(
        "Deviance residual function for family <", family[["family"]],
        "> not available."
      )
    }
  }
  var_fun <- family[["variance"]] # variance function
  fit <- fitted(model)
  prior_w <- weights(model, type = "prior")
  sigma2 <- model[["sig2"]]
  # I don't know why this is necessary as summary() doesn't seem to change
  # sig2, but at least stop it from doing expensive ranef tests
  if (is.null(sigma2)) {
    sigma2 <- summary(model, re.test = FALSE)$dispersion
  }
  na_action <- na.action(model)

  sims <- replicate(
    n = n,
    qq_simulate_data(rd_fun,
      fit = fit, weights = prior_w,
      sigma2 = sigma2,
      dev_resid_fun = dev_resid_fun,
      var_fun = var_fun, type = type,
      na_action = na_action,
      model = model
    )
  )
  ## mv_response <- c("Multivariate normal", "multinom")
  if ((!family_name(family) %in% multivariate_y()) && is.matrix(fit)) {
    fit <- fit[, 1]
  }
  n_obs <- if (is.matrix(fit)) {
    length(as.vector(fit))
  } else {
    NROW(fit) # NROW(fit)
  }
  out <- quantile(sims, probs = (seq_len(n_obs) - 0.5) / n_obs)
  int <- apply(sims, 1L, quantile, probs = c(alpha, 1 - alpha))
  r <- residuals(model, type = type)
  if (is.matrix(r)) {
    r <- as.vector(r)
  }
  r <- sort(r)

  ## detrend for worm plots?
  if (isTRUE(detrend)) {
    r <- r - out
    int[1L, ] <- int[1L, ] - out
    int[2L, ] <- int[2L, ] - out
  }

  out <- tibble(
    theoretical = out,
    residuals = r,
    lower = int[1L, ],
    upper = int[2L, ]
  )
  out
}

`qq_simulate_data` <- function(rd_fun, fit, weights, sigma2, dev_resid_fun,
                               var_fun, type, na_action, model) {
  ## simulate data
  ysim <- rd_fun(fit, weights, sigma2)
  ## new residuals
  r <- compute_residuals(ysim,
    fit = fit, weights = weights, type = type,
    dev_resid_fun = dev_resid_fun, var_fun = var_fun,
    na_action = na_action, model = model
  )
  # r can be a matrix for models like mvn(), so collapse to a vector
  if (is.matrix(r)) {
    r <- as.vector(r)
  }
  ## sort residuals & return
  sort(r)
}

#' @importFrom stats ppoints pnorm dnorm IQR median
`qq_normal` <- function(model, type = c("deviance", "response", "pearson"),
                        level = 0.9, detrend = FALSE) {
  se_zscore <- function(z) {
    n <- length(z)
    pnorm_z <- pnorm(z)
    sqrt(pnorm_z * (1 - pnorm_z) / n) / dnorm(z)
  }
  type <- match.arg(type)
  r <- residuals(model, type = type)
  nr <- length(r)
  ord <- order(order(r))
  sd <- IQR(r) / 1.349
  theoretical <- qnorm(ppoints(nr)) # [ord] :no need to reorder as return is df
  med <- median(r) + theoretical * sd
  se <- sd * se_zscore(theoretical)
  crit <- coverage_normal(level)
  crit_se <- crit * se

  ## detrend for worm plots?
  r <- sort(r)
  if (isTRUE(detrend)) {
    r <- r - med
    med <- med * 0
  }

  out <- tibble(
    theoretical = theoretical,
    residuals = r,
    lower = med - crit_se,
    upper = med + crit_se
  )
  out
}

#' @importFrom mgcv fix.family.qf
#' @importFrom stats residuals fitted family weights na.action
`qq_uniform` <- function(model, n = 10,
                         type = c("deviance", "response", "pearson"),
                         level = 0.9, detrend = FALSE) {
  type <- match.arg(type)
  family <- family(model) # extract family
  family <- fix.family.qf(family) # add quantile fun to family
  dev_resid_fun <- family[["residuals"]] # deviance residuals function
  # If dev_resid_fun is NULL it means it is one of the standard families so
  # copy over the the dev.resids object from the family instead
  if (is.null(dev_resid_fun)) {
    dev_resid_fun <- family[["dev.resids"]] # deviance residuals function
  }
  var_fun <- family[["variance"]] # variance function
  q_fun <- family[["qf"]]
  if (is.null(q_fun)) {
    stop(
      "Quantile function for family <", family[["family"]],
      "> not available."
    )
  }
  r <- residuals(model, type = type)
  fit <- fitted(model)
  weights <- weights(model, type = "prior")
  sigma2 <- model[["sig2"]]
  # I don't know why this is necessary as summary() doesn't seem to change
  # sig2, but at least stop it from doing expensive ranef tests
  if (is.null(sigma2)) {
    sigma2 <- summary(model, re.test = FALSE)$dispersion
  }
  na_action <- na.action(model)
  nr <- length(r) # number of residuals
  unif <- (seq_len(nr) - 0.5) / nr

  sims <- matrix(0, ncol = n, nrow = nr)
  for (i in seq_len(n)) {
    unif <- sample(unif, nr)
    sims[, i] <- qq_uniform_quantiles(unif, q_fun,
      fit = fit,
      weights = weights,
      sigma2 = sigma2,
      dev_resid_fun = dev_resid_fun,
      var_fun = var_fun,
      type = type,
      na_action = na_action,
      model = model
    )
  }

  out <- rowMeans(sims)
  r <- sort(r)

  ## detrend for worm plots?
  if (isTRUE(detrend)) {
    r <- r - out
  }

  out <- tibble(
    theoretical = out,
    residuals = r
  )
  out
}

`qq_uniform_quantiles` <- function(
  qs, q_fun, fit, weights, sigma2, dev_resid_fun,
  var_fun, type, na_action, model
) {
  ## generate quantiles for uniforms from q_fun
  qq <- q_fun(qs, fit, weights, sigma2)
  ## new residuals
  r <- compute_residuals(qq,
    fit = fit, weights = weights, type = type,
    dev_resid_fun = dev_resid_fun, var_fun = var_fun,
    na_action = na_action, model = model
  )
  ## sort residuals & return
  sort(r)
}

#' @importFrom stats naresid
`compute_residuals` <- function(y, fit, weights,
                                type = c("deviance", "response", "pearson"),
                                dev_resid_fun, var_fun, na_action, model) {
  type <- match.arg(type)

  r <- switch(type,
    deviance = deviance_residuals(y, fit, weights, dev_resid_fun,
      model = model
    ),
    response = response_residuals(y, fit),
    pearson = pearson_residuals(y, fit, weights, var_fun)
  )
  ## apply any na.action
  naresid(na_action, r)
}

`response_residuals` <- function(y, fit) {
  y - fit
}

`deviance_residuals` <- function(y, fit, weights, dev_resid_fun, model) {
  if ("object" %in% names(formals(dev_resid_fun))) {
    # have to handle families that provide a residuals function, which
    # takes the fitted model as input
    model$y <- y
    model$fitted.values <- fit
    model$prior.weights <- weights
    # r <- dev_resid_fun(list(y = y, fitted.values = fit,
    #                        prior.weights = weights),
    #                   type = "deviance")
    r <- dev_resid_fun(model, type = "deviance")
  } else {
    ## compute deviance residuals
    r <- dev_resid_fun(y, fit, weights)
    ## sign of residuals is typically an attribute
    posneg <- attr(r, "sign")
    ## ...but may be missing for some families
    if (is.null(posneg)) {
      posneg <- sign(y - fit)
    }
    ## calculate the deviance residuals
    r <- sqrt(pmax(r, 0)) * posneg
  }
  r
}

`pearson_residuals` <- function(y, fit, weights, var_fun) {
  ## if no variance function then bail out
  if (is.null(var_fun)) {
    stop("Pearson residuals are not available for this family.")
  }
  ## compute pearson residuals
  (y - fit) * sqrt(weights) / sqrt(var_fun(fit))
}

#' @title Plot of residuals versus linear predictor values
#'
#' @param model a fitted model. Currently only class `"gam"`.
#' @param type character; type of residuals to use. One of `"deviance"`,
#'   `"response"`, `"pearson"`, `"pit"`, and `"quantile"` residuals are
#'   allowed. `"pit"` uses probability integral transform (PIT) residuals,
#'   which, if the model is correct should be approximately uniformly
#'   distributed, while `"quantile"` transforms the PIT residuals through
#'   application of the inverse CDF of the standard normal, and therefore the
#'   quantile residuals should be approximately normally distributed (mean = 0,
#'   sd = 1) if the model is correct. PIT and quantile residuals are not yet
#'   available for most families that can be handled by `gam()`, but most
#'   standard families are supported, e.g. those used by `glm()`.
#' @param xlab character or expression; the label for the y axis. If not
#'   supplied, a suitable label will be generated.
#' @param ylab character or expression; the label for the y axis. If not
#'   supplied, a suitable label will be generated.
#' @param title character or expression; the title for the plot. See
#'   [ggplot2::labs()].
#' @param subtitle character or expression; the subtitle for the plot. See
#'   [ggplot2::labs()].
#' @param caption character or expression; the plot caption. See
#'   [ggplot2::labs()].
#' @param point_col colour used to draw points in the plots. See
#'   [graphics::par()] section **Color Specification**. This is passed to
#'   the individual plotting functions, and therefore affects the points of
#'   all plots.
#' @param point_alpha numeric; alpha transparency for points in plots.
#' @param line_col colour specification for 1:1 line.
#' @param seed integer; random seed to use for PIT or quantile residuals.
#'
#' @export
#'
#' @importFrom stats napredict residuals
#' @importFrom tools toTitleCase
#' @importFrom ggplot2 ggplot aes geom_point geom_hline labs
`residuals_linpred_plot` <- function(
  model,
  type = c("deviance", "pearson", "response", "pit", "quantile"),
  ylab = NULL,
  xlab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  point_col = "black",
  point_alpha = 1,
  line_col = "red",
  seed = NULL
) {
  type <- match.arg(type)
  y_intercept <- 0
  if (type %in% c("pit", "quantile")) {
    r <- quantile_residuals(model, type = type, seed = seed)
    if (is.null(ylab) && type == "pit") {
      ylab <- "PIT residuals"
      y_intercept <- 0.5
    }
  } else {
    r <- residuals(model, type = type)
    y_intercept <- 0
  }
  mv_y <- family_name(model) %in% multivariate_y()
  if (is.matrix(r) && mv_y) { # handle mvn() like fits
    r <- as.vector(r)
  }
  eta <- model[["linear.predictors"]]
  if (is.matrix(eta) && mv_y) {
    eta <- as.vector(eta) # handle multinom(), mvn() like fits
  }

  na_action <- na.action(model)
  if (is.matrix(eta) && !is.matrix(r)) {
    eta <- eta[, 1]
  }
  eta <- napredict(na_action, eta)

  df <- data.frame(eta = eta, residuals = r)
  plt <- ggplot(df, aes(
    x = .data$eta,
    y = .data$residuals
  )) +
    geom_hline(yintercept = y_intercept, col = line_col)

  ## add point layer
  plt <- plt + geom_point(colour = point_col, alpha = point_alpha)

  ## add labels
  if (is.null(xlab)) {
    xlab <- "Linear predictor"
  }
  if (is.null(ylab)) {
    ylab <- paste(toTitleCase(type), "residuals")
  }
  if (missing(title)) {
    title <- "Residuals vs linear predictor"
  }
  if (missing(subtitle)) {
    subtitle <- paste("Family:", family(model)[["family"]])
  }

  plt <- plt + labs(
    x = xlab, y = ylab, title = title, subtitle = subtitle,
    caption = caption
  )

  plt
}

#' @title Plot of fitted against observed response values
#'
#' @inheritParams residuals_linpred_plot
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point labs
`observed_fitted_plot` <- function(
  model,
  ylab = NULL,
  xlab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  point_col = "black",
  point_alpha = 1
) {
  ## extract data for plot
  fit <- fitted(model)
  ## handle case where fitted is a matrix; extended.families
  ##   - the needs to be more involved as what about mvn or multinom families
  mv_y <- family_name(model) %in% multivariate_y()
  if (NCOL(fit) > 1L) {
    fit <- if (mv_y) {
      as.vector(fit) # was fit[, 1], but changed for mvn
    } else {
      fit[, 1]
    }
  }
  obs <- as.vector(model[["y"]]) # also for mvn, etc

  df <- data.frame(observed = obs, fitted = fit)

  ## base plot
  plt <- ggplot(df, aes(
    x = .data$fitted,
    y = .data$observed
  ))

  ## add point layer
  plt <- plt + geom_point(colour = point_col, alpha = point_alpha)

  ## add labels
  if (is.null(xlab)) {
    xlab <- "Fitted values"
  }
  if (is.null(ylab)) {
    ylab <- "Response"
  }
  if (missing(title)) {
    title <- "Observed vs fitted values"
  }
  if (missing(subtitle)) {
    subtitle <- paste("Family:", family(model)[["family"]])
  }

  plt <- plt + labs(
    x = xlab, y = ylab, title = title, subtitle = subtitle,
    caption = caption
  )

  plt
}

#' @title Histogram of model residuals
#'
#' @param n_bins character or numeric; either the number of bins or a string
#'   indicating how to calculate the number of bins.
#'
#' @inheritParams residuals_linpred_plot
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_histogram labs
#' @importFrom tools toTitleCase
#' @importFrom stats residuals
#' @importFrom grDevices nclass.Sturges nclass.scott nclass.FD
`residuals_hist_plot` <- function(
  model,
  type = c("deviance", "pearson", "response", "pit", "quantile"),
  n_bins = c("sturges", "scott", "fd"),
  ylab = NULL, xlab = NULL, title = NULL,
  subtitle = NULL, caption = NULL,
  seed = NULL
) {
  ## extract data for plot
  type <- match.arg(type)

  if (type %in% c("pit", "quantile")) {
    r <- quantile_residuals(model, type = type, seed = seed)
  } else {
    r <- as.vector(residuals(model, type = type))
  }
  df <- data.frame(residuals = r)

  ## work out number of bins
  if (is.character(n_bins)) {
    n_bins <- match.arg(n_bins)
    n_bins <- switch(n_bins,
      sturges = nclass.Sturges(df[["residuals"]]),
      scott   = nclass.scott(df[["residuals"]]),
      fd      = nclass.FD(df[["residuals"]])
    )
    n_bins <- n_bins + 2
  }
  ## now n_bins should be numeric, if not bail
  if (!is.numeric(n_bins)) {
    stop(
      "'n_bins' should be a number or one of: ",
      paste(dQuote(c("sturges", "scott", "fd")),
        collapse = ", "
      )
    )
  }

  ## base plot
  plt <- ggplot(df, aes(x = .data$residuals))

  ## add point layer
  plt <- plt + geom_histogram(
    bins = n_bins,
    colour = "black",
    fill = "grey80",
    center = 0
  )

  ## add labels
  if (is.null(xlab)) {
    xlab <- paste(toTitleCase(type), "residuals")
  }
  if (is.null(ylab)) {
    ylab <- "Frequency"
  }
  if (missing(title)) {
    title <- "Histogram of residuals"
  }
  if (missing(subtitle)) {
    subtitle <- paste("Family:", family(model)[["family"]])
  }

  plt <- plt + labs(
    x = xlab, y = ylab, title = title, subtitle = subtitle,
    caption = caption
  )

  plt
}

#' Worm plot of model residuals
#'
#' @inheritParams qq_plot
#'
#' @export
`worm_plot` <- function(model, ...) {
  UseMethod("worm_plot")
}

#' @export
`worm_plot.default` <- function(model, ...) {
  stop("Unable to produce a worm plot for <",
    class(model)[[1L]], ">",
    call. = FALSE
  ) # don't show the call, simpler error
}

#' @inheritParams qq_plot.gam
#' @rdname worm_plot
#'
#' @note The wording used in [mgcv::qq.gam()] uses *direct* in reference to the
#'   simulated residuals method (`method = "simulated"`). To avoid confusion,
#'   `method = "direct"` is deprecated in favour of `method = "uniform"`.
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_point geom_hline geom_ribbon labs aes
#' @importFrom tools toTitleCase
#'
#' @examples
#' load_mgcv()
#' ## simulate binomial data...
#' dat <- data_sim("eg1", n = 200, dist = "binary", scale = .33, seed = 0)
#' p <- binomial()$linkinv(dat$f) # binomial p
#' n <- sample(c(1, 3), 200, replace = TRUE) # binomial n
#' dat <- transform(dat, y = rbinom(n, n, p), n = n)
#' m <- gam(y / n ~ s(x0) + s(x1) + s(x2) + s(x3),
#'   family = binomial, data = dat, weights = n,
#'   method = "REML"
#' )
#'
#' ## Worm plot; default using direct randomization of uniform quantiles
#' ## Note no reference bands are drawn with this method.
#' worm_plot(m)
#'
#' ## Alternatively use simulate new data from the model, which
#' ## allows construction of reference intervals for the Q-Q plot
#' worm_plot(m,
#'   method = "simulate", point_col = "steelblue",
#'   point_alpha = 0.4
#' )
#'
#' ## ... or use the usual normality assumption
#' worm_plot(m, method = "normal")
`worm_plot.gam` <- function(
  model,
  method = c("uniform", "simulate", "normal", "direct"),
  type = c("deviance", "response", "pearson"),
  n_uniform = 10, n_simulate = 50,
  level = 0.9,
  ylab = NULL, xlab = NULL,
  title = NULL, subtitle = NULL, caption = NULL,
  ci_col = "black",
  ci_alpha = 0.2,
  point_col = "black",
  point_alpha = 1,
  line_col = "red",
  ...
) {
  method <- match.arg(method) # what method for the QQ plot?
  if (identical(method, "direct")) {
    message("`method = \"direct\"` is deprecated, use `\"uniform\"`")
    method <- "uniform"
  }
  ## check if we can do the method
  if (identical(method, "uniform") &&
    is.null(fix.family.qf(family(model))[["qf"]])) {
    method <- "simulate"
  }
  if (identical(method, "simulate") &&
    is.null(fix.family.rd(family(model))[["rd"]])) {
    method <- "normal"
  }

  if (level <= 0 || level >= 1) {
    stop("Level must be 0 < level < 1. Supplied level <", level, ">",
      call. = FALSE
    )
  }

  type <- match.arg(type) # what type of residuals

  ## generate theoretical quantiles
  df <- switch(method,
    uniform = qq_uniform(model,
      n = n_uniform, type = type,
      level = level, detrend = TRUE
    ),
    simulate = qq_simulate(model,
      n = n_simulate, type = type,
      level = level, detrend = TRUE
    ),
    normal = qq_normal(model,
      type = type, level = level,
      detrend = TRUE
    )
  )
  df <- as_tibble(df)

  ## add labels if not supplied
  if (is.null(ylab)) {
    ylab <- paste(toTitleCase(type), "residuals (Deviation)")
  }

  if (is.null(xlab)) {
    xlab <- "Theoretical quantiles"
  }

  if (is.null(title)) {
    title <- "Worm plot of residuals"
  }

  if (is.null(subtitle)) {
    subtitle <- paste("Method:", method)
  }

  ## base plot
  plt <- ggplot(df, aes(
    x = .data$theoretical,
    y = .data$residuals
  ))

  ## Now need a reference horizonta line
  plt <- plt + geom_hline(yintercept = 0, col = line_col)

  ## add reference interval
  if (isTRUE(method %in% c("simulate", "normal"))) {
    plt <- plt + geom_ribbon(
      aes(
        ymin = .data$lower,
        ymax = .data$upper,
        x = .data$theoretical
      ),
      inherit.aes = FALSE,
      alpha = ci_alpha, fill = ci_col
    )
  }

  ## add point layer
  plt <- plt + geom_point(colour = point_col, alpha = point_alpha)

  ## add labels
  plt <- plt + labs(
    title = title, subtitle = subtitle, caption = caption,
    y = ylab, x = xlab
  )

  ## return
  plt
}


#' @export
#' @rdname worm_plot
`worm_plot.glm` <- function(model, ...) {
  if (is.null(model[["sig2"]])) {
    model[["sig2"]] <- summary(model)$dispersion
  }
  worm_plot.gam(model, ...)
}

#' @rdname worm_plot
#' @importFrom stats df.residual
#' @export
`worm_plot.lm` <- function(model, ...) {
  r <- residuals(model)
  r.df <- df.residual(model)
  model[["sig2"]] <- sum((r - mean(r))^2) / r.df
  if (is.null(weights(model))) {
    model$prior.weights <- rep(1, nrow(model.frame(model)))
  }
  if (is.null(model[["linear.predictors"]])) {
    model[["linear.predictors"]] <- model[["fitted.values"]]
  }
  worm_plot.gam(model, ...)
}

#' @export
`weights.lm` <- function(object, type = c("prior", "working"), ...) {
  type <- match.arg(type)
  wts <- if (type == "prior") {
    object$prior.weights
  } else {
    object$weights
  }
  if (is.null(object$na.action)) {
    wts
  } else {
    naresid(object$na.action, wts)
  }
}
