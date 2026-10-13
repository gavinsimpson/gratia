## Functions to diagnose problems with fitted GAMs

#' Quantile-quantile plot of model residuals
#'
#' Quantile-quantile plots (QQ-plots) for GAMs using the reference quantiles of
#' Augustin *et al* (2012), or fixed normal references for randomised quantile
#' residuals following Dunn and Smyth (1996).
#'
#' @param model a fitted model. Currently models inheriting from class `"gam"`,
#'   as well as classes `"glm"` and `"lm"` from calls to [stats::glm] or
#'   [stats::lm] are supported.
#' @param method character or `NULL`; reference-quantile method. For quantile
#'   residuals, `NULL` and `"normal"` use a fixed standard-normal reference;
#'   `"uniform"` and `"simulate"` require a traditional residual type instead.
#'   For grouped-family traditional residuals, `NULL` selects `"simulate"` when
#'   every component has a generator, or `"uniform"` with a message when every
#'   component has an inverse CDF. An unavailable reference distribution is an
#'   error; `"normal"` is available only as an explicit approximation.
#'   For ordinary models, `NULL` retains the default `"simulate"` for Tweedie
#'   families and `"uniform"` otherwise, with existing availability fallbacks.
#'   `"uniform"` averages residual reference quantiles obtained by inverse-CDF
#'   sampling over `n_uniform` draws. `"simulate"` uses `n_simulate` simulated
#'   response data sets at the observed covariates and permits reference bands.
#'   `"normal"` uses a location/scale-fitted normal approximation for traditional
#'   residuals. `"direct"` is deprecated in favour of `"uniform"`.
#'
#' @param by_family logical or `NULL`; separate indexed grouped-family components
#'   into facets. The default pools quantile residuals with component colours
#'   and facets traditional residuals. `TRUE` and `FALSE` explicitly select the
#'   layout. In `appraise()`, observed-versus-fitted values always use component
#'   facets. Standalone `observed_fitted_plot()` facets by default and permits
#'   explicit pooling. Ignored for ordinary models.
#' @param type character or `NULL`; residual type: `"deviance"`, `"response"`,
#'   `"pearson"`, or `"quantile"`. The default selects supported grouped-family
#'   quantile residuals and ordinary-model deviance residuals.
#' @param n_uniform numeric; number of times to randomize uniform quantiles
#'   in the direct computation method (`method = "uniform"`).
#' @param n_simulate numeric; number of data sets to simulate from the estimated
#'   model when using the simulation method (`method = "simulate"`).
#' @param seed numeric; the random number seed to use for `method = "simulate"`
#'   and `method = "uniform"`, and for randomised quantile residuals.
#' @param level numeric; the coverage level for reference intervals. Must be
#'   strictly `0 < level < 1`. Used for quantile reference bands and traditional
#'   `method = "simulate"` or `"normal"` bands.
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
#' @param ci_col fill colour for reference intervals, including calibrated
#'   quantile-residual bands.
#' @param ci_alpha alpha transparency for reference intervals.
#' @param point_col constant point colour when family components are not mapped
#'   to colour. Pooled grouped diagnostics instead colour points by component.
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
#' @details See [appraise()] for grouped-family defaults, distributional support,
#'   pooling and faceting. Quantile QQ/worm plots use fixed standard-normal
#'   reference quantiles and pointwise order-statistic bands for an ideal
#'   independent normal sample, conditional on fitted distribution parameters;
#'   they do not include fitting uncertainty or provide joint coverage.
#'   Traditional residuals instead use the fitted response distributions or an
#'   explicitly requested normal approximation.
#'
#' @references
#'
#' The underlying methodology used when `method` is `"simulate"` or `"uniform"`
#' is described in Augustin *et al* (2012):
#'
#' Dunn, P.K. and Smyth, G.K. (1996) Randomized quantile residuals.
#' *Journal of Computational and Graphical Statistics* **5**, 236--244.
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

# Choose the automatic method before applying family-availability fallbacks.
# Tweedie numerical quantiles are much slower than random generation (#410).
diagnostic_method <- function(model, method = NULL) {
  if (is_gfam_family(family(model))) return(gfam_diagnostic_method(model, method))
  if (is.null(method)) {
    method <- if (identical(family_type(model), "tweedie")) {
      "simulate"
    } else {
      "uniform"
    }
  }
  method <- match.arg(method, c("uniform", "simulate", "normal", "direct"))
  if (identical(method, "direct")) {
    message("`method = \"direct\"` is deprecated, use `\"uniform\"`")
    method <- "uniform"
  }
  method
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
#' @importFrom stringr str_to_title
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
  method = NULL,
  type = NULL,
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
  point_col = "grey20",
  point_alpha = 1,
  line_col = "red", by_family = NULL, ...
) {
  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
    on.exit({
      if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
      else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(seed)
  }

  # figure out method stuff
  type <- diagnostic_type(model, type)
  by_family <- diagnostic_by_family(model, type, by_family)
  if (type == "quantile") {
    return(calibrated_qq_plot(model, method, level, FALSE, by_family,
      ylab, xlab, title, subtitle, caption, ci_col, ci_alpha,
      point_col, point_alpha, line_col))
  }
  method <- diagnostic_method(model, method)
  ## check if we can do the uniform method - needs a quantile fun
  grouped <- is_gfam_family(family(model))
  ff_qf <- if (!grouped && identical(method, "uniform")) fix_family_qf(family(model))[["qf"]]
  if (!grouped && identical(method, "uniform") && is.null(ff_qf)) {
    method <- "simulate"
  }
  ff_rd <- if (!grouped) fix_family_rd(family(model))[["rd"]]
  if (!grouped && identical(method, "simulate") && is.null(ff_rd)) {
    method <- "normal"
  }
  if (level <= 0 || level >= 1) {
    stop("Level must be 0 < level < 1. Supplied level <", level, ">",
      call. = FALSE
    )
  }

  ## r <- residuals(model, type = type)  # model residuals
  ## generate theoretical quantiles
  df <- switch(method,
    uniform = qq_uniform(model, n = n_uniform, type = type, by_family = by_family),
    simulate = qq_simulate(model,
      n = n_simulate, type = type,
      level = level, by_family = by_family
    ),
    normal = qq_normal(model, type = type, level = level, by_family = by_family)
  )
  df <- as_tibble(df)
  ## add labels if not supplied
  if (is.null(ylab)) {
    ylab <- paste(str_to_title(type), "residuals")
  }
  if (is.null(xlab)) {
    xlab <- "Theoretical quantiles"
  }
  if (is.null(title)) {
    title <- "QQ plot of residuals"
  }
  if (is.null(subtitle)) {
    subtitle <- if (grouped && method == "normal") "Method: normal (approximation)"
      else paste("Method:", method)
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

  if (method == "normal" && ".family" %in% names(df)) {
    lines <- unique(df[c(".family", ".qq_slope", ".qq_intercept")])
    plt <- plt + geom_abline(data = lines,
      aes(slope = .data$.qq_slope, intercept = .data$.qq_intercept), col = line_col)
  } else {
    plt <- plt + geom_abline(slope = qq_slope, intercept = qq_intercept, col = line_col)
  }
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
  plt <- plt + geom_point(colour = point_col, alpha = point_alpha,
      data = function(d) d[stats::complete.cases(d), , drop = FALSE])
  ## add labels
  plt <- plt + labs(
    title = title, subtitle = subtitle, caption = caption,
    y = ylab, x = xlab
  )
  ## return
  gfam_diagnostic_facets(plt, model, by_family)
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
  model <- model_used_rows(model)
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
                          level = 0.9, detrend = FALSE, by_family = TRUE) {
  if (inherits(model, "gratia_glmmtmb_diagnostic")) {
    return(qq_simulate_glmmtmb(model, n, type, level, detrend))
  }
  model <- model_used_rows(model)
  type <- match.arg(type)
  if (is_gfam_family(family(model))) {
    return(gfam_qq(model, n, type, "simulate", level, detrend, by_family))
  }
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
  prior_w <- diagnostic_weights(model)
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
  # Percentage names are unused and expensive to format for every row.
  out <- quantile(sims, probs = (seq_len(n_obs) - 0.5) / n_obs, names = FALSE)
  int <- apply(sims, 1L, quantile, probs = c(alpha, 1 - alpha), names = FALSE)
  r <- diagnostic_residuals(model, type = type)
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
                        level = 0.9, detrend = FALSE, by_family = TRUE) {
  se_zscore <- function(z) {
    n <- length(z)
    pnorm_z <- pnorm(z)
    sqrt(pnorm_z * (1 - pnorm_z) / n) / dnorm(z)
  }
  model <- model_used_rows(model)
  type <- match.arg(type)
  if (is_gfam_family(family(model))) {
    return(gfam_qq(model, 1L, type, "normal", level, detrend, by_family))
  }
  r <- diagnostic_residuals(model, type = type)
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
                         level = 0.9, detrend = FALSE, by_family = TRUE) {
  model <- model_used_rows(model)
  type <- match.arg(type)
  if (is_gfam_family(family(model))) {
    return(gfam_qq(model, n, type, "uniform", level, detrend, by_family))
  }
  family <- family(model) # extract family
  family <- fix_family_qf(family) # add quantile fun to family
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
  r <- diagnostic_residuals(model, type = type)
  fit <- fitted(model)
  weights <- diagnostic_weights(model)
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

  if ("object" %in% names(formals(dev_resid_fun))) {
    model$y <- y
    model$fitted.values <- fit
    model$prior.weights <- weights
    return(naresid(na_action, dev_resid_fun(model, type = type)))
  }

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
#' @param type character or `NULL`; residual type: `"deviance"`, `"pearson"`,
#'   `"response"`, `"pit"`, or `"quantile"`. Grouped models automatically use
#'   quantile residuals when all active components are supported, otherwise all
#'   use deviance residuals with a message. Ordinary models retain deviance
#'   residuals by default. See [appraise()] for distributional support.
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
#' @param point_col constant point colour when components are not mapped to
#'   colour. Pooled grouped diagnostics instead colour points by component.
#' @param point_alpha numeric; alpha transparency for points in plots.
#' @param line_col colour specification for 1:1 line.
#' @param by_family logical or `NULL`; separate indexed grouped-family components
#'   into facets. The default pools quantile residuals with component colours
#'   and facets traditional residuals. `TRUE` and `FALSE` explicitly select the
#'   layout. In `appraise()`, observed-versus-fitted values always use component
#'   facets. Standalone `observed_fitted_plot()` facets by default and permits
#'   explicit pooling. Ignored for ordinary models.
#' @param rank logical or `NULL`; use within-component ranks on the horizontal
#'   axis of residual-versus-predictor plots. The default ranks pooled grouped
#'   plots and retains original predictors in facets. `TRUE` or `FALSE` overrides
#'   this independently of the residual type and layout. Only finite observations
#'   included in each component determine its ranks; average ties are used.
#' @param seed integer; random seed to use for PIT or quantile residuals.
#'
#' @export
#'
#' @importFrom stats napredict residuals
#' @importFrom stringr str_to_title
#' @importFrom ggplot2 ggplot aes geom_point geom_hline labs
`residuals_linpred_plot` <- function(
  model,
  type = NULL,
  ylab = NULL,
  xlab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  point_col = "grey20",
  point_alpha = 1,
  line_col = "red",
  seed = NULL,
  by_family = NULL,
  rank = NULL
) {
  if (inherits(model, "glmmTMB")) {
    type <- if (is.null(type)) "pearson" else
      match.arg(type, c("deviance", "pearson", "response"))
    model <- glmmtmb_diagnostic_model(model, type = type)
  }

  if (inherits(model, "gamm4")) model <- gamm4_gam(model)
  original <- model
  model <- model_used_rows(model)
  type <- diagnostic_type(model, type, allow_pit = TRUE)
  by_family <- diagnostic_by_family(model, type, by_family)
  y_intercept <- 0
  if (type %in% c("pit", "quantile")) {
    r <- diagnostic_residuals(model, type = type, seed = seed)
    if (is.null(ylab) && type == "pit") {
      ylab <- "PIT residuals"
      y_intercept <- 0.5
    }
  } else {
    r <- diagnostic_residuals(model, type = type)
    y_intercept <- 0
  }
  r <- restore_model_rows(r, original)
  mv_y <- family_name(model) %in% multivariate_y()
  if (is.matrix(r) && mv_y) { # handle mvn() like fits
    r <- as.vector(r)
  }
  eta <- restore_model_rows(model[["linear.predictors"]], original)
  if (is.matrix(eta) && mv_y) {
    eta <- as.vector(eta) # handle multinom(), mvn() like fits
  }

  na_action <- na.action(model)
  if (is.matrix(eta) && !is.matrix(r)) {
    eta <- eta[, 1]
  }
  eta <- napredict(na_action, eta)

  df <- diagnostic_component_data(data.frame(eta = eta, residuals = r), original)
  df <- diagnostic_predictor_data(df, original, by_family, rank)
  coloured <- is_gfam_family(family(model)) && !by_family
  plt <- ggplot(df, aes(
    x = .data$eta,
    y = .data$residuals
  )) +
    geom_hline(yintercept = y_intercept, col = line_col)

  ## add point layer
  if (coloured) {
    plt <- plt + geom_point(aes(colour = .data$.family), alpha = point_alpha,
      data = function(d) d[stats::complete.cases(d), , drop = FALSE]) +
      ggokabeito::scale_colour_okabe_ito(limits = gfam_diagnostic_labels(original),
        drop = FALSE)
  } else {
    plt <- plt + geom_point(colour = point_col, alpha = point_alpha,
      data = function(d) d[stats::complete.cases(d), , drop = FALSE])
  }

  ## add labels
  if (is.null(xlab)) {
    xlab <- if (isTRUE(attr(df, "ranked"))) "Within-component linear predictor rank"
      else "Linear predictor"
  }
  if (is.null(ylab)) {
    ylab <- paste(str_to_title(type), "residuals")
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
  if (coloured) plt <- plt + labs(colour = "Family component")

  gfam_diagnostic_facets(plt, original, by_family)
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
  point_col = "grey20",
  point_alpha = 1,
  by_family = NULL
) {
  if (inherits(model, "glmmTMB")) {
    model <- glmmtmb_diagnostic_model(model, type = "response")
  }

  if (inherits(model, "gamm4")) model <- gamm4_gam(model)
  ## extract data for plot
  original <- model
  model <- model_used_rows(model)
  check_gfam_appraise(model)
  by_family <- diagnostic_by_family(model, "response", by_family)
  fit <- if (is_gfam_family(family(model))) {
    context <- gfam_diagnostic_context(model)
    as.numeric(gfam_response_means(matrix(context$eta, ncol = 1L), context))
  } else fitted(model)
  fit <- restore_model_rows(fit, original)
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
  obs <- model[["y"]]
  if (is.null(obs)) obs <- stats::model.response(stats::model.frame(model))
  obs <- as.vector(restore_model_rows(obs, original))

  df <- data.frame(observed = obs, fitted = fit)

  ## base plot
  plt <- ggplot(df, aes(
    x = .data$fitted,
    y = .data$observed
  ))

  ## add point layer
  plt <- plt + geom_point(colour = point_col, alpha = point_alpha,
      data = function(d) d[stats::complete.cases(d), , drop = FALSE])

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

  plt <- gfam_diagnostic_facets(plt, original, by_family)
  if (is_gfam_family(family(model)) && by_family)
    plt <- plt + ggplot2::scale_x_continuous(breaks = gfam_observed_breaks(plt$data))
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
#' @importFrom stringr str_to_title
#' @importFrom stats residuals
#' @importFrom grDevices nclass.Sturges nclass.scott nclass.FD
`residuals_hist_plot` <- function(
  model,
  type = NULL,
  n_bins = c("sturges", "scott", "fd"),
  ylab = NULL, xlab = NULL, title = NULL,
  subtitle = NULL, caption = NULL,
  seed = NULL,
  by_family = NULL
) {
  if (inherits(model, "glmmTMB")) {
    type <- if (is.null(type)) "pearson" else
      match.arg(type, c("deviance", "pearson", "response"))
    model <- glmmtmb_diagnostic_model(model, type = type)
  }

  if (inherits(model, "gamm4")) model <- gamm4_gam(model)
  ## extract data for plot
  original <- model
  model <- model_used_rows(model)
  type <- diagnostic_type(model, type, allow_pit = TRUE)
  by_family <- diagnostic_by_family(model, type, by_family)

  if (type %in% c("pit", "quantile")) {
    r <- diagnostic_residuals(model, type = type, seed = seed)
  } else {
    r <- diagnostic_residuals(model, type = type)
  }
  r <- as.vector(restore_model_rows(r, original))
  df <- diagnostic_component_data(data.frame(residuals = r), original)
  pooled <- is_gfam_family(family(model)) && !by_family

  ## work out number of bins
  if (is.character(n_bins)) {
    n_bins <- match.arg(n_bins)
    n_bins <- switch(n_bins,
      sturges = nclass.Sturges(stats::na.omit(df[["residuals"]])),
      scott   = nclass.scott(stats::na.omit(df[["residuals"]])),
      fd      = nclass.FD(stats::na.omit(df[["residuals"]]))
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
  if (pooled) {
    valid <- df$residuals[is.finite(df$residuals)]
    # All components use one set of bin edges; density normalization makes
    # unequal component sizes comparable without silently changing weights.
    limits <- range(c(valid, 0))
    if (diff(limits) == 0) limits <- limits + c(-.5, .5)
    breaks <- seq(limits[1L], limits[2L], length.out = max(2L, n_bins + 1L))
    plt <- plt + geom_histogram(aes(y = ggplot2::after_stat(.data$density), fill = .data$.family),
      data = function(d) d[is.finite(d$residuals), , drop = FALSE],
      breaks = breaks, colour = "white", position = "identity", alpha = .45) +
      ggokabeito::scale_fill_okabe_ito(limits = gfam_diagnostic_labels(original),
        drop = FALSE)
  } else {
    plt <- plt + geom_histogram(
      data = function(d) d[!is.na(d$residuals), , drop = FALSE], bins = n_bins,
      colour = "white", fill = "grey20", center = 0)
  }

  ## add labels
  if (is.null(xlab)) {
    xlab <- paste(str_to_title(type), "residuals")
  }
  if (is.null(ylab)) {
    ylab <- if (pooled) "Density" else "Frequency"
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
  if (pooled) plt <- plt + labs(fill = "Family component")

  gfam_diagnostic_facets(plt, original, by_family)
}

#' Worm plot of model residuals
#'
#' @inherit qq_plot details
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
#' @importFrom stringr str_to_title
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
  method = NULL,
  type = NULL,
  n_uniform = 10, n_simulate = 50,
  level = 0.9,
  ylab = NULL, xlab = NULL,
  title = NULL, subtitle = NULL, caption = NULL,
  ci_col = "black",
  ci_alpha = 0.2,
  point_col = "grey20",
  point_alpha = 1,
  line_col = "red",
  seed = NULL,
  by_family = NULL,
  ...
) {
  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    }
    on.exit({
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(seed)
  }

  type <- diagnostic_type(model, type)
  by_family <- diagnostic_by_family(model, type, by_family)
  if (type == "quantile") {
    return(calibrated_qq_plot(model, method, level, TRUE, by_family,
      ylab, xlab, title, subtitle, caption, ci_col, ci_alpha,
      point_col, point_alpha, line_col))
  }
  method <- diagnostic_method(model, method)
  ## check if we can do the method
  grouped <- is_gfam_family(family(model))
  if (!grouped && identical(method, "uniform") &&
    is.null(fix_family_qf(family(model))[["qf"]])) {
    method <- "simulate"
  }
  if (!grouped && identical(method, "simulate") &&
    is.null(fix_family_rd(family(model))[["rd"]])) {
    method <- "normal"
  }

  if (level <= 0 || level >= 1) {
    stop("Level must be 0 < level < 1. Supplied level <", level, ">",
      call. = FALSE
    )
  }



  ## generate theoretical quantiles
  df <- switch(method,
    uniform = qq_uniform(model,
      n = n_uniform, type = type,
      level = level, detrend = TRUE, by_family = by_family
    ),
    simulate = qq_simulate(model,
      n = n_simulate, type = type,
      level = level, detrend = TRUE, by_family = by_family
    ),
    normal = qq_normal(model,
      type = type, level = level,
      detrend = TRUE, by_family = by_family
    )
  )
  df <- as_tibble(df)

  ## add labels if not supplied
  if (is.null(ylab)) {
    ylab <- paste(str_to_title(type), "residuals (Deviation)")
  }

  if (is.null(xlab)) {
    xlab <- "Theoretical quantiles"
  }

  if (is.null(title)) {
    title <- "Worm plot of residuals"
  }

  if (is.null(subtitle)) {
    subtitle <- if (grouped && method == "normal") "Method: normal (approximation)"
      else paste("Method:", method)
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
  plt <- plt + geom_point(colour = point_col, alpha = point_alpha,
      data = function(d) d[stats::complete.cases(d), , drop = FALSE])

  ## add labels
  plt <- plt + labs(
    title = title, subtitle = subtitle, caption = caption,
    y = ylab, x = xlab
  )

  ## return
  gfam_diagnostic_facets(plt, model, by_family)
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
  model <- model_used_rows(model)
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

# Internal weights for diagnostics; keep base lm dispatch unchanged.
`diagnostic_weights` <- function(model) {
  if (inherits(model, "gam")) {
    return(stats::napredict(model$na.action, model$prior.weights))
  }
  if (inherits(model, "glm")) {
    return(stats::weights(model, type = "prior"))
  }
  w <- stats::weights(model)
  if (is.null(w)) {
    w <- stats::napredict(model$na.action, rep(1, nrow(model.frame(model))))
  }
  w
}
