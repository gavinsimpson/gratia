## Functions to diagnose problems with fitted GAMs

#' Quantile-quantile plot of model residuals
#'
#' Quantile-quantile plots (QQ plots) for fitted models using the reference quantiles of
#' Augustin *et al* (2012), or fixed normal references for randomised quantile
#' residuals following Dunn and Smyth (1996).
#'
#' @param model a fitted model supported by [diagnostic_support].
#' @inheritParams appraise
#' @param method character or `NULL`; how to calculate reference quantiles.
#'   Probability integral transform (PIT) and quantile residuals use fixed
#'   uniform and standard-normal references, respectively. For quantile residuals, `"normal"` also selects that fixed
#'   reference; other named methods require traditional residuals.
#'   For deviance, response and Pearson residuals:
#'
#'   * `"uniform"` applies the fitted inverse cumulative distribution function
#'     (CDF) to randomised uniform quantiles and averages the resulting ordered residuals over `n_uniform`
#'     draws.
#'   * `"simulate"` generates `n_simulate` response data sets at the fitting
#'     covariates. Responses are generated together when needed to preserve
#'     dependence, and their ordered residuals supply reference values and bands.
#'   * `"marginal"` approximates the pooled distribution of individual residuals,
#'     without bands for dependence. This is selected for BAM GEE and PQL
#'     correlations that do not define a joint response distribution. Explicit
#'     joint simulation is unavailable in those cases.
#'   * `"normal"` uses a normal approximation fitted to the residual location
#'     and scale. For dependent responses it omits bands based on independence.
#'   * `"direct"` is deprecated in favour of `"uniform"`.
#'
#'   For ordinary GAMs, `NULL` selects `"simulate"` for Tweedie and `"uniform"`
#'   otherwise. Missing inverse CDFs cause a switch to simulation; missing response
#'   generators can cause a switch to a normal approximation for independent
#'   responses. Dependent responses use joint simulation when available, or
#'   marginal checks otherwise. For grouped families, `NULL` selects simulation
#'   if every component has a generator, or inverse-CDF sampling with a message
#'   if every component has an inverse CDF. If neither is available, it gives an
#'   error; a normal approximation must be requested explicitly.
#'
#' @param by_family logical or `NULL`; separate indexed grouped-family components
#'   into facets. The default pools quantile residuals with component colours
#'   and facets traditional residuals. `TRUE` and `FALSE` explicitly select the
#'   layout. In `appraise()`, observed-versus-fitted values always use component
#'   facets. Standalone `observed_fitted_plot()` facets by default and permits
#'   explicit pooling. Ignored for ordinary models.
#' @param type character or `NULL`; residual type: `"deviance"`, `"response"`,
#'   `"pearson"`, `"pit"`, `"quantile"`, `"dharma"`, or `"surrogate"` (for
#'   `ocat()` or `multinom()` GAMs). `NULL` uses the model-specific default:
#'   deviance for ordinary lm/glm/GAM fits, and quantile for supported grouped
#'   families, GJRM, sdmTMB and full mixed-model checks. glmmTMB uses Pearson
#'   residuals conditionally and response residuals marginally. Extracted GAM
#'   components follow the ordinary GAM defaults. See [diagnostic_support].
#' @param n_uniform numeric; number of inverse-CDF draws to average when
#'   `method = "uniform"`.
#' @param n_simulate numeric; number of data sets to simulate from the estimated
#'   model when using the simulation method (`method = "simulate"`).
#' @param seed numeric; the random number seed to use for `method = "simulate"`
#'   and `method = "uniform"`, and for randomised quantile residuals.
#' @param level numeric; the coverage level for reference intervals. Must be
#'   strictly `0 < level < 1`. Used for quantile reference bands and traditional
#'   `method = "simulate"` or `"normal"` bands.
#' @param xlab character or expression; the label for the x axis. If not
#'   supplied, a suitable label will be generated.
#' @param ylab character or expression; the label for the y axis. If not
#'   supplied, a suitable label will be generated.
#' @param title character or expression; the title for the plot. See
#'   [ggplot2::labs()]. May be a vector, one per penalty.
#' @param subtitle character or expression; the subtitle for the plot. See
#'   [ggplot2::labs()]. May be a vector, one per penalty.
#' @param caption character or expression; the plot caption. See
#'   [ggplot2::labs()]. May be a vector, one per penalty.
#' @param ci_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Fill colour for reference intervals, including bands for
#'   quantile residuals.
#' @param ci_alpha Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Alpha transparency for reference intervals.
#' @param point_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Constant point colour when family components are not mapped
#'   to colour. Pooled grouped diagnostics instead colour points by component.
#' @param point_alpha Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Alpha transparency of points on the QQ plot.
#' @param line_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Colour used to draw the reference line.
#' @param ... arguments passed to other methods.
#'
#' @note The wording used in [mgcv::qq.gam()] uses *direct* in reference to the
#'   simulated residuals method (`method = "simulated"`). To avoid confusion,
#'   `method = "direct"` is deprecated in favour of `method = "uniform"`.
#'
#' @seealso [mgcv::qq.gam] for more details on the methods used.
#'
#' @details See [appraise()] for grouped-family defaults, supported distributions,
#'   pooling and separate component panels. PIT and quantile residuals use fixed
#'   uniform and standard-normal reference quantiles. For ideal independent
#'   responses, bands are pointwise intervals for ordered residuals. For dependent
#'   responses, bands use simulations that preserve dependence when available;
#'   otherwise they are omitted. Residuals calculated from simulation ranks also
#'   use simulated bands. See [diagnostic_support] for these choices and limits.
#'   Bands condition on fitted parameters and exclude fitting uncertainty; they
#'   do not provide simultaneous coverage. Traditional residuals use the fitted
#'   response distributions or a normal approximation.
#'
#'   For [mgcv::ocat()] fits, `type = "surrogate"` uses logistic reference
#'   quantiles with location zero and scale one. Set `method = NULL`. Independent
#'   observations have approximate pointwise bands obtained by applying
#'   `qlogis()` to beta order-statistic limits. Worm plots subtract the same
#'   logistic reference quantiles from residuals and bands. These references
#'   treat fitted parameters as fixed. For [mgcv::multinom()] GAMs, surrogate
#'   residuals retain a utility component for every category, including 0.
#'   Components have separate facets and centred Gumbel reference quantiles,
#'   including when `by_family = FALSE`. The same reference is subtracted in
#'   worm plots, and pointwise beta order-statistic bands use the centred
#'   Gumbel quantile function separately for each component. Ordinary and
#'   scalable displays share their draw. See [surrogate_residuals()].
#'
#'   Subtitles name the method. Captions identify conditioning and field choices,
#'   joint simulation bands or omitted bands. Routine pointwise bands are unlabelled.
#'   Supply `caption` to replace this text, or `caption = ""` to omit it.
#'   Ordinary and scalable QQ and worm plots use the same labels.
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

# Choose a preferred method first, then check which distribution functions exist.
# Simulating Tweedie responses is much faster than inverting their CDF (#410).
diagnostic_method <- function(model, method = NULL) {
  if (is_gfam_family(family(model))) return(gfam_diagnostic_method(model, method))
  if (is.null(method)) {
    method <- if (identical(family_type(model), "tweedie")) {
      "simulate"
    } else {
      "uniform"
    }
  }
  method <- match.arg(method, c("uniform", "simulate", "normal", "direct", "marginal"))
  if (identical(method, "direct")) {
    message("`method = \"direct\"` is deprecated, use `\"uniform\"`")
    method <- "uniform"
  }
  method
}

#' @rdname qq_plot
#' @export
qq_plot.default <- function(model, ...) {
  scalable_qq(model, ..., aggregate = FALSE)
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
#' ## Alternatively simulate new data from the model, which
#' ## allows construction of reference intervals for the Q-Q plot
#' qq_plot(m,
#'   method = "simulate",
#'   seed = 42,
#'   point_col = "steelblue",
#'   point_alpha = 0.4
#' )
#'
#' ## ... or request a normal approximation for the residuals.
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
  line_col = "red", by_family = NULL, conditioning = NULL, ..., n_dharma = 250L
) {
  gratia_plot_arguments(c("ci_col", "ci_alpha", "point_col", "point_alpha", "line_col"), environment())
  if (!is.null(type)) type <- diagnostic_type(model, type)
  if (is_multinom_surrogate(model, type)) return(scalable_qq(model,
    method = method, type = "surrogate", seed = seed, level = level,
    xlab = xlab, ylab = ylab, title = title, subtitle = subtitle,
    caption = caption, ci_col = ci_col, ci_alpha = ci_alpha,
    point_col = point_col, point_alpha = point_alpha, line_col = line_col,
    by_family = by_family, conditioning = conditioning, aggregate = FALSE,
    worm = FALSE, ...))
  if (identical(type, "dharma") ||
      (is.null(type) && identical(attr(model, "gratia.default_residual_type"), "dharma"))) {
    return(scalable_qq(model, method = method, type = "dharma", seed = seed,
      level = level, xlab = xlab, ylab = ylab, title = title,
      subtitle = subtitle, caption = caption, ci_col = ci_col, ci_alpha = ci_alpha,
      point_col = point_col, point_alpha = point_alpha, line_col = line_col,
      by_family = by_family, conditioning = conditioning,
      n_dharma = if (missing(n_dharma)) NULL else n_dharma,
      aggregate = FALSE, worm = FALSE, ...))
  }
  model <- diagnostic_plot_source(model, type, seed, conditioning, ...)

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
  if (type %in% c("pit", "quantile", "surrogate")) {
    return(calibrated_qq_plot(model, method, level, FALSE, by_family,
      ylab, xlab, title, subtitle, caption, ci_col, ci_alpha,
      point_col, point_alpha, line_col, type = type, n_simulate = n_simulate))
  }
  method <- diagnostic_reference_method(model, method)
  if (level <= 0 || level >= 1) {
    stop("Level must be 0 < level < 1. Supplied level <", level, ">",
      call. = FALSE
    )
  }

  ## r <- residuals(model, type = type)  # model residuals
  ## generate theoretical quantiles
  df <- switch(method,
    marginal = qq_marginal(model, n = n_simulate, type = type, level = level,
      by_family = by_family),
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
    subtitle <- diagnostic_qq_subtitle(method)
  }
  caption <- caption %||% diagnostic_qq_caption(model, df)
  ## base plot
  plt <- gratia_plot(df, aes(
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
    plt <- plt + geom_gratia_diagnostic_reference_abline(data = lines,
      aes(slope = .data$.qq_slope, intercept = .data$.qq_intercept), col = line_col)
  } else {
    plt <- plt + geom_gratia_diagnostic_reference_abline(slope = qq_slope, intercept = qq_intercept, col = line_col)
  }
  ## add reference interval
  if (all(c("lower", "upper") %in% names(df))) {
    plt <- plt + geom_gratia_diagnostic_interval_ribbon(
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
  plt <- plt + geom_gratia_diagnostic_point(colour = point_col, alpha = point_alpha,
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
qq_plot.glm <- function(model, ...) {
  qq_plot.gam(prepare_lm_diagnostics(model), ...)
}

#' @export
#' @rdname qq_plot
#' @importFrom stats df.residual
qq_plot.lm <- function(model, ...) {
  qq_plot.gam(prepare_lm_diagnostics(model), ...)
}

#' @importFrom mgcv fix.family.rd
#' @importFrom stats weights
qq_simulate <- function(model, n = 50,
    type = c("deviance", "response", "pearson"), level = 0.9,
    detrend = FALSE, by_family = TRUE) {
  model <- model_used_rows(model)
  type <- match.arg(type)
  if (is_gfam_family(stats::family(model)))
    return(gfam_qq(model, n, type, "simulate", level, detrend, by_family))
  distribution <- response_distribution(model)
  check_distribution_reference(distribution)
  if (!is.function(distribution$rd)) stop("Response simulation is unavailable for this family.")
  qq_reference_draws(diagnostic_residuals(model, type),
    draw = function(p) distribution$rd(),
    residual = function(y) diagnostic_reference_residuals(model, y, type),
    n = n, method = "simulate", level = level, detrend = detrend)
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
    out <- gfam_qq(model, 1L, type, "normal", level, detrend, by_family)
    if (!isTRUE(response_distribution(model)$independent)) out[c("lower", "upper")] <- NULL
    return(out)
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
  if (!isTRUE(response_distribution(model)$independent)) out[c("lower", "upper")] <- NULL
  out
}

#' @importFrom mgcv fix.family.qf
#' @importFrom stats residuals fitted family weights na.action
qq_uniform <- function(model, n = 10,
    type = c("deviance", "response", "pearson"), level = 0.9,
    detrend = FALSE, by_family = TRUE) {
  model <- model_used_rows(model)
  type <- match.arg(type)
  if (is_gfam_family(stats::family(model)))
    return(gfam_qq(model, n, type, "uniform", level, detrend, by_family))
  distribution <- response_distribution(model)
  check_distribution_reference(distribution)
  if (!is.function(distribution$qf)) stop("An inverse CDF is unavailable for this family.")
  probabilities <- NULL
  draw <- function(p) {
    probabilities <<- sample(probabilities %||% p, length(p))
    distribution$qf(probabilities)
  }
  qq_reference_draws(diagnostic_residuals(model, type),
    draw = draw,
    residual = function(y) diagnostic_reference_residuals(model, y, type),
    n = n, method = "uniform", level = level, detrend = detrend)
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
#' @param model a fitted model supported by [diagnostic_support].
#' @inheritParams appraise
#' @param type character or `NULL`; residual type: `"deviance"`, `"pearson"`,
#'   `"response"`, `"pit"`, `"quantile"`, `"dharma"`, or `"surrogate"` (for
#'   `ocat()` or `multinom()` GAMs). Grouped models automatically use
#'   quantile residuals when all active components are supported, otherwise all
#'   use deviance residuals with a message. Ordinary models retain deviance
#'   residuals by default. See [appraise()] for distributional support.
#' @param xlab character or expression; the label for the x axis. If not
#'   supplied, a suitable label will be generated.
#' @param ylab character or expression; the label for the y axis. If not
#'   supplied, a suitable label will be generated.
#' @param title character or expression; the title for the plot. See
#'   [ggplot2::labs()].
#' @param subtitle character or expression; the subtitle for the plot. See
#'   [ggplot2::labs()].
#' @param caption character or expression; the plot caption. See
#'   [ggplot2::labs()].
#' @param point_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Constant point colour when components are not mapped to
#'   colour. Pooled grouped diagnostics instead colour points by component.
#' @param point_alpha Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Numeric; alpha transparency for points in plots.
#' @param line_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Colour specification for 1:1 line.
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
#' @param seed integer; random seed to use for PIT, quantile, surrogate or
#'   DHARMa-style residuals.
#' @param ... optional arguments passed to model preparation, such as the
#'   sdmTMB `field_method`. For grouped-family GAMs and joint GJRM models,
#'   `response = NULL` uses every response and an integer selects one; see
#'   [appraise()]. Selection leaves `by_family` independent.
#'
#' @details The default subtitle identifies the response family. Conditioning
#'   and field choices appear in the caption where relevant. Supply `caption`
#'   to replace this text, or `caption = ""` to omit it. The scalable version
#'   uses the same labels.
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
  rank = NULL,
  conditioning = NULL, ..., n_dharma = 250L
) {
  gratia_plot_arguments(c("point_col", "point_alpha", "line_col"), environment())
  model <- diagnostic_plot_source(model, type = type, seed = seed, conditioning = conditioning, n_dharma = if (missing(n_dharma)) NULL else n_dharma, ...)
  if (inherits(model, "gratia_gjrm_diagnostic")) {
    d <- diagnostic_data(model, type, seed, n_dharma = if (missing(n_dharma)) NULL else n_dharma)
    return(gjrm_diagnostic_point_plot(d, "predictor", point_col = point_col,
      point_alpha = point_alpha, line_col = line_col, xlab = xlab, ylab = ylab,
      title = title, subtitle = subtitle, caption = caption, rank = rank, by_family = by_family))
  }
  if (is_multinom_surrogate(model, type)) return(multinom_residual_plot(
    prepare_diagnostic_data(model, type, seed), "predictor", FALSE,
    rank = rank, xlab = xlab, ylab = ylab,
    title = if (missing(title)) NULL else title,
    subtitle = subtitle, caption = caption, point_col = point_col,
    point_alpha = point_alpha, line_col = line_col))
  original <- model
  model <- model_used_rows(model)
  type <- diagnostic_type(model, type, allow_pit = TRUE)
  by_family <- diagnostic_by_family(model, type, by_family)
  y_intercept <- 0
  if (type %in% c("pit", "quantile", "dharma", "surrogate")) {
    r <- diagnostic_residuals(model, type = type, seed = seed)
    if (type %in% c("pit", "dharma")) y_intercept <- 0.5
    if (is.null(ylab) && type %in% c("dharma", "surrogate"))
      ylab <- diagnostic_residual_label(type)
    if (is.null(ylab) && type == "pit") {
      ylab <- "PIT residuals"
      y_intercept <- 0.5
    }
  } else {
    r <- diagnostic_residuals(model, type = type)
    y_intercept <- 0
  }
  r <- restore_model_rows(r, original)
  mv_y <- is.matrix(response_distribution(model)$mean) ||
    family_name(model) %in% multivariate_y()
  if (is.matrix(r) && mv_y) { # handle mvn() like fits
    r <- as.vector(r)
  }
  response_mean <- response_distribution(model)$mean
  use_response_mean <- is.matrix(response_mean) &&
    !family_name(model) %in% multivariate_y()
  eta <- restore_model_rows(if (use_response_mean) response_mean else
    model[["linear.predictors"]], original)
  if (is.matrix(eta) && mv_y) {
    eta <- as.vector(eta) # handle multinom(), mvn() like fits
  }

  na_action <- na.action(model)
  if (is.matrix(eta) && !is.matrix(r)) {
    eta <- eta[, 1]
  }
  eta <- napredict(na_action, eta)

  df <- diagnostic_component_data(data.frame(eta = unname(eta), residuals = unname(r)), original)
  df <- diagnostic_predictor_data(df, original, by_family, rank)
  coloured <- is_gfam_family(family(model)) && !by_family
  plt <- gratia_plot(df, aes(
    x = .data$eta,
    y = .data$residuals
  )) +
    geom_gratia_diagnostic_reference_hline(yintercept = y_intercept, col = line_col)

  ## add point layer
  if (coloured) {
    plt <- plt + geom_gratia_diagnostic_point(aes(colour = .data$.family), alpha = point_alpha,
      data = function(d) d[stats::complete.cases(d), , drop = FALSE]) +
      gratia_default_scale(scale_colour_okabe_ito(limits = gfam_diagnostic_labels(original),
        drop = FALSE))
  } else {
    plt <- plt + geom_gratia_diagnostic_point(colour = point_col, alpha = point_alpha,
      data = function(d) d[stats::complete.cases(d), , drop = FALSE])
  }

  ## add labels
  if (is.null(xlab)) {
    xlab <- if (isTRUE(attr(df, "ranked"))) "Within-component linear predictor rank"
      else if (use_response_mean) "Fitted response mean"
      else original$eta_label %||% "Linear predictor"
  }
  if (is.null(ylab)) {
    ylab <- diagnostic_residual_label(type)
  }
  if (missing(title)) {
    title <- if (use_response_mean) "Residuals vs fitted response mean" else
      "Residuals vs linear predictor"
  }
  if (missing(subtitle)) {
    subtitle <- paste("Family:", family(model)[["family"]])
  }
  caption <- caption %||% diagnostic_predictor_caption(model)

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
  by_family = NULL,
  conditioning = NULL, ...
) {
  gratia_plot_arguments(c("point_col", "point_alpha"), environment())
  model <- diagnostic_plot_source(model, type = "response", conditioning = conditioning, ...)
  if (inherits(model, "gratia_gjrm_diagnostic")) {
    return(gjrm_diagnostic_point_plot(diagnostic_data(model, type = "response"),
      "observed", point_col = point_col, point_alpha = point_alpha,
      xlab = xlab, ylab = ylab, title = title, subtitle = subtitle, caption = caption, by_family = by_family))
  }
  original <- model
  model <- model_used_rows(model)
  check_gfam_appraise(model)
  by_family <- diagnostic_by_family(model, "response", by_family)
  fit <- response_distribution(model)$mean %||% fitted(model)
  fit <- restore_model_rows(fit, original)
  ## handle case where fitted is a matrix; extended.families
  ##   - the needs to be more involved as what about mvn or multinom families
  mv_y <- is.matrix(response_distribution(model)$mean) ||
    family_name(model) %in% multivariate_y()
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

  df <- data.frame(observed = unname(obs), fitted = unname(fit))

  ## base plot
  plt <- gratia_plot(df, aes(
    x = .data$fitted,
    y = .data$observed
  ))

  ## add point layer
  plt <- plt + geom_gratia_diagnostic_point(colour = point_col, alpha = point_alpha,
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
    subtitle <- diagnostic_reference_description(response_distribution(model),
      paste("Family:", family(model)[["family"]]))
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
#' @details Pooled grouped-family histograms use common bin edges and
#'   component-specific densities, with faint group-coloured fills and opaque
#'   stepped upper boundaries. Set `by_family = TRUE` for separate panels.
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
  by_family = NULL,
  conditioning = NULL, ..., n_dharma = 250L
) {
  model <- diagnostic_plot_source(model, type = type, seed = seed, conditioning = conditioning, n_dharma = if (missing(n_dharma)) NULL else n_dharma, ...)
  if (inherits(model, "gratia_gjrm_diagnostic")) {
    return(scalable_residuals_hist_plot(diagnostic_data(model, type, seed,
      n_dharma = if (missing(n_dharma)) NULL else n_dharma), n_bins = n_bins,
      ylab = ylab %||% "Frequency", xlab = xlab, title = title,
      subtitle = subtitle, caption = caption, by_family = by_family))
  }
  if (is_multinom_surrogate(model, type)) return(multinom_residual_plot(
    prepare_diagnostic_data(model, type, seed), "histogram", FALSE,
    n_bins = n_bins, xlab = xlab, ylab = ylab, title = title,
    subtitle = subtitle, caption = caption))
  original <- model
  model <- model_used_rows(model)
  type <- diagnostic_type(model, type, allow_pit = TRUE)
  by_family <- diagnostic_by_family(model, type, by_family)

  if (type %in% c("pit", "quantile", "dharma", "surrogate")) {
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
  plt <- gratia_plot(df, aes(x = .data$residuals))

  ## add point layer
  if (pooled) {
    valid <- df$residuals[is.finite(df$residuals)]
    # All components use one set of bin edges; density normalization makes
    # unequal component sizes comparable without silently changing weights.
    limits <- range(c(valid, 0))
    if (diff(limits) == 0) limits <- limits + c(-.5, .5)
    breaks <- seq(limits[1L], limits[2L], length.out = max(2L, n_bins + 1L))
    plt <- plt + geom_gratia_histogram(aes(y = ggplot2::after_stat(.data$density), fill = .data$.family),
      data = function(d) d[is.finite(d$residuals), , drop = FALSE],
      breaks = breaks, colour = NA, position = "identity", alpha = .15) +
      geom_gratia_histogram_step(
        aes(y = ggplot2::after_stat(.data$density), colour = .data$.family),
        data = function(d) d[is.finite(d$residuals), , drop = FALSE],
        stat = "bin", breaks = breaks, pad = TRUE, position = "identity",
        direction = "mid", linewidth = .6, alpha = 1) +
      gratia_default_scale(scale_fill_okabe_ito(limits = gfam_diagnostic_labels(original),
        drop = FALSE)) +
      gratia_default_scale(scale_colour_okabe_ito(limits = gfam_diagnostic_labels(original),
        drop = FALSE)) +
      labs(fill = "Family component", colour = "Family component")
  } else {
    plt <- plt + geom_gratia_histogram(
      data = function(d) d[!is.na(d$residuals), , drop = FALSE], bins = n_bins,
      colour = "white", fill = "grey20", center = 0)
  }

  ## add labels
  if (is.null(xlab)) {
    xlab <- diagnostic_residual_label(type)
  }
  if (is.null(ylab)) {
    ylab <- if (pooled) "Density" else "Frequency"
  }
  if (missing(title)) {
    title <- "Histogram of residuals"
  }
  if (missing(subtitle)) {
    subtitle <- diagnostic_reference_description(response_distribution(model),
      paste("Family:", family(model)[["family"]]))
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
worm_plot.default <- function(model, ...) {
  scalable_qq(model, ..., worm = TRUE, aggregate = FALSE)
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
#' ## The default uses inverse-CDF sampling of randomised uniform quantiles.
#' ## Note no reference bands are drawn with this method.
#' worm_plot(m)
#'
#' ## Alternatively simulate new data from the model, which
#' ## allows construction of reference intervals for the Q-Q plot
#' worm_plot(m,
#'   method = "simulate", point_col = "steelblue",
#'   point_alpha = 0.4
#' )
#'
#' ## ... or request a normal approximation for the residuals.
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
  conditioning = NULL, ..., n_dharma = 250L
) {
  gratia_plot_arguments(c("ci_col", "ci_alpha", "point_col", "point_alpha", "line_col"), environment())
  if (!is.null(type)) type <- diagnostic_type(model, type)
  if (is_multinom_surrogate(model, type)) return(scalable_qq(model,
    method = method, type = "surrogate", seed = seed, level = level,
    xlab = xlab, ylab = ylab, title = title, subtitle = subtitle,
    caption = caption, ci_col = ci_col, ci_alpha = ci_alpha,
    point_col = point_col, point_alpha = point_alpha, line_col = line_col,
    by_family = by_family, conditioning = conditioning, aggregate = FALSE,
    worm = TRUE, ...))
  if (identical(type, "dharma") ||
      (is.null(type) && identical(attr(model, "gratia.default_residual_type"), "dharma"))) {
    return(scalable_qq(model, method = method, type = "dharma", seed = seed,
      level = level, xlab = xlab, ylab = ylab, title = title,
      subtitle = subtitle, caption = caption, ci_col = ci_col, ci_alpha = ci_alpha,
      point_col = point_col, point_alpha = point_alpha, line_col = line_col,
      by_family = by_family, conditioning = conditioning,
      n_dharma = if (missing(n_dharma)) NULL else n_dharma,
      aggregate = FALSE, worm = TRUE, ...))
  }
  model <- diagnostic_plot_source(model, type, seed, conditioning, ...)

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
  if (type %in% c("pit", "quantile", "surrogate")) {
    return(calibrated_qq_plot(model, method, level, TRUE, by_family,
      ylab, xlab, title, subtitle, caption, ci_col, ci_alpha,
      point_col, point_alpha, line_col, type = type, n_simulate = n_simulate))
  }
  method <- diagnostic_reference_method(model, method)
  if (level <= 0 || level >= 1) {
    stop("Level must be 0 < level < 1. Supplied level <", level, ">",
      call. = FALSE
    )
  }



  ## generate theoretical quantiles
  df <- switch(method,
    marginal = qq_marginal(model, n = n_simulate, type = type, level = level,
      detrend = TRUE, by_family = by_family),
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
    subtitle <- diagnostic_qq_subtitle(method)
  }
  caption <- caption %||% diagnostic_qq_caption(model, df)

  ## base plot
  plt <- gratia_plot(df, aes(
    x = .data$theoretical,
    y = .data$residuals
  ))

  ## Now need a reference horizonta line
  plt <- plt + geom_gratia_diagnostic_reference_hline(yintercept = 0, col = line_col)

  ## add reference interval
  if (all(c("lower", "upper") %in% names(df))) {
    plt <- plt + geom_gratia_diagnostic_interval_ribbon(
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
  plt <- plt + geom_gratia_diagnostic_point(colour = point_col, alpha = point_alpha,
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
worm_plot.glm <- function(model, ...) {
  worm_plot.gam(prepare_lm_diagnostics(model), ...)
}

#' @rdname worm_plot
#' @importFrom stats df.residual
#' @export
worm_plot.lm <- function(model, ...) {
  worm_plot.gam(prepare_lm_diagnostics(model), ...)
}

# Extract diagnostic weights without changing the standard lm weights method.
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
