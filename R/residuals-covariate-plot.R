#' Plot residuals against covariates
#'
#' Inspect residuals from the complete fitted model against individual
#' covariates or pairs of numeric covariates.
#'
#' @param model a fitted model supported by [diagnostic_support], or prepared
#'   diagnostic data. The original model must retain its fitting observations.
#' @param covariates character vector of exact covariate names, in plotting
#'   order. `NULL` selects model covariates, excluding variables used only as
#'   offsets or random-effect groups. `character()` selects no individual
#'   panels, for use with `pairs`.
#' @param terms character vector of exact mgcv smooth labels, as returned by
#'   [smooths()]. For example, use `"te(x,z)"`, not `"te(x, z)"`. Parametric
#'   terms, partial matches and numeric or logical selectors are not accepted.
#'   Selected smooths supply their original covariates and `by` variables.
#'   Cannot be combined with a non-`NULL` `covariates`.
#' @param pairs list of character vectors, each naming two distinct numeric
#'   covariates. Adds a joint residual map for each pair, in the supplied order.
#' @param data original fitting data or retained fitting observations, including
#'   model predictors needed to verify alignment. Can contain additional
#'   covariates absent from the model. Rows are matched to stored observations
#'   and their evaluated predictors checked. Row counts alone do not establish
#'   alignment. `NULL` uses stored data, recovering raw covariates from the
#'   fitting call only when needed and verifying them in the same way.
#' @param by optional name of a categorical covariate defining separate facets.
#' @param smooth logical or `NULL`; add a descriptive penalised smooth of the
#'   mean residual to numeric panels. The default, `NULL`, fits a trend only
#'   for groups with at most 20,000 eligible observations. `TRUE` fits a trend
#'   regardless of group size; `FALSE` omits all trends. No confidence bands
#'   or tests are computed.
#' @param bins positive integer or pair of integers giving the numbers of cells
#'   along the two covariate axes of joint maps.
#' @param min_n positive integer; joint-map cells with fewer observations are
#'   blank. Counts for occupied cells are retained in the plotting data.
#' @param statistic joint-map summary: `"mean"` or sample standard deviation,
#'   `"sd"`. Cells containing one observation have undefined standard deviation.
#' @param ncol optional number of columns when composing several panels.
#' @inheritParams residuals_linpred_plot
#' @inheritParams scalable_qq_plot
#' @param point_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Colour for individual residual points or observation locations.
#' @param line_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Colour for reference lines and categorical group means.
#' @param n_dharma positive integer; number of simulations for DHARMa residuals.
#' @param ... arguments used to prepare model diagnostics, such as sdmTMB
#'   `component` and `field_method`. For grouped-family GAMs and joint GJRM
#'   models, `response = NULL` uses every response and an integer selects one;
#'   see [appraise()]. Selection leaves `by_family` independent.
#' @return A ggplot for one panel, or a patchwork composition for several.
#'   Scalable plots retain only summaries and evaluated trend curves.
#'
#' @details
#' Covariates are plotted in their original units. A term such as `s(log(x))`
#' selects `x`; stored transformed coordinates are never inverted to recover
#' original values. Supply `data` if those values are unavailable. Term
#' selection does not plot fitted term contributions or partial residuals. A
#' factor-specific smooth selects covariates without restricting observations;
#' use `by` to inspect patterns separately among groups.
#'
#' Pearson residuals are useful for checking the mean function because they
#' account for the specified mean--variance relationship. Response residuals
#' retain the response units. A remaining curve can suggest an incorrectly
#' specified mean function, including insufficient flexibility in a smooth.
#' Quantile and PIT residuals are useful for checking the conditional response
#' distribution, including changing spread and tails. Non-Gaussian deviance
#' residuals need not have zero conditional mean, so their mean curve need not
#' follow the zero reference line even under the fitted model.
#'
#' Numeric panels show points, or conditional-density heatmaps in the scalable
#' version. Categorical panels show boxplots and group means; the ordinary
#' version also shows points. Joint maps show means or standard deviations
#' within rectangular cells. The ordinary version also shows observation
#' locations. Mean maps use a diverging colour scale centred on zero, or 0.5
#' for PIT and DHARMa residuals. Standard-deviation maps use a sequential scale.
#' These are descriptive summaries, not tests against simulated reference maps.
#' An omitted interaction can produce a joint pattern even when both individual
#' covariate panels appear flat. Correlated covariates can share residual
#' patterns; use the plots to guide model revisions and subsequent checks.
#'
#' All eligible observations are used. Mean trends use a shrinkage cubic spline
#' with up to 40 basis functions, fitted with GCV below 10,000 observations and
#' discrete fREML via [mgcv::bam()] otherwise. The same calculation is used by
#' both plotting functions. Trends are omitted with fewer than four distinct
#' covariate values. Summaries and trends are calculated separately within each
#' response, family component and `by` group. Missing plotting values are
#' reported and omitted separately for each requested plot.
#'
#' @seealso [appraise()], [residuals_linpred_plot()], [partial_residuals()]
#' @export
#' @examples
#' library("mgcv")
#' dat <- data_sim("eg1", n = 400, seed = 2)
#' m <- gam(y ~ x2 + s(x0) + s(x1), data = dat, method = "REML")
#' # A curve against x2 suggests reconsidering its linear effect.
#' residuals_covariate_plot(m, covariates = "x2", type = "pearson")
#' # Check a covariate omitted from the model.
#' residuals_covariate_plot(m, covariates = "x3", data = dat)
#' # Inspect individual covariates and possible remaining interaction structure.
#' scalable_residuals_covariate_plot(m, covariates = c("x0", "x2"),
#'   pairs = list(c("x0", "x2")), bins = 6, type = "quantile", seed = 42)
residuals_covariate_plot <- function(model, covariates = NULL, terms = NULL,
    pairs = NULL, data = NULL, by = NULL, type = NULL, smooth = NULL,
    bins = c(20L, 20L), min_n = 5L, statistic = c("mean", "sd"),
    seed = NULL, conditioning = NULL, n_dharma = 250L,
    point_col = "grey20", point_alpha = 0.5, line_col = "red",
    xlab = NULL, ylab = NULL, title = NULL, subtitle = NULL, caption = NULL,
    ncol = NULL, ...) {
  gratia_plot_arguments(c("point_col", "point_alpha", "line_col"), environment())
  residual_covariate_plots(model, covariates, terms, pairs, data, by, type,
    smooth, bins, min_n, match.arg(statistic), seed, conditioning,
    if (missing(n_dharma)) NULL else n_dharma, FALSE,
    point_col = point_col, point_alpha = point_alpha, line_col = line_col,
    xlab = xlab, ylab = ylab, title = title, subtitle = subtitle,
    caption = caption, ncol = ncol, ...)
}

#' @rdname residuals_covariate_plot
#' @export
scalable_residuals_covariate_plot <- function(model, covariates = NULL,
    terms = NULL, pairs = NULL, data = NULL, by = NULL, type = NULL,
    smooth = NULL, bins = c(20L, 20L), min_n = 5L,
    statistic = c("mean", "sd"), seed = NULL, conditioning = NULL,
    n_dharma = 250L, density_bins = c(50L, 50L),
    density = c("conditional", "count"), density_method = c("kde", "histogram"),
    bandwidth = NULL, line_col = "red", xlab = NULL, ylab = NULL,
    title = NULL, subtitle = NULL, caption = NULL, ncol = NULL, ...) {
  gratia_plot_arguments(c("line_col"), environment())
  residual_covariate_plots(model, covariates, terms, pairs, data, by, type,
    smooth, bins, min_n, match.arg(statistic), seed, conditioning,
    if (missing(n_dharma)) NULL else n_dharma, TRUE,
    density_bins = density_bins, density = match.arg(density),
    density_method = match.arg(density_method), bandwidth = bandwidth,
    line_col = line_col, xlab = xlab, ylab = ylab, title = title,
    subtitle = subtitle, caption = caption, ncol = ncol, ...)
}

residual_covariate_names <- function(x, name, empty = TRUE) {
  if (!is.character(x) || anyNA(x) || any(!nzchar(x)) ||
      (!empty && !length(x))) {
    stop("'", name, "' must be a character vector of exact names.", call. = FALSE)
  }
  unique(x)
}

# Work with the original fitting frame, not a proxy's synthetic response frame.
residual_covariate_context <- function(model) {
  source <- model
  model <- diagnostic_original(model)
  if (inherits(model, "sdmTMB")) {
    ff <- model$formula[[1L]]
    frame <- if (inherits(source, "gratia_diagnostic_proxy")) source$model else model$data
    ctx <- list(model = model, data = frame, formula = ff,
      terms = stats::terms(ff), envir = environment(ff), call = model$call)
  } else if (inherits(model, "merMod")) {
    ff <- stats::formula(model, fixed.only = TRUE)
    ctx <- list(model = model, data = stats::model.frame(model), formula = ff,
      terms = stats::terms(ff), envir = environment(ff), call = stats::getCall(model))
  } else if (inherits(model, "lme")) {
    ff <- stats::formula(model)
    ctx <- list(model = model, data = lme_fitting_data(model), formula = ff,
      terms = stats::terms(ff), envir = environment(ff), call = model$call)
  } else ctx <- model_context(model)
  if (inherits(model, c("gamlss", "gjrm"))) {
    # Embedded GAMs contain working responses. Verify the final observed
    # response and the predictors from every equation instead.
    components <- gjrm_gamlss_components(model)
    frame <- ctx$data[, FALSE, drop = FALSE]
    for (component in components) {
      predictors <- component$model
      if (attr(component$terms, "response") > 0L) predictors <- predictors[, -1L, drop = FALSE]
      index <- match(rownames(frame), rownames(predictors))
      if (anyNA(index)) stop("GJRM equation rows are not aligned.", call. = FALSE)
      for (v in setdiff(names(predictors), names(frame))) frame[[v]] <- predictors[[v]][index]
    }
    response <- paste(deparse(model$formula[[1L]][[2L]], width.cutoff = 500L), collapse = "")
    frame[[response]] <- as.vector(model$y1)
    if (inherits(model, "gjrm") && !isTRUE(model$univar.gamlss)) {
      for (i in seq_along(model$margins)) {
        response <- paste(deparse(model$formula[[i]][[2L]], width.cutoff = 500L), collapse = "")
        frame[[response]] <- as.vector(model[[paste0("y", i)]])
      }
    }
    ctx$data <- frame
    ctx$verification_labels <- names(frame)
  }
  structural <- ctx$model
  labels <- if (inherits(model, c("merMod", "lme"))) character() else smooths(model)
  sms <- if (inherits(model, c("gamlss", "gjrm"))) {
    unlist(lapply(gjrm_gamlss_components(model), `[[`, "smooth"), recursive = FALSE)
  } else if (length(labels)) get_smooths_by_id(model, seq_along(labels)) else list()
  list(model = model, context = ctx, structural = structural,
    labels = labels, smooths = sms)
}

residual_smooth_covariates <- function(sm) {
  expressions <- c(sm$term, if (!is.null(sm$by) && sm$by != "NA") sm$by)
  unique(unlist(lapply(expressions, function(x) all.vars(term_expression(x)))))
}

residual_random_inputs <- function(expr) {
  if (is.call(expr)) {
    if (as.character(expr[[1L]])[1L] %in% c("|", "||")) {
      return(all.vars(expr[[2L]]))
    }
    return(unique(unlist(lapply(as.list(expr)[-1L], residual_random_inputs))))
  }
  if (is.list(expr)) return(unique(unlist(lapply(expr, residual_random_inputs))))
  character()
}

residual_default_covariates <- function(info) {
  m <- info$structural
  pterms <- if (inherits(info$model, c("gamlss", "gjrm"))) {
    lapply(gjrm_gamlss_components(info$model), `[[`, "pterms")
  } else if (inherits(info$model, "glmmTMB")) {
    lapply(glmmtmb_components(info$model), function(component)
      stats::terms(info$model, component = component))
  } else if (inherits(info$model, "sdmTMB")) {
    lapply(info$model$formula, stats::terms)
  } else if (is.list(m)) m$pterms else NULL
  tt <- pterms %||% info$context$terms
  if (inherits(tt, "terms")) tt <- list(tt)
  labels <- unlist(lapply(tt, function(t) attr(t, "term.labels")))
  # Offset-only variables and native random-effect groups are not default axes.
  labels <- labels[!grepl("offset\\(|\\|", labels)]
  variables <- unique(unlist(lapply(labels, function(x)
    all.vars(term_expression(x, if (is.list(m)) m else NULL)))))
  fixed <- Filter(function(sm) !inherits(sm, "random.effect"), info$smooths)
  random <- Filter(function(sm) inherits(sm, "random.effect"), info$smooths)
  smvars <- unique(unlist(lapply(fixed, residual_smooth_covariates)))
  revars <- unique(unlist(lapply(random, residual_smooth_covariates)))
  # A random-slope covariate is still a useful diagnostic axis. Exclude only
  # categorical grouping inputs, retaining numeric inputs and by variables.
  numeric_re <- vapply(revars, function(v) {
    value <- info$context$data[[v]]
    if (is.null(value) && is.list(m)) value <- m$var.summary[[v]]
    is.numeric(value)
  }, logical(1))
  random_by <- unique(unlist(lapply(random, function(sm) {
    if (!is.null(sm$by) && sm$by != "NA") all.vars(term_expression(sm$by))
  })))
  smvars <- unique(c(smvars, revars[numeric_re], random_by))
  if (is.null(pterms)) {
    variables <- setdiff(variables, setdiff(revars, smvars))
  }
  ff <- if (inherits(info$model, "glmmTMB")) {
    lapply(glmmtmb_components(info$model), function(component)
      stats::formula(info$model, component = component))
  } else if (inherits(info$model, "sdmTMB")) {
    info$model$formula
  } else if (inherits(info$model, "merMod")) {
    stats::formula(info$model)
  } else info$context$formula
  native <- if (inherits(info$model, "lme")) {
    unique(unlist(lapply(stats::formula(info$model$modelStruct$reStruct), all.vars)))
  } else residual_random_inputs(ff)
  # GAM working fits can store matrix-valued random-effect design columns.
  native <- native[!vapply(native, function(v)
    is.matrix(info$context$data[[v]]), logical(1))]
  as.character(unique(c(variables, smvars, native)))
}

# Verify evaluated fitting coordinates before attaching residuals. Matching row
# names handles subset/NA exclusions; a checked positional match also accepts a
# retained frame whose row names have been reset, as happens with tibbles.
residual_covariate_frame <- function(info, variables, data = NULL) {
  ctx <- info$context
  stored <- as.data.frame(ctx$data)
  if (is.null(data) && all(variables %in% names(stored))) return(stored)
  automatic <- is.null(data)
  if (automatic) {
    data <- tryCatch(eval(ctx$call$data, envir = ctx$envir), error = function(e) NULL)
  }
  if (!is.data.frame(data) || !all(variables %in% names(data))) {
    missing <- setdiff(variables, names(data))
    stop("Cannot recover covariates", if (length(missing))
      paste0(": ", paste(missing, collapse = ", ")), ". Supply original fitting 'data'.",
      call. = FALSE)
  }
  data <- as.data.frame(data)
  exprs <- as.list(attr(stats::delete.response(ctx$terms), "variables"))[-1L]
  labels <- vapply(exprs, function(x) paste(deparse(x, width.cutoff = 500L),
    collapse = ""), character(1))
  labels <- intersect(unique(c(labels, ctx$verification_labels)), names(stored))
  # Direct response columns also help distinguish otherwise identical predictors.
  labels <- unique(c(labels, intersect(names(stored), names(data))))
  labels <- labels[!startsWith(labels, "(")]
  if (!length(labels)) stop("Cannot verify fitting observations in 'data'.", call. = FALSE)
  verify <- function(index) {
    if (length(index) != nrow(stored) || anyNA(index)) return(NULL)
    candidate <- data[index, , drop = FALSE]
    evaluated <- tryCatch(evaluate_terms(candidate, labels, ctx$model,
      envir = ctx$envir), error = function(e) NULL)
    if (is.null(evaluated)) return(NULL)
    equal <- vapply(labels, function(v) isTRUE(all.equal(
      unname(evaluated[[v]]), unname(stored[[v]]), check.attributes = FALSE)), logical(1))
    if (all(equal)) candidate else NULL
  }
  aligned <- verify(match(rownames(stored), rownames(data)))
  if (is.null(aligned) && nrow(data) == nrow(stored)) {
    aligned <- verify(seq_len(nrow(stored)))
  }
  if (is.null(aligned)) stop(
    "Covariates in 'data' do not agree with the stored fitting observations. ",
    "Supply original fitting data or retained observations in fitting order.",
    call. = FALSE)
  aligned
}

residual_covariate_data <- function(model, covariates, terms, pairs, data, by,
    type, seed, conditioning, n_dharma, ...) {
  d <- prepare_diagnostic_data(model, type, seed, conditioning = conditioning,
    n_dharma = n_dharma, ...)
  info <- residual_covariate_context(d$source)
  if (!is.null(terms)) {
    if (!is.null(covariates)) stop("Supply only one of 'covariates' and 'terms'.", call. = FALSE)
    if (!is.character(terms) || !length(terms) || anyNA(terms) ||
        any(!nzchar(terms))) stop(
      "'terms' must contain exact character smooth labels from smooths(model).",
      call. = FALSE)
    terms <- unique(terms)
    missing <- setdiff(terms, info$labels)
    if (length(missing)) stop("Unmatched smooth label(s): ",
      paste(missing, collapse = ", "), ". Use exact labels from smooths(model).",
      call. = FALSE)
    covariates <- unique(unlist(lapply(info$smooths[match(terms, info$labels)],
      residual_smooth_covariates)))
  }
  if (is.null(covariates)) covariates <- residual_default_covariates(info)
  covariates <- residual_covariate_names(covariates, "covariates")
  if (!is.null(pairs) && (!is.list(pairs) || is.data.frame(pairs))) {
    stop("'pairs' must be a list of pairs of numeric covariate names.", call. = FALSE)
  }
  pairs <- lapply(pairs, function(x) {
    residual_covariate_names(x, "pairs", empty = FALSE)
    if (length(x) != 2L || anyDuplicated(x)) stop("Each 'pairs' entry must name two distinct covariates.", call. = FALSE)
    x
  })
  if (!is.null(by) && (length(by) != 1L ||
      length(residual_covariate_names(by, "by", empty = FALSE)) != 1L)) {
    stop("'by' must name one categorical covariate.", call. = FALSE)
  }
  if (!length(covariates) && !length(pairs)) stop("No covariates or pairs selected.", call. = FALSE)
  variables <- unique(c(covariates, unlist(pairs), by))
  frame <- residual_covariate_frame(info, variables, data)
  # .row repeats for multivariate responses; each response gets its own facet.
  rows <- sort(unique(d$data$.row))
  if (length(rows) != nrow(frame)) stop("Fitting covariates and diagnostic rows are not aligned.", call. = FALSE)
  index <- match(d$data$.row, rows)
  out <- frame[index, variables, drop = FALSE]
  for (v in variables) {
    if (is.matrix(out[[v]]) || !(is.numeric(out[[v]]) || is.factor(out[[v]]) ||
        is.character(out[[v]]) || is.logical(out[[v]]))) {
      stop("Covariate '", v, "' must be a numeric or categorical vector.", call. = FALSE)
    }
  }
  for (pair in pairs) if (!all(vapply(out[pair], is.numeric, logical(1)))) {
    stop("Joint maps require numeric covariates. Use 'by' for categorical groups.", call. = FALSE)
  }
  if (!is.null(by) && is.numeric(out[[by]])) stop("'by' must be categorical.", call. = FALSE)
  groups <- list()
  if (length(unique(d$data$.response)) > 1L) {
    groups$Response <- paste("Response", d$data$.response)
  }
  if (".family" %in% names(d$data)) groups$Family <- d$data$.family
  if (!is.null(by)) groups[[by]] <- ifelse(is.na(out[[by]]),
    NA_character_, paste(by, "=", out[[by]]))
  group <- if (length(groups)) do.call(interaction,
    c(lapply(groups, factor), list(drop = TRUE, lex.order = TRUE, sep = "; "))) else
    factor(rep("All observations", nrow(out)))
  list(covariates = covariates, pairs = pairs, values = out,
    residuals = d$data$residuals, row = d$data$.row, group = group, type = d$type,
    subtitle = diagnostic_reference_description(response_distribution(d$source),
      paste("Family:", d$family)))
}

residual_covariate_trend <- function(dat, smooth = NULL) {
  out <- data.frame(x = numeric(), residuals = numeric())
  if (isFALSE(smooth) || (is.null(smooth) && nrow(dat) > 20000L)) return(out)
  n_unique <- length(unique(dat$x))
  if (n_unique < 4L || nrow(dat) < 5L) return(out)
  if (diff(range(dat$residuals)) <= sqrt(.Machine$double.eps) *
      max(1, abs(dat$residuals))) {
    return(data.frame(x = seq(min(dat$x), max(dat$x), length.out = 200L),
      residuals = mean(dat$residuals)))
  }
  k <- min(40L, n_unique - 1L)
  # Resolve s() in the formula environment without attaching mgcv.
  s <- mgcv::s
  formula <- residuals ~ s(x, bs = "cs", k = k)
  fit <- if (nrow(dat) >= 10000L) {
    mgcv::bam(formula, data = dat, method = "fREML", discrete = TRUE,
      gamma = 1.4, nthreads = 1L)
  } else mgcv::gam(formula, data = dat, method = "GCV.Cp", gamma = 1.4)
  out <- data.frame(x = seq(min(dat$x), max(dat$x), length.out = 200L))
  out$residuals <- as.numeric(stats::predict(fit, newdata = out))
  out
}

residual_covariate_boxes <- function(dat) {
  dplyr::bind_rows(lapply(split(dat, dat$x, drop = TRUE), function(z) {
    q <- stats::quantile(z$residuals, c(.25, .5, .75), names = FALSE)
    inside <- z$residuals[z$residuals >= q[1L] - 1.5 * (q[3L] - q[1L]) &
      z$residuals <= q[3L] + 1.5 * (q[3L] - q[1L])]
    data.frame(x = z$x[1L], ymin = min(inside), lower = q[1L], middle = q[2L],
      upper = q[3L], ymax = max(inside), mean = mean(z$residuals), count = nrow(z))
  }))
}

residual_covariate_surface <- function(dat, bins, min_n, statistic) {
  breaks <- function(x, n) {
    r <- range(x)
    if (diff(r) == 0) r <- r + c(-.5, .5) * max(1, abs(r[1L]) * .01)
    seq(r[1L], r[2L], length.out = n + 1L)
  }
  xb <- breaks(dat$x, bins[1L]); yb <- breaks(dat$y, bins[2L])
  xi <- findInterval(dat$x, xb, all.inside = TRUE)
  yi <- findInterval(dat$y, yb, all.inside = TRUE)
  id <- xi + (yi - 1L) * bins[1L]
  parts <- split(dat$residuals, id)
  cells <- as.integer(names(parts))
  ix <- (cells - 1L) %% bins[1L] + 1L
  iy <- (cells - 1L) %/% bins[1L] + 1L
  count <- lengths(parts)
  value <- vapply(parts, if (statistic == "mean") mean else stats::sd, numeric(1))
  value[count < min_n] <- NA_real_
  data.frame(xmin = xb[ix], xmax = xb[ix + 1L], ymin = yb[iy],
    ymax = yb[iy + 1L], count = unname(count), value = unname(value))
}

residual_covariate_groups <- function(dat, fun) {
  dplyr::bind_rows(lapply(split(dat, dat$.panel, drop = TRUE), function(z) {
    ans <- fun(z)
    ans$.panel <- rep(z$.panel[1L], nrow(ans))
    ans
  }))
}

residual_covariate_plots <- function(model, covariates, terms, pairs, data, by,
    type, smooth, bins, min_n, statistic, seed, conditioning, n_dharma,
    scalable, density_bins = c(50L, 50L), density = "conditional",
    density_method = "kde", bandwidth = NULL, point_col = "grey20",
    point_alpha = .5, line_col = "red", xlab = NULL, ylab = NULL,
    title = NULL, subtitle = NULL, caption = NULL, ncol = NULL, ...) {
  if (is_multinom_surrogate(model, type)) {
    nominal <- prepare_diagnostic_data(model, type, seed,
      conditioning = conditioning, n_dharma = n_dharma, ...)
    plots <- lapply(seq_len(ncol(nominal$residuals)), function(j) {
      component <- multinom_diagnostic_component(nominal, j)
      component$data$.family <- factor(rep(paste("Category", colnames(nominal$residuals)[j]), nominal$n_obs))
      residual_covariate_plots(component, covariates, terms, pairs, data, by,
        type, smooth, bins, min_n, statistic, NULL, NULL, n_dharma,
        scalable, density_bins, density, density_method, bandwidth,
        point_col, point_alpha, line_col, xlab, ylab, title, subtitle,
        caption, ncol)
    })
    return(patchwork::wrap_plots(plots, ncol = 1L, guides = "keep"))
  }
  if (!is.null(smooth) &&
      (!is.logical(smooth) || length(smooth) != 1L || is.na(smooth))) {
    stop("'smooth' must be NULL, TRUE or FALSE.", call. = FALSE)
  }
  bins <- rep(validate_diagnostic_bins(bins, "bins", length = c(1L, 2L)), length.out = 2L)
  min_n <- validate_diagnostic_bins(min_n, "min_n")
  validate_diagnostic_bins(density_bins, "density_bins", length = c(1L, 2L))
  validate_diagnostic_bandwidth(bandwidth)
  d <- residual_covariate_data(model, covariates, terms, pairs, data, by, type,
    seed, conditioning, n_dharma, ...)
  reference <- if (d$type %in% c("pit", "dharma")) .5 else 0
  requests <- c(lapply(d$covariates, c), d$pairs)
  plots <- lapply(requests, function(vars) {
    dat <- data.frame(x = d$values[[vars[1L]]], residuals = d$residuals,
      .panel = d$group, .row = d$row)
    joint <- length(vars) == 2L
    if (joint) dat$y <- d$values[[vars[2L]]]
    categorical <- !is.numeric(dat$x)
    if (categorical) dat$x <- factor(dat$x, levels = if (is.factor(dat$x))
      levels(dat$x) else unique(dat$x))
    keep <- !is.na(dat$x) & !is.na(dat$.panel) & is.finite(dat$residuals)
    if (!categorical) keep <- keep & is.finite(dat$x)
    if (joint) keep <- keep & is.finite(dat$y)
    if (any(!keep)) warning("Dropped ", sum(!keep),
      " non-finite or missing observation(s) for ", paste(vars, collapse = ", "),
      ".", call. = FALSE)
    dat <- dat[keep, , drop = FALSE]
    if (!nrow(dat)) stop("No finite observations for ", paste(vars, collapse = ", "), ".", call. = FALSE)
    if (joint) {
      summary <- residual_covariate_groups(dat, function(z)
        residual_covariate_surface(z, bins, min_n, statistic))
      p <- gratia_plot(summary, ggplot2::aes(xmin = .data$xmin,
        xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax, fill = .data$value)) +
        geom_gratia_surface_rect(colour = NA)
      label <- if (statistic == "mean") "Mean residual" else "Residual SD"
      p <- if (statistic == "mean") p + gratia_default_scale(ggplot2::scale_fill_gradient2(
        name = label, midpoint = reference, low = "#2166AC", mid = "white",
        high = "#B2182B", na.value = "transparent")) else
        p + gratia_default_scale(ggplot2::scale_fill_viridis_c(name = label, na.value = "transparent"))
      if (!scalable) p <- p + geom_gratia_residual_covariate_point(data = dat,
        ggplot2::aes(x = .data$x, y = .data$y), inherit.aes = FALSE,
        colour = point_col, alpha = point_alpha, size = .5)
    } else if (categorical) {
      summary <- residual_covariate_groups(dat, residual_covariate_boxes)
      p <- gratia_plot(summary, ggplot2::aes(x = .data$x)) +
        geom_gratia_diagnostic_reference_hline(yintercept = reference, colour = line_col) +
        geom_gratia_diagnostic_boxplot(ggplot2::aes(ymin = .data$ymin, lower = .data$lower,
          middle = .data$middle, upper = .data$upper, ymax = .data$ymax), stat = "identity")
      if (!scalable) p <- p + geom_gratia_residual_covariate_point(data = dat,
        ggplot2::aes(y = .data$residuals), colour = point_col, alpha = point_alpha)
      p <- p + geom_gratia_residual_covariate_point(ggplot2::aes(y = .data$mean),
        shape = 18L, colour = line_col, size = 2)
    } else {
      trend <- if (!isFALSE(smooth)) residual_covariate_groups(dat,
        function(z) residual_covariate_trend(z, smooth)) else NULL
      if (scalable) {
        summary <- residual_covariate_groups(dat, function(z) {
          if (density == "conditional" && density_method == "kde") {
            diagnostic_density_grid(z$x, z$residuals, density_bins, bandwidth)
          } else diagnostic_grid(z$x, z$residuals, density_bins, density)
        })
        p <- render_diagnostic_grid(summary, density, NULL, NULL, NULL, NULL,
          NULL, intercept = reference, line_col = line_col,
          fill_transform = if (density == "conditional" && density_method == "kde") "sqrt" else "identity")
      } else {
        p <- gratia_plot(dat, ggplot2::aes(x = .data$x, y = .data$residuals)) +
          geom_gratia_diagnostic_reference_hline(yintercept = reference, colour = line_col) +
          geom_gratia_residual_covariate_point(colour = point_col, alpha = point_alpha)
      }
      if (!is.null(trend) && nrow(trend)) p <- p + geom_gratia_trend_line(data = trend,
        ggplot2::aes(x = .data$x, y = .data$residuals), inherit.aes = FALSE,
        colour = "#0072B2", linewidth = .7)
    }
    if (nlevels(droplevels(dat$.panel)) > 1L || !is.null(by)) {
      p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$.panel))
    }
    p <- p + ggplot2::labs(x = xlab %||% vars[1L],
      y = ylab %||% if (joint) vars[2L] else diagnostic_residual_label(d$type),
      title = title %||% paste("Residuals vs", paste(vars, collapse = " and ")),
      subtitle = subtitle %||% d$subtitle, caption = caption)
    compact_diagnostic_plot(p)
  })
  if (length(plots) == 1L) plots[[1L]] else patchwork::wrap_plots(plots, ncol = ncol, guides = "keep")
}
