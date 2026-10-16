# Each model supplies its fitted distribution and a function for calculating
# residuals. QQ calculations and plots use these inputs for every fitting package.
prepare_lm_diagnostics <- function(model) {
  if (!is.null(model$distribution)) return(model)
  action <- stats::na.action(model)
  model <- model_used_rows(model)
  model$sig2 <- if (inherits(model, "glm")) summary(model)$dispersion else
    stats::deviance(model) / stats::df.residual(model)
  if (is.null(model$prior.weights)) model$prior.weights <- diagnostic_weights(model)
  if (is.null(model$linear.predictors)) model$linear.predictors <- model$fitted.values
  if (is.null(model$y)) model$y <- stats::model.response(stats::model.frame(model))
  model$na.action <- action
  model
}

# Fitting weights can describe repeated observations rather than binomial
# trials or inverse response variance. Use the response format and the package's
# likelihood to choose weights for the response distribution.
diagnostic_sampling_weights <- function(model, weights) {
  f <- stats::family(model)
  ft <- family_type(f)
  response <- stats::model.response(stats::model.frame(model))
  if (ft == "binomial" && is.factor(response)) return(rep(1, NROW(response)))
  # gaulss() ignores prior weights in its likelihood, although its rd function
  # divides the variance by those weights. Use unit weights to match the fit;
  # callers can still request different weights explicitly when sampling.
  if (ft == "gaulss" && native_mgcv_hook(f$ll, mgcv::gaulss()$ll))
    return(rep(1, NROW(response)))
  weights
}

# Keep the existing internal name for callers that prepare binomial trials.
binomial_sampling_weights <- diagnostic_sampling_weights

diagnostic_reference_residuals <- function(model, y, type) {
  UseMethod("diagnostic_reference_residuals")
}

#' @export
diagnostic_reference_residuals.default <- function(model, y, type) {
  model <- model_used_rows(model)
  distribution <- response_distribution(model)
  y <- observe_diagnostic_response(distribution, y)
  check_distribution_residual_moments(distribution, type)
  if (is.function(distribution$diagnostic_residuals))
    return(distribution$diagnostic_residuals(y, type))
  fam <- stats::family(model)
  if (is_gfam_family(fam)) {
    return(gfam_diagnostic_residuals(gfam_diagnostic_context(model), y, type))
  }
  hook <- fam$residuals %||% fam$dev.resids
  compute_residuals(y, stats::fitted(model), diagnostic_weights(model),
    type = type, dev_resid_fun = hook, var_fun = fam$variance,
    na_action = NULL, model = model)
}

# Check which reference calculations the fitted distribution supports.
# A missing CDF does not establish that its residuals follow a normal law.
diagnostic_reference_method <- function(model, method = NULL) {
  if (!is.null(method)) method <- match.arg(method, c("uniform", "simulate", "normal", "direct", "marginal"))
  distribution <- response_distribution(model)
  if (!identical(method, "normal")) check_distribution_reference(distribution)
  joint <- isTRUE(distribution$joint_available) && is.function(distribution$rd)
  dependent <- !isTRUE(distribution$independent)
  if (dependent && !joint) {
    if (identical(method, "simulate")) stop("Joint response references are unavailable: ",
      distribution$dependence_reason %||% "no valid joint response generator", ". ",
      "Use calibrated PIT/quantile residuals or method = 'marginal' for distribution checks.",
      call. = FALSE)
    if (identical(method, "normal")) return("normal")
    if (is.function(distribution$marginal_rd) || is.function(distribution$qf))
      return("marginal")
    stop("Marginal reference sampling is unavailable. ",
      distribution$dependence_reason %||% "", call. = FALSE)
  }
  if (identical(method, "marginal")) return("marginal")
  if (is_gfam_family(stats::family(model))) return(gfam_diagnostic_method(model, method))
  method <- diagnostic_method(model, method)
  if (method == "uniform" && dependent) {
    message("Using joint simulated reference quantiles for dependent responses.")
    method <- "simulate"
  }
  if (method == "uniform" && !is.function(distribution$qf)) {
    message("Using simulated reference quantiles: an inverse CDF is unavailable.")
    method <- "simulate"
  }
  if (method == "simulate" && !joint) {
    message("Using a normal approximation: a response generator is unavailable.")
    method <- "normal"
  }
  method
}

# Estimate the pooled marginal QQ curve from each observation's distribution.
# These draws describe the mixture of those distributions; they do not establish
# dependence between responses, so they cannot supply joint reference bands.
qq_marginal <- function(model, n = 50L, type = "deviance", level = .9,
                        detrend = FALSE, by_family = TRUE) {
  model <- model_used_rows(model)
  distribution <- response_distribution(model)
  draw <- distribution$marginal_rd
  if (!is.function(draw) && is.function(distribution$qf))
    draw <- function() distribution$qf(stats::runif(NROW(model$y)))
  if (!is.function(draw)) stop("No marginal response reference generator.", call. = FALSE)
  observed <- as.vector(diagnostic_residuals(model, type))
  grouped <- is_gfam_family(stats::family(model))
  index <- if (grouped && by_family) gfam_diagnostic_context(model)$index else
    rep(1L, length(observed))
  result <- dplyr::bind_rows(lapply(sort(unique(index)), function(i) {
    rows <- which(index == i)
    out <- qq_reference_draws(observed[rows], draw = function(p) draw(),
      residual = function(y) as.vector(diagnostic_reference_residuals(model, y, type))[rows],
      n = n, method = "simulate", level = level, detrend = detrend)
    out[c("lower", "upper")] <- NULL
    if (grouped && by_family) out$.family <- factor(rep(gfam_diagnostic_labels(model)[i],
      nrow(out)), levels = gfam_diagnostic_labels(model))
    out
  }))
  attr(result, "band_method") <- "none"
  result
}

diagnostic_reference_description <- function(distribution, base) {
  if (is.null(distribution)) return(base)
  describe <- !isTRUE(distribution$independent) ||
    !is.null(distribution$dependence_reason) || !is.null(distribution$approximation) ||
    !identical(distribution$conditioning, "conditional")
  if (describe) {
    base <- paste0(base, "; conditioning: ", distribution$conditioning %||% "specified")
    if (!is.null(distribution$approximation)) base <- paste(base,
      distribution$approximation, sep = "\n")
    base <- paste(vapply(strsplit(base, "\n", fixed = TRUE)[[1L]],
      stringr::str_wrap, character(1), width = 50L), collapse = "\n")
  }
  base
}

diagnostic_qq_subtitle <- function(method) {
  paste("Method:", if (method == "normal") "std. normal" else method)
}

diagnostic_caption <- function(context) {
  if (!length(context)) return(NULL)
  lines <- context[1L]
  for (item in context[-1L]) {
    last <- length(lines)
    if (nchar(lines[last]) + 2L + nchar(item) > 50L) {
      lines <- c(lines, item)
    } else {
      lines[last] <- paste(lines[last], item, sep = "; ")
    }
  }
  paste(lines, collapse = "\n")
}

diagnostic_qq_caption <- function(model, data, bands = attr(data, "band_method")) {
  distribution <- response_distribution(model)
  independent <- isTRUE(distribution$independent)
  if (is.null(bands) && !independent) {
    bands <- if (all(c("lower", "upper") %in% names(data))) "joint" else "none"
  }
  context <- c(
    if (inherits(model, c("gratia_diagnostic_proxy", "gratia_glmmtmb_diagnostic")) ||
        !identical(distribution$conditioning, "conditional"))
      paste("Conditioning:", distribution$conditioning %||% "conditional"),
    if (!is.null(distribution$field_method) &&
        !identical(distribution$conditioning, "marginal"))
      paste("Fields:", distribution$field_method),
    switch(bands %||% "", joint = "joint simulation bands", none = "bands omitted")
  )
  diagnostic_caption(context)
}

diagnostic_predictor_caption <- function(model) {
  distribution <- response_distribution(model)
  describe <- inherits(model, c("gratia_diagnostic_proxy", "gratia_glmmtmb_diagnostic")) ||
    !isTRUE(distribution$independent) || !is.null(distribution$dependence_reason) ||
    !is.null(distribution$approximation) ||
    !identical(distribution$conditioning, "conditional")
  diagnostic_caption(c(
    if (describe) paste("Conditioning:", distribution$conditioning %||% "conditional"),
    if (!is.null(distribution$field_method) &&
        !identical(distribution$conditioning, "marginal"))
      paste("Fields:", distribution$field_method)
  ))
}

# Average the sorted quantiles for uniform references. For simulated references,
# keep the existing R type-7 quantiles of all draws and intervals at each rank.
qq_reference_draws <- function(observed, draw, residual, n, method,
                               level = 0.9, detrend = FALSE,
                               max_cells = getOption("gratia.diagnostics.max_cells", 1e7)) {
  n <- validate_diagnostic_bins(n, "reference samples")
  r <- sort(as.vector(observed))
  nr <- length(r)
  if (!nr) stop("No observations available for QQ references.", call. = FALSE)
  if (!is.numeric(max_cells) || length(max_cells) != 1L ||
      !is.finite(max_cells) || max_cells < 1) {
    stop("'gratia.diagnostics.max_cells' must be a positive number.", call. = FALSE)
  }
  probabilities <- (seq_len(nr) - 0.5) / nr
  next_sample <- function() {
    value <- sort(as.vector(residual(draw(probabilities))))
    if (length(value) != nr || anyNA(value)) {
      stop("Simulated residuals do not match the observed residual layout.", call. = FALSE)
    }
    value
  }
  if (method == "uniform") {
    theoretical <- numeric(nr)
    for (i in seq_len(n)) theoretical <- theoretical + next_sample() / n
    bounds <- NULL
  } else if (as.double(nr) * n <= max_cells) {
    sims <- matrix(NA_real_, nr, n)
    for (i in seq_len(n)) sims[, i] <- next_sample()
    theoretical <- stats::quantile(sims, probabilities, names = FALSE)
    alpha <- (1 - level) / 2
    bounds <- apply(sims, 1L, stats::quantile,
      probs = c(alpha, 1 - alpha), names = FALSE)
    bounds <- matrix(bounds, nrow = 2L)
  } else {
    result <- qq_disk_reference(next_sample, nr, n, probabilities, level, max_cells)
    theoretical <- result$theoretical
    bounds <- result$bounds
  }
  out <- tibble::tibble(theoretical = theoretical,
    residuals = if (detrend) r - theoretical else r)
  if (!is.null(bounds)) {
    out$lower <- bounds[1L, ] - if (detrend) theoretical else 0
    out$upper <- bounds[2L, ] - if (detrend) theoretical else 0
  }
  out
}

# Store each sorted draw on disk when the full matrix would exceed the memory
# limit. Read small blocks to calculate intervals, then merge the sorted files
# to recover the same pooled quantiles as the calculation in memory.
qq_disk_reference <- function(next_sample, nr, n, probabilities, level, max_cells) {
  directory <- tempfile("gratia-qq-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  paths <- file.path(directory, paste0("draw-", seq_len(n)))
  for (i in seq_len(n)) writeBin(as.double(next_sample()), paths[i], size = 8L)
  block <- max(1L, min(nr, floor(max_cells / max(1, n))))
  bounds <- matrix(NA_real_, 2L, nr)
  alpha <- (1 - level) / 2
  for (first in seq.int(1L, nr, by = block)) {
    count <- min(block, nr - first + 1L)
    values <- vapply(paths, function(path) {
      con <- file(path, "rb")
      on.exit(close(con))
      seek(con, where = as.double(first - 1L) * 8, origin = "start")
      readBin(con, "double", n = count, size = 8L)
    }, numeric(count))
    values <- matrix(values, nrow = count, ncol = n)
    bounds[, seq.int(first, length.out = count)] <- matrix(apply(values,
      1L, stats::quantile, probs = c(alpha, 1 - alpha), names = FALSE), nrow = 2L)
  }
  merge_block <- max(1L, min(nr, floor(max_cells / 4)))
  generation <- 0L
  while (length(paths) > 1L) {
    generation <- generation + 1L
    merged <- character(ceiling(length(paths) / 2))
    for (j in seq_along(merged)) {
      i <- 2L * j - 1L
      if (i == length(paths)) {
        merged[j] <- paths[i]
      } else {
        merged[j] <- file.path(directory, paste0("merge-", generation, "-", j))
        qq_merge_files(paths[i], paths[i + 1L], merged[j], merge_block)
        unlink(paths[c(i, i + 1L)])
      }
    }
    paths <- merged
  }
  positions <- 1 + (as.double(nr) * n - 1) * probabilities
  lower <- floor(positions)
  upper <- ceiling(positions)
  wanted <- sort(unique(c(lower, upper)))
  values <- numeric(length(wanted))
  con <- file(paths, "rb")
  # Windows cannot remove the reference file while its connection is open.
  on.exit(close(con), add = TRUE, after = FALSE)
  # Read successive blocks so each plotted quantile does not require a file seek.
  cursor <- 0
  index <- 1L
  repeat {
    chunk <- readBin(con, "double", n = merge_block, size = 8L)
    if (!length(chunk)) break
    last <- findInterval(cursor + length(chunk), wanted)
    if (last >= index) {
      take <- seq.int(index, last)
      values[take] <- chunk[wanted[take] - cursor]
      index <- last + 1L
    }
    cursor <- cursor + length(chunk)
  }
  a <- values[match(lower, wanted)]
  b <- values[match(upper, wanted)]
  weight <- positions - lower
  theoretical <- a
  interpolate <- which(weight != 0 & a != b)
  theoretical[interpolate] <- (1 - weight[interpolate]) * a[interpolate] +
    weight[interpolate] * b[interpolate]
  list(theoretical = theoretical, bounds = bounds)
}

qq_merge_files <- function(left, right, output, block) {
  a <- file(left, "rb"); b <- file(right, "rb"); out <- file(output, "wb")
  on.exit({ close(a); close(b); close(out) })
  x <- readBin(a, "double", n = block, size = 8L)
  y <- readBin(b, "double", n = block, size = 8L)
  repeat {
    if (!length(x) && !length(y)) break
    if (!length(x)) {
      writeBin(y, out, size = 8L)
      y <- readBin(b, "double", n = block, size = 8L)
    } else if (!length(y)) {
      writeBin(x, out, size = 8L)
      x <- readBin(a, "double", n = block, size = 8L)
    } else {
      pivot <- min(x[length(x)], y[length(y)])
      ix <- findInterval(pivot, x)
      iy <- findInterval(pivot, y)
      writeBin(sort(c(x[seq_len(ix)], y[seq_len(iy)])), out, size = 8L)
      x <- if (ix == length(x)) readBin(a, "double", n = block, size = 8L) else if (ix) x[-seq_len(ix)] else x
      y <- if (iy == length(y)) readBin(b, "double", n = block, size = 8L) else if (iy) y[-seq_len(iy)] else y
    }
  }
  invisible(output)
}

# Model-specific functions can return an object containing just the inputs
# diagnostics need. Shared diagnostic methods work with this prepared object;
# the original fitted object remains available for the package's own methods.
diagnostic_plot_source <- function(model, type = NULL, seed = NULL,
                                    conditioning = NULL, ..., n_dharma = NULL) {
  UseMethod("diagnostic_plot_source")
}
#' @export
diagnostic_plot_source.default <- function(model, type = NULL, seed = NULL,
                                            conditioning = NULL, ..., n_dharma = NULL) {
  prepare_diagnostic_data(model, type = type, seed = seed, conditioning = conditioning, n_dharma = n_dharma, ...)$source
}
#' @export
diagnostic_plot_source.gratia_diagnostics <- function(model, type = NULL,
    seed = NULL, conditioning = NULL, ..., n_dharma = NULL) {
  if (identical(model$type, "dharma")) {
    model <- diagnostic_data(model, type, seed, conditioning, n_dharma = n_dharma, ...)
    source <- model$source
    attr(source, "gratia.default_residual_type") <- model$type
    return(source)
  }
  source <- model$source
  if (!is.null(conditioning) || length(list(...)))
    source <- diagnostic_plot_source(source, type, seed, conditioning, ...)
  attr(source, "gratia.default_residual_type") <- model$type
  source
}
#' @export
diagnostic_plot_source.gam <- function(model, type = NULL, seed = NULL,
                                       conditioning = NULL, ..., n_dharma = NULL) {
  if (!is.null(type)) type <- diagnostic_type(model, type)
  check_distribution_conditioning(conditioning)
  if (identical(type, "dharma")) return(cache_dharma_residuals(model, n_dharma, seed))
  model
}
#' @export
diagnostic_plot_source.lm <- function(model, type = NULL, seed = NULL,
                                      conditioning = NULL, ..., n_dharma = NULL) {
  check_distribution_conditioning(conditioning)
  diagnostic_plot_source.gam(prepare_lm_diagnostics(model), type, seed, conditioning, n_dharma = n_dharma)
}
#' @export
diagnostic_plot_source.gratia_diagnostic_proxy <- function(model, type = NULL,
    seed = NULL, conditioning = NULL, ..., n_dharma = NULL) {
  if (!is.null(type)) type <- diagnostic_type(model, type)
  if ((is.null(conditioning) || identical(conditioning, model$conditioning)) &&
      !length(list(...))) return(if (identical(type, "dharma")) cache_dharma_residuals(model, n_dharma, seed) else model)
  original <- diagnostic_original(model)
  if (is.null(original)) stop("Cannot change conditioning on a proxy without its fitted model.", call. = FALSE)
  diagnostic_plot_source(original, type, seed,
    conditioning = conditioning %||% model$conditioning, n_dharma = n_dharma, ...)
}
#' @export
diagnostic_plot_source.gratia_glmmtmb_diagnostic <-
  diagnostic_plot_source.gratia_diagnostic_proxy

diagnostic_original <- function(model) UseMethod("diagnostic_original")
#' @export
diagnostic_original.default <- function(model) model
#' @export
diagnostic_original.gratia_diagnostics <- function(model) diagnostic_original(model$source)
#' @export
diagnostic_original.gratia_diagnostic_proxy <- function(model) model$original
#' @export
diagnostic_original.gratia_glmmtmb_diagnostic <- function(model) {
  original <- model$source
  original$na.action <- model$na.action
  attr(original$frame, "na.action") <- model$na.action
  original
}

calibrated_residual_reference <- function(data, type, level, detrend = FALSE,
    by_family = FALSE, distribution = NULL, n_simulate = 50L) {
  data$.row <- seq_len(nrow(data))
  data <- data[is.finite(data$residuals), , drop = FALSE]
  if (!nrow(data)) stop("No finite calibrated residuals are available.", call. = FALSE)
  parts <- if (by_family && ".family" %in% names(data))
    split(data, data$.family, drop = TRUE) else list(data)
  transform <- if (type == "pit") identity else stats::qnorm
  independent <- is.null(distribution) || (isTRUE(distribution$independent) &&
    !identical(distribution$calibration, "simulation ranks"))
  joint <- !independent && isTRUE(distribution$joint_available) && is.function(distribution$rd)
  result <- dplyr::bind_rows(lapply(parts, function(part) {
    part <- part[order(part$residuals), , drop = FALSE]
    n <- nrow(part); i <- seq_len(n); alpha <- (1 - level) / 2
    part$theoretical <- transform(stats::ppoints(n))
    if (independent) {
      part$lower <- transform(stats::qbeta(alpha, i, n + 1 - i))
      part$upper <- transform(stats::qbeta(1 - alpha, i, n + 1 - i))
    } else if (joint) {
      reference <- qq_reference_draws(part$residuals,
        draw = function(p) distribution$rd(),
        residual = function(y) as.vector(distribution_quantile_residuals(
          distribution, observe_diagnostic_response(distribution, y), type))[part$.row],
        n = n_simulate, method = "simulate", level = level)
      part$lower <- reference$lower
      part$upper <- reference$upper
    }
    if (detrend) {
      part$residuals <- part$residuals - part$theoretical
      if ("lower" %in% names(part)) {
        part$lower <- part$lower - part$theoretical
        part$upper <- part$upper - part$theoretical
      }
    }
    part
  }))
  attr(result, "band_method") <- if (independent) "iid" else if (joint) "joint" else "none"
  result
}

# mgcv predict() omits offsets passed through the fitting call's offset argument,
# even when newdata is absent. Restore its argument contribution separately;
# let predict() handle formula offsets, fixed smooth values and excluded terms.
fitting_link_correction <- function(model) {
  if (is.null(model$linear.predictors) && is.null(model$offset)) return(0)
  eta <- if (needs_fitting_offset_restore(model)) fitting_linear_predictors(model) else
    model$linear.predictors %||% fitting_linear_predictors(model)
  eta - predict_model(model, type = "link")
}

family_prediction_parameters <- function(family, eta) {
  ft <- family_type(family)
  if (!inherits(family, "general.family")) return(family$linkinv(eta))
  eta <- as.matrix(eta)
  mu <- if (is.function(family$predict)) family$predict(family, eta = eta)[[1L]] else NULL
  if (ft == "multivariate_normal") return(eta)
  if (ft == "multinom") return(mu)
  if (is.matrix(mu) && NCOL(mu) == NCOL(eta)) {
    if (ft == "gumbls" && (attr(family, "gratia.gumbel_mean") %||%
        native_mgcv_hook(family$predict, mgcv::gumbls()$predict))) {
      mu[, 1L] <- mu[, 1L] + (-digamma(1)) * exp(mu[, 2L])
    }
    return(mu)
  }
  do.call(cbind, lapply(seq_len(NCOL(eta)), function(j) {
    family$linfo[[j]]$linkinv(eta[, j])
  }))
}

# Older diagnostic_data() methods may not accept conditioning. Pass that
# argument only when the caller explicitly selects how to handle random effects.
prepare_diagnostic_data <- function(model, type = NULL, seed = NULL,
                                    conditioning = NULL, ..., n_dharma = NULL) {
  if (!is.null(type)) type <- diagnostic_type(model, type)
  if (identical(type, "dharma") ||
      (inherits(model, "gratia_diagnostics") && identical(model$type, "dharma"))) {
    return(diagnostic_data(model, type, seed, conditioning = conditioning,
      n_dharma = n_dharma, ...))
  }
  if (is.null(conditioning)) diagnostic_data(model, type, seed, ...) else
    diagnostic_data(model, type, seed, conditioning = conditioning, ...)
}


calibrated_reference_subtitle <- function(type) {
  diagnostic_qq_subtitle(if (type %in% c("pit", "dharma")) "uniform" else "normal")
}

diagnostic_posterior_source <- function(model) UseMethod("diagnostic_posterior_source")
#' @export
diagnostic_posterior_source.default <- function(model) model
#' @export
diagnostic_posterior_source.gratia_diagnostics <- function(model)
  diagnostic_posterior_source(model$source)
#' @export
diagnostic_posterior_source.gratia_glmmtmb_diagnostic <- function(model) model$source

diagnostic_context_label <- function(distribution) {
  label <- sub("^; ", "", diagnostic_reference_description(distribution, ""))
  if (nzchar(label)) label else NULL
}


# CDF checks can remain useful when the population mean does not exist.
# Explain that limitation in panels requiring an expected response: a location
# parameter or a finite simulation average would misrepresent the expectation.
diagnostic_mean_panel <- function(distribution) {
  if (distribution_moment_available(distribution, "mean")) return(NULL)
  reason <- distribution$moment_reason %||% "The fitted response expectation does not exist."
  ggplot2::ggplot() + ggplot2::theme_void() +
    ggplot2::annotate("text", x = 0, y = 0,
      label = stringr::str_wrap(reason, width = 40L), size = 3.5) +
    ggplot2::labs(title = "Observed vs fitted unavailable")
}


# Find supported model-specific arguments on the original fitted object.
# A field or component selection then reaches the function that prepares
# diagnostic inputs, rather than the function that arranges the plots.
diagnostic_adapter_arguments <- function(model, arguments) {
  original <- diagnostic_original(model)
  supported <- character()
  for (generic in c("diagnostic_data", "response_distribution")) {
    for (cl in class(original)) {
      method <- utils::getS3method(generic, cl, optional = TRUE,
        envir = environment(diagnostic_data))
      if (!is.null(method)) {
        supported <- union(supported, names(formals(method)))
        break
      }
    }
  }
  supported <- setdiff(supported, c("model", "type", "seed", "conditioning", "..."))
  arguments[intersect(names(arguments), supported)]
}

wrap_diagnostic_appraisal <- function(..., plot_args = list()) {
  do.call(patchwork::wrap_plots, c(list(...), plot_args))
}
