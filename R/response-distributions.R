# Distribution functions capture fitted parameters before diagnostics call them.
# NULL marks an unavailable operation. Model-specific functions prepare these
# inputs so other packages do not have to use mgcv's family argument signatures.
response_distribution <- function(model, conditioning = NULL, ..., response = NULL) {
  diagnostic_response_selection(model, response)
  UseMethod("response_distribution")
}

new_response_distribution <- function(mean, rd = NULL, cdf = NULL, qf = NULL,
                                      bounds = NULL, variance = NULL,
                                      family = NULL, cdf_left = NULL, independent = TRUE,
                                      joint_available = is.function(rd),
                                      marginal_rd = rd, dependence_reason = NULL,
                                      conditioning = "conditional", approximation = NULL,
                                      mean_available = TRUE, variance_available = TRUE,
                                      moment_reason = NULL) {
  for (hook in list(rd, cdf, qf, bounds, cdf_left)) {
    if (!is.null(hook) && !is.function(hook))
      stop("Distribution hooks must be functions or NULL.", call. = FALSE)
  }
  structure(list(mean = mean, rd = rd, cdf = cdf, qf = qf, bounds = bounds,
    variance = variance, family = family, cdf_left = cdf_left, independent = independent,
    joint_available = joint_available, marginal_rd = marginal_rd,
    dependence_reason = dependence_reason, conditioning = conditioning,
    approximation = approximation, mean_available = mean_available,
    variance_available = variance_available, moment_reason = moment_reason),
    class = "gratia_response_distribution")
}

# Record whether a finite population mean or variance exists separately from
# whether its value has been calculated. When availability is TRUE, a NULL value
# can be estimated by simulating complete response datasets.
distribution_moment_available <- function(distribution, moment) {
  !identical(distribution[[paste0(moment, "_available")]], FALSE)
}

check_distribution_residual_moments <- function(distribution, type) {
  if (!type %in% c("response", "pearson")) return(invisible(distribution))
  required <- if (type == "pearson") c("mean", "variance") else "mean"
  for (moment in required) {
    if (!distribution_moment_available(distribution, moment))
      stop(type, " residuals require a finite response ", moment, ". ",
        distribution$moment_reason %||% "This population moment does not exist.",
        " Use PIT or quantile residuals.", call. = FALSE)
  }
  invisible(distribution)
}

#' @export
response_distribution.default <- function(model, conditioning = NULL, ...) {
  stop("A fitted response distribution is unavailable for this model class.",
    call. = FALSE)
}

check_distribution_conditioning <- function(conditioning) {
  if (!is.null(conditioning) && !identical(conditioning, "conditional"))
    stop("This distribution supports conditioning = 'conditional' only.",
      call. = FALSE)
}

#' @export
response_distribution.lm <- function(model, conditioning = NULL, ...) {
  check_distribution_conditioning(conditioning)
  model <- model_used_rows(model)
  w <- stats::model.weights(stats::model.frame(model))
  if (is.null(w)) w <- rep(1, length(model$fitted.values))
  distribution_from_family(stats::gaussian(), model$fitted.values, w,
    stats::deviance(model) / stats::df.residual(model),
    y = stats::model.response(stats::model.frame(model)))
}

#' @export
response_distribution.glm <- function(model, conditioning = NULL, ...) {
  check_distribution_conditioning(conditioning)
  model <- model_used_rows(model)
  distribution_from_family(stats::family(model), model$fitted.values,
    binomial_sampling_weights(model, model$prior.weights), summary(model)$dispersion,
    y = model$y, eta = model$linear.predictors)
}

#' @export
response_distribution.gam <- function(model, conditioning = NULL, ..., response = NULL) {
  model <- gfam_select_diagnostics(model, response, supplied = !missing(response))
  check_distribution_conditioning(conditioning)
  model <- model_used_rows(model)
  if (is_gfam_family(stats::family(model))) {
    context <- gfam_diagnostic_context(model)
    selected <- attr(model, "gratia.gfam.response", exact = TRUE)
    if (!is.null(selected)) return(bam_response_dependence(model,
      gfam_component_distribution(context, selected), stats::family(model)))
    components <- lapply(seq_along(context$components), function(i)
      if (any(context$index == i)) gfam_component_distribution(context, i) else NULL)
    return(bam_response_dependence(model,
      combine_response_distributions(components, context$index), stats::family(model)))
  }
  f <- stats::family(model)
  # ziP, ordinal and multinom family functions expect linear predictors.
  # Their expected responses are calculated separately.
  ft <- family_type(f)
  latent <- ft == "multinom" ||
    grepl("^ordered_categorical|^zero_inflated_poisson", ft)
  mu <- if (latent) model$linear.predictors else model$fitted.values
  d <- distribution_from_family(f, mu, binomial_sampling_weights(model, model$prior.weights),
    model$sig2, y = model$y, eta = model$linear.predictors)
  bam_response_dependence(model, d, f)
}

#' @export
response_distribution.scam <- response_distribution.gam

bam_response_dependence <- function(model, d, f) {
  if (!is.null(model$AR1.rho) && model$AR1.rho != 0) {
    d$independent <- FALSE
    if (family_type(f) == "gaussian" && identical(f$link, "identity")) {
      rho <- model$AR1.rho
      start <- if (inherits(model, "scam")) model$AR.start else
        model$model[["(AR.start)"]]
      start <- start %||% rep(FALSE, NROW(model$model))
      if (length(start) != NROW(model$model) || anyNA(start)) stop(
        "AR.start must identify series starts for every fitting observation.",
        call. = FALSE)
      start[1L] <- TRUE
      sd <- sqrt(model$sig2 / model$prior.weights)
      d$rd <- function() {
        z <- stats::rnorm(length(sd))
        error <- z
        for (i in seq_along(z)) if (!start[i])
          error[i] <- rho * error[i - 1L] + sqrt(1 - rho^2) * z[i]
        model$fitted.values + sd * error
      }
      d$joint_available <- TRUE
      d$dependence_reason <- "Gaussian AR1 response correlation"
      d$marginal_rd <- NULL
    } else {
      # This AR1 correction acts on residuals during fitting without supplying
      # a joint response distribution. Its marginal CDF still supports PIT
      # checks, but joint reference bands need a complete response simulator.
      d$marginal_rd <- d$rd
      d["rd"] <- list(NULL)
      d$joint_available <- FALSE
      d$dependence_reason <- paste("BAM GEE working AR1 correlation does not",
        "define a joint response distribution")
      d$approximation <- "Marginal fitted-family check; GEE working correlation"
    }
  }
  d
}

# Keep existing family functions and caller replacements unchanged. These
# wrappers supply their fitted parameters before shared diagnostics call them.
distribution_from_family <- function(family, mu, wt, scale, y = NULL, eta = NULL) {
  if (is.null(wt)) wt <- rep(1, NROW(mu))
  if (length(wt) == 1L) wt <- rep(wt, NROW(mu))
  ft <- family_type(family)
  native_beta <- ft == "beta_regression" &&
    native_beta_hook(family$rd, family, mgcv::betar()$rd) &&
    (is.null(family$cdf) || isTRUE(attr(family$cdf, "gratia_beta_cdf"))) &&
    (is.null(family$qf) || native_beta_hook(family$qf, family, mgcv::betar()$qf))
  family <- fix_family_rd(fix_family_qf(fix_family_cdf(family)))
  bind_cdf <- function(hook) {
    if (is.null(hook)) return(NULL)
    args <- names(formals(hook))
    native_tails <- "lower_tail" %in% args || "lower.tail" %in% args
    fun <- function(q, log_p = FALSE, lower_tail = TRUE) {
      call <- list(q = q, mu = mu, wt = wt, scale = scale)
      lp <- intersect(c("log_p", "logp", "log.p"), args)
      lt <- intersect(c("lower_tail", "lower.tail"), args)
      if (length(lp)) call[[lp[1L]]] <- log_p
      if (length(lt)) call[[lt[1L]]] <- lower_tail
      value <- do.call(hook, call)
      if (!length(lp)) value <- if (log_p) log(value) else value
      if (!native_tails && !lower_tail)
        value <- if (log_p) log1mexp(value) else 1 - value
      value
    }
    attr(fun, "native_log_tails") <- native_tails &&
      any(c("log_p", "logp", "log.p") %in% args)
    fun
  }
  bind_qf <- function(hook) {
    if (is.null(hook)) return(NULL)
    args <- names(formals(hook))
    function(p, log_p = FALSE, lower_tail = TRUE) {
      # Some older inverse CDFs accept lower tails only. Convert a requested
      # upper probability before calling them, and detect any lost precision.
      original <- p
      lp <- intersect(c("log_p", "logp", "log.p"), args)
      lt <- intersect(c("lower_tail", "lower.tail"), args)
      if (!length(lt) && !lower_tail)
        p <- if (log_p) log1mexp(p) else 1 - p
      if (!length(lp) && log_p) p <- exp(p)
      call <- list(p = p, mu = mu, wt = wt, scale = scale)
      if (length(lp)) call[[lp[1L]]] <- log_p
      if (length(lt)) call[[lt[1L]]] <- lower_tail
      value <- do.call(hook, call)
      if ((!length(lt) && !lower_tail) || (!length(lp) && log_p))
        check_legacy_quantile_tail(value, original, p, log_p, log_p && length(lp) > 0)
      value
    }
  }
  rd <- if (is.function(family$rd)) function() family$rd(mu, wt, scale) else NULL
  cdf <- bind_cdf(family$cdf)
  # Use a shash CDF that returns log probabilities correctly, without
  # replacing the original family function.
  if (ft == "shash" && native_mgcv_hook(family$cdf, mgcv::shash()$cdf) &&
      "logp" %in% names(formals(family$cdf))) cdf <- bind_cdf(cdf_shash)
  qf <- bind_qf(family$qf)
  # For recognized mgcv functions, use implementations that accept logarithms
  # and upper tails. Leave the original family functions and caller replacements
  # unchanged.
  if (ft == "gaussian" && native_mgcv_hook(family$qf,
      mgcv::fix.family.qf(stats::gaussian())$qf)) qf <- bind_qf(qf_gaussian)
  if (ft == "poisson" && native_mgcv_hook(family$qf,
      mgcv::fix.family.qf(stats::poisson())$qf)) qf <- bind_qf(qf_poisson)
  if (native_beta) qf <- bind_qf(make_qf_beta(family$getTheta(TRUE), betar_eps(family)))
  if (ft == "shash" && native_mgcv_hook(family$qf, mgcv::shash()$qf) &&
      !"log_p" %in% names(formals(family$qf))) qf <- bind_qf(qf_shash)
  bounds <- function(y) distribution_bounds(y, family, wt)
  cdf_left <- if (is.function(cdf)) function(q, log_p = FALSE, lower_tail = TRUE) {
    at <- q
    if (ft %in% c("poisson", "negative_binomial", "ziplss", "cpois", "multinom") ||
        grepl("^ordered_categorical|^zero_inflated_poisson", ft)) at <- ceiling(q) - 1
    if (ft == "binomial") {
      q <- rep_len(q, max(length(q), length(wt)))
      at <- binomial_left_count(q, wt) / pmax(wt, 1)
      zero <- which(wt == 0)
      at[zero] <- ifelse(q[zero] > 0, 0, -1)
    }
    if (ft %in% c("tweedie", "twlss")) at[which(q == 0)] <- -Inf
    cdf(at, log_p = log_p, lower_tail = lower_tail)
  } else NULL
  response_mean <- family_response_mean(family, mu)
  if (ft == "binomial") response_mean[which(wt == 0)] <- 0
  d <- new_response_distribution(response_mean, rd, cdf, qf,
    bounds, family = family, cdf_left = cdf_left,
    independent = ft != "multivariate_normal")
  if (native_beta) {
    eps <- betar_eps(family)
    moments <- beta_clamped_moments(mu, family$getTheta(TRUE), eps)
    d <- clamp_response_distribution(d, eps, 1 - eps,
      mean = moments$mean, variance = moments$variance)
    d$approximation <- paste("Native epsilon-clamped observation approximation;",
      "underlying beta likelihood")
  }
  if (ft == "gevlss") {
    shape <- mu[, 3L]
    d$mean_available <- all(!is.na(shape) & shape < 1)
    d$variance_available <- all(!is.na(shape) & shape < .5)
    d$mean[which(is.na(shape) | shape >= 1)] <- NA_real_
    d$variance <- exp(2 * mu[, 2L]) * gev_standard_variance(shape)
    d$moment_reason <- "GEV finite means require shape < 1 and finite variances shape < 0.5."
  }
  if (ft %in% c("shash", "gevlss") && native_mgcv_hook(family$residuals,
      if (ft == "shash") mgcv::shash()$residuals else mgcv::gevlss()$residuals))
    d$diagnostic_residuals <- normalized_general_residuals(d, mu, ft)
  extend_family_distribution(d, family, mu, wt, scale, y, eta)
}

distribution_bounds <- function(y, family, wt = 1) {
  ft <- family_type(family)
  if (ft %in% c("cnorm", "clog", "cpois"))
    return(censored_response_bounds(y, discrete = ft == "cpois"))
  lower <- y
  count <- ft %in% c("poisson", "negative_binomial", "ziplss", "ocat", "zip", "multinom") ||
    grepl("^ordered_categorical|^zero_inflated_poisson", ft)
  if (count) lower <- y - 1
  if (ft == "binomial") lower <- y - 1 / pmax(wt, 1)
  if (ft %in% c("tweedie", "twlss")) lower[which(y == 0)] <- -Inf
  list(lower = lower, upper = y)
}

distribution_quantile_residuals <- function(distribution, y,
                                           type = c("pit", "quantile"),
                                           seed = NULL,
                                           quantile_function = stats::qnorm) {
  type <- match.arg(type)
  if (!is.function(distribution$cdf))
    stop("Quantile residuals are unavailable: no response CDF.", call. = FALSE)
  compute <- function() {
    bounds <- if (is.function(distribution$bounds)) distribution$bounds(y) else
      list(lower = y, upper = y)
    cdf <- distribution$cdf
    native <- attr(cdf, "native_log_tails")
    if (is.null(native)) native <- "lower_tail" %in% names(formals(cdf))
    if (isTRUE(native)) {
      adapter <- function(q, mu, wt, scale, log_p = FALSE, lower_tail = TRUE)
        cdf(q, log_p = log_p, lower_tail = lower_tail)
      return(log_tail_residuals(bounds, NULL, NULL, NULL, adapter, type,
        quantile_function = quantile_function))
    }
    r <- if (is.function(bounds$cdf_upper)) bounds$cdf_upper() else cdf(bounds$upper)
    random <- which(bounds$lower != bounds$upper | (bounds$left_limit %||% FALSE))
    if (length(random)) {
      lower <- if (is.function(bounds$cdf_lower)) bounds$cdf_lower() else cdf(bounds$lower)
      r[random] <- stats::runif(length(random), lower[random], r[random])
    }
    if (type == "quantile") quantile_function(r) else r
  }
  unname(if (is.null(seed)) compute() else withr::with_seed(seed, compute()))
}

# Each observation uses the distribution selected by its group index. Preserve
# its CDF jumps and calibration information when assembling the response vector.
combine_response_distributions <- function(distributions, index) {
  active <- sort(unique(index))
  combine <- function(hook) {
    if (!all(vapply(distributions[active], function(d)
        is.function(d[[hook]]), logical(1)))) return(NULL)
    function(value = NULL, log_p = FALSE, lower_tail = TRUE) {
      out <- numeric(length(index))
      for (i in active) {
        rows <- which(index == i)
        d <- distributions[[i]]
        out[rows] <- if (hook == "rd") d$rd() else
          d[[hook]](rep_len(value, length(index))[rows],
            log_p = log_p, lower_tail = lower_tail)
      }
      out
    }
  }
  mean <- numeric(length(index))
  for (i in active) mean[index == i] <- distributions[[i]]$mean %||% NA_real_
  bounds <- function(y) {
    lower <- upper <- numeric(length(index))
    left_limit <- logical(length(index))
    pieces <- vector("list", length(distributions))
    for (i in active) {
      rows <- which(index == i)
      yy <- slice_observations(y, rows)
      b <- if (is.function(distributions[[i]]$bounds)) distributions[[i]]$bounds(yy)
        else list(lower = yy, upper = yy)
      pieces[[i]] <- b
      lower[rows] <- b$lower
      upper[rows] <- b$upper
      left_limit[rows] <- b$left_limit %||% FALSE
    }
    cdf_lower <- function(log_p = FALSE, lower_tail = TRUE) {
      out <- numeric(length(index))
      for (i in active) {
        rows <- which(index == i)
        b <- pieces[[i]]
        out[rows] <- if (is.function(b$cdf_lower))
          b$cdf_lower(log_p = log_p, lower_tail = lower_tail) else
          distributions[[i]]$cdf(b$lower, log_p = log_p, lower_tail = lower_tail)
      }
      out
    }
    cdf_upper <- function(log_p = FALSE, lower_tail = TRUE) {
      out <- numeric(length(index))
      for (i in active) {
        rows <- which(index == i)
        b <- pieces[[i]]
        out[rows] <- if (is.function(b$cdf_upper))
          b$cdf_upper(log_p = log_p, lower_tail = lower_tail) else
          distributions[[i]]$cdf(b$upper, log_p = log_p, lower_tail = lower_tail)
      }
      out
    }
    list(lower = lower, upper = upper, left_limit = left_limit,
      cdf_lower = cdf_lower, cdf_upper = cdf_upper)
  }

  cdf <- combine("cdf")
  if (!is.null(cdf)) attr(cdf, "native_log_tails") <- all(vapply(
    distributions[active], function(d) !identical(
      attr(d$cdf, "native_log_tails"), FALSE), logical(1)))
  cdf_left <- if (is.function(cdf)) function(q, log_p = FALSE, lower_tail = TRUE) {
    out <- numeric(length(index))
    q <- rep_len(q, length(index))
    for (i in active) {
      rows <- which(index == i)
      out[rows] <- distribution_cdf_left(distributions[[i]], q[rows],
        log_p = log_p, lower_tail = lower_tail)
    }
    out
  } else NULL
  d <- new_response_distribution(mean, rd = combine("rd"), cdf = cdf,
    qf = combine("qf"), bounds = bounds, family = "indexed response families",
    cdf_left = cdf_left,
    independent = all(vapply(distributions[active], function(d) isTRUE(d$independent), logical(1))),
    mean_available = all(vapply(distributions[active], distribution_moment_available,
      logical(1), moment = "mean")),
    variance_available = all(vapply(distributions[active], distribution_moment_available,
      logical(1), moment = "variance")),
    joint_available = all(vapply(distributions[active], function(d) isTRUE(d$joint_available), logical(1))),
    marginal_rd = combine("marginal_rd"),
    dependence_reason = collapse_distribution_metadata(distributions[active], "dependence_reason"),
    approximation = collapse_distribution_metadata(distributions[active], "approximation"),
    conditioning = collapse_distribution_metadata(distributions[active], "conditioning"))
  if (any(vapply(distributions[active], function(d)
      identical(d$calibration, "simulation ranks"), logical(1))))
    d$calibration <- "simulation ranks"
  counts <- unique(unlist(lapply(distributions[active], `[[`, "n_reference"),
    use.names = FALSE))
  if (length(counts)) d$n_reference <- counts
  d$reference_available <- !any(vapply(distributions[active], function(d)
    identical(d$reference_available, FALSE), logical(1)))
  d$reference_reason <- collapse_distribution_metadata(distributions[active], "reference_reason")
  d$moment_reason <- collapse_distribution_metadata(distributions[active], "moment_reason")
  if (any(vapply(distributions[active], function(d) is.function(d$observe), logical(1)))) {
    d$observe <- function(y) {
      out <- numeric(length(index))
      censor <- rep(NA_real_, length(index))
      for (i in active) {
        rows <- which(index == i)
        value <- observe_diagnostic_response(distributions[[i]], slice_observations(y, rows))
        if (is.matrix(value)) {
          if (NCOL(value) != 2L) stop("Grouped observation processes require a scalar ",
            "response or a two-column censoring matrix.", call. = FALSE)
          censor[rows] <- value[, 2L]
          value <- value[, 1L]
        } else censor[rows] <- attr(value, "censor") %||% value
        out[rows] <- value
      }
      attr(out, "censor") <- censor
      out
    }
  }
  d
}

family_response_mean <- function(family, mu) {
  ft <- family_type(family)
  if (grepl("^ordered_categorical", ft) || ft == "ocat") {
    cuts <- family$getTheta(TRUE)
    return(rowSums(matrix(vapply(cuts, function(cut) stats::plogis(mu - cut),
      numeric(length(mu))), nrow = length(mu))) + 1)
  }
  if (grepl("^zero_inflated_poisson", ft) || ft == "zip") {
    th <- family$getTheta(TRUE)
    lambda <- exp(mu)
    return(-expm1(-exp(th[1L] + th[2L] * mu)) *
      lambda / (-expm1(-lambda)))
  }
  if (ft == "multinom" && is.null(dim(mu))) mu <- matrix(mu, ncol = family$nlp)
  if (is.matrix(mu) && ft != "multivariate_normal") {
    if (ft == "multinom") {
      eta <- cbind(0, mu)
      p <- exp(eta - apply(eta, 1L, max))
      p <- p / rowSums(p)
      return(as.numeric(p %*% seq.int(0, ncol(mu))))
    }
    if (ft == "ziplss") return(-expm1(-exp(mu[, 2L])) *
      exp(mu[, 1L]) / (-expm1(-exp(mu[, 1L]))))
    if (ft == "shash") {
      delta <- exp(mu[, 4L])
      a <- 1 / delta
      moment <- exp(.25) / sqrt(8 * pi) *
        (besselK(.25, (a + 1) / 2) + besselK(.25, (a - 1) / 2))
      return(mu[, 1L] + delta * exp(mu[, 2L]) * sinh(mu[, 3L] / delta) * moment)
    }
    if (ft == "gevlss") {
      return(mu[, 1L] + exp(mu[, 2L]) * gev_standard_mean(mu[, 3L]))
    }
    return(mu[, 1L])
  }
  mu
}

# Use the distribution's explicit P(Y < q) function when available. Otherwise
# obtain the probability below an observation from its supplied response bounds.
distribution_cdf_left <- function(distribution, q, log_p = FALSE, lower_tail = TRUE) {
  if (is.function(distribution$cdf_left))
    return(distribution$cdf_left(q, log_p = log_p, lower_tail = lower_tail))
  lower <- if (is.function(distribution$bounds)) distribution$bounds(q)$lower else q
  distribution$cdf(lower, log_p = log_p, lower_tail = lower_tail)
}

# Add a point mass at zero to any response distribution, including ones with
# negative support. The CDF jump spans (1-z)F_base(0-) to z+(1-z)F_base(0).
zero_inflated_distribution <- function(distribution, zprob) {
  force(distribution); force(zprob)
  probability <- function(q, log_p, lower_tail, left = FALSE) {
    base <- if (left) distribution_cdf_left(distribution, q,
      log_p = TRUE, lower_tail = lower_tail) else
      distribution$cdf(q, log_p = TRUE, lower_tail = lower_tail)
    q <- rep_len(q, length(base))
    out <- log1p(-zprob) + base
    include <- if (lower_tail) if (left) q > 0 else q >= 0 else
      if (left) q <= 0 else q < 0
    rows <- which(include)
    if (length(rows)) out[rows] <- logspace_add(out[rows], rep_len(log(zprob), length(out))[rows])
    if (log_p) out else exp(out)
  }
  cdf <- if (is.function(distribution$cdf))
    function(q, log_p = FALSE, lower_tail = TRUE)
      probability(q, log_p, lower_tail) else NULL
  cdf_left <- if (is.function(distribution$cdf))
    function(q, log_p = FALSE, lower_tail = TRUE)
      probability(q, log_p, lower_tail, left = TRUE) else NULL
  qf <- if (is.function(distribution$qf) && is.function(cdf))
    function(p, log_p = FALSE, lower_tail = TRUE) {
      log_probability <- quantile_log_probability(p, log_p)
      log_f <- if (lower_tail) log_probability else log1mexp(log_probability)
      log_s <- if (lower_tail) log1mexp(log_probability) else log_probability
      log_f0 <- cdf_left(0, log_p = TRUE)
      log_s0left <- cdf_left(0, log_p = TRUE, lower_tail = FALSE)
      log_s0 <- cdf(0, log_p = TRUE, lower_tail = FALSE)
      log_f0right <- cdf(0, log_p = TRUE)
      n <- max(length(log_f0), length(log_f))
      log_f <- rep_len(log_f, n); log_s <- rep_len(log_s, n)
      below <- which(log_f < log_f0 | log_s > log_s0left)
      above <- which(log_s < log_s0 | log_f > log_f0right)
      value <- rep(0, n)
      invalid <- which(is.na(log_f) | is.na(log_s))
      value[invalid] <- NA_real_
      if (length(below)) {
        pbase <- pmin(0, log_f - log1p(-zprob))
        pbase[which(zprob == 1)] <- 0
        q <- distribution$qf(pbase, log_p = TRUE, lower_tail = TRUE)
        value[below] <- q[below]
      }
      if (length(above)) {
        pbase <- pmin(0, log_s - log1p(-zprob))
        pbase[which(zprob == 1)] <- 0
        q <- distribution$qf(pbase, log_p = TRUE, lower_tail = FALSE)
        value[above] <- q[above]
      }
      value
    } else NULL
  rd <- if (is.function(distribution$rd)) function() {
    y <- distribution$rd()
    y[stats::runif(length(y)) < zprob] <- 0
    y
  } else NULL
  bounds <- function(y) {
    b <- if (is.function(distribution$bounds)) distribution$bounds(y) else
      list(lower = y, upper = y)
    b$left_limit <- (b$left_limit %||% FALSE) | y == 0
    b$cdf_lower <- function(log_p = FALSE, lower_tail = TRUE) {
      out <- cdf(b$lower, log_p = log_p, lower_tail = lower_tail)
      zero <- which(y == 0)
      if (length(zero)) out[zero] <- cdf_left(y, log_p = log_p,
        lower_tail = lower_tail)[zero]
      out
    }
    b
  }
  variance <- if (is.null(distribution$variance)) NULL else
    (1 - zprob) * distribution$variance + zprob * (1 - zprob) * distribution$mean^2
  mean <- if (is.null(distribution$mean)) NULL else (1 - zprob) * distribution$mean
  new_response_distribution(mean, rd, cdf, qf,
    bounds, variance, distribution$family, cdf_left = cdf_left,
    independent = distribution$independent,
    mean_available = distribution_moment_available(distribution, "mean"),
    variance_available = distribution_moment_available(distribution, "variance"),
    moment_reason = distribution$moment_reason)
}

# Restrict responses to Y > lower using the CDF and inverse CDF. Callers can
# supply the mean and variance after this restriction.
truncate_distribution <- function(distribution, lower = 0, mean = NULL,
                                   variance = NULL) {
  if (!is.function(distribution$cdf)) stop("Truncation requires a CDF.", call. = FALSE)
  log_survival <- distribution$cdf(lower, log_p = TRUE, lower_tail = FALSE)
  cdf <- function(q, log_p = FALSE, lower_tail = TRUE) {
    log_s <- distribution$cdf(q, log_p = TRUE, lower_tail = FALSE) - log_survival
    q <- rep_len(q, length(log_s))
    log_s[which(q <= lower)] <- 0
    out <- if (lower_tail) log1mexp(log_s) else log_s
    if (log_p) out else exp(out)
  }
  qf <- if (is.function(distribution$qf))
    function(p, log_p = FALSE, lower_tail = TRUE) {
      log_s <- if (lower_tail) log1mexp(quantile_log_probability(p, log_p)) else
        quantile_log_probability(p, log_p)
      log_s <- rep_len(log_s, max(length(log_s), length(log_survival)))
      value <- distribution$qf(log_s + log_survival, log_p = TRUE, lower_tail = FALSE)
      if (is.character(distribution$family) && distribution$family %in%
          c("poisson", "nbinom1", "nbinom2"))
        value[which(log_s == 0)] <- floor(lower) + 1
      value
    } else NULL
  rd <- if (is.function(qf)) function() qf(stats::runif(length(distribution$mean))) else NULL
  cdf_left <- function(q, log_p = FALSE, lower_tail = TRUE) {
    log_s <- distribution_cdf_left(distribution, q, log_p = TRUE,
      lower_tail = FALSE) - log_survival
    q <- rep_len(q, length(log_s))
    log_s[which(q <= lower)] <- 0
    out <- if (lower_tail) log1mexp(log_s) else log_s
    if (log_p) out else exp(out)
  }
  new_response_distribution(mean, rd, cdf, qf, distribution$bounds, variance,
    distribution$family, cdf_left = cdf_left, independent = distribution$independent)
}

# Here dispersion is the Gaussian SD, Gamma shape, or SD of a lognormal response
# on the log scale. Beta variance is mu*(1-mu)/(1+dispersion); NB1 variance is
# mu*(1+dispersion); NB2 variance is mu+mu^2/dispersion; Tweedie variance is
# dispersion*mu^power. Binomial responses are proportions.
distribution_from_parameters <- function(family, mu, dispersion = 1,
                                         trials = 1, power = NULL, df = NULL) {
  force(family); force(mu); force(dispersion); force(trials); force(power)
  if (identical(family, "student")) {
    return(student_response_distribution(mu, dispersion, df))
  }
  if (startsWith(family, "truncated_")) {
    name <- sub("^truncated_", "", family)
    if (!name %in% c("poisson", "nbinom1", "nbinom2"))
      stop("Automatic truncation moments are available for nonnegative counts only.",
        call. = FALSE)
    base <- distribution_from_parameters(name, mu, dispersion, trials, power)
    survival <- exp(base$cdf(0, log_p = TRUE, lower_tail = FALSE))
    mean <- mu / survival
    extra <- if (name == "poisson") 0 else
      if (name == "nbinom1") dispersion else mu / dispersion
    excess <- mean - 1
    small <- which(mu < 1e-4 & extra < 1e-4)
    # When mu and extra are small, evaluate E[Y|Y>0]-1 with a series.
    # Subtracting nearly equal moments would otherwise produce a negative variance.
    excess[small] <- (mu[small] + rep_len(extra, length(mu))[small]) / 2 +
      (mu[small]^2 - rep_len(extra, length(mu))[small]^2) / 12 +
      (rep_len(extra, length(mu))[small]^3 -
        mu[small]^2 * rep_len(extra, length(mu))[small]) / 24
    variance <- mean * (mu + extra - excess)
    return(truncate_distribution(base, mean = mean, variance = variance))
  }
  n <- length(mu)
  dispersion <- rep_len(dispersion, n)
  trials <- rep_len(trials, n)
  continuous <- family %in% c("gaussian", "Gamma", "beta", "lognormal", "tweedie")
  if (!family %in% c("gaussian", "Gamma", "beta", "lognormal", "tweedie",
      "poisson", "binomial", "nbinom1", "nbinom2"))
    stop("No response-distribution constructor for family '", family, "'.", call. = FALSE)
  size <- if (family == "nbinom1") mu / dispersion else dispersion
  # Zero mean implies Y = 0 with probability one. NB1 size then equals zero;
  # use a positive dummy size so rnbinom returns the same degenerate response.
  if (family == "nbinom1") size[which(mu == 0)] <- 1
  cdf <- function(q, log_p = FALSE, lower_tail = TRUE) {
    switch(family,
      gaussian = stats::pnorm(q, mu, dispersion, lower.tail = lower_tail, log.p = log_p),
      poisson = stats::ppois(q, mu, lower.tail = lower_tail, log.p = log_p),
      binomial = stats::pbinom(q * (trials + as.numeric(trials == 0)), trials, mu, lower.tail = lower_tail, log.p = log_p),
      nbinom1 =, nbinom2 = stats::pnbinom(q, mu = mu, size = size,
        lower.tail = lower_tail, log.p = log_p),
      Gamma = stats::pgamma(q, shape = dispersion, scale = mu / dispersion,
        lower.tail = lower_tail, log.p = log_p),
      beta = stats::pbeta(q, mu * dispersion, (1 - mu) * dispersion,
        lower.tail = lower_tail, log.p = log_p),
      lognormal = stats::plnorm(q, log(mu) - dispersion^2 / 2, dispersion,
        lower.tail = lower_tail, log.p = log_p),
      tweedie = fitted_tweedie_cdf(q, mu, power = power, phi = dispersion,
        lower_tail = lower_tail, log_p = log_p))
  }
  qf <- function(p, log_p = FALSE, lower_tail = TRUE) {
    switch(family,
      gaussian = stats::qnorm(p, mu, dispersion, lower.tail = lower_tail, log.p = log_p),
      poisson = stats::qpois(p, mu, lower.tail = lower_tail, log.p = log_p),
      binomial = stats::qbinom(p, trials, mu, lower.tail = lower_tail, log.p = log_p) /
        (trials + as.numeric(trials == 0)),
      nbinom1 =, nbinom2 = stats::qnbinom(p, mu = mu, size = size,
        lower.tail = lower_tail, log.p = log_p),
      Gamma = stats::qgamma(p, shape = dispersion, scale = mu / dispersion,
        lower.tail = lower_tail, log.p = log_p),
      beta = stats::qbeta(p, mu * dispersion, (1 - mu) * dispersion,
        lower.tail = lower_tail, log.p = log_p),
      lognormal = stats::qlnorm(p, log(mu) - dispersion^2 / 2, dispersion,
        lower.tail = lower_tail, log.p = log_p),
      tweedie = fitted_tweedie_quantile(p,
        mu, power = power, phi = dispersion, log_p = log_p, lower_tail = lower_tail))
  }
  rd <- function() {
    switch(family,
      gaussian = stats::rnorm(n, mu, dispersion),
      poisson = stats::rpois(n, mu),
      binomial = stats::rbinom(n, trials, mu) / (trials + as.numeric(trials == 0)),
      nbinom1 =, nbinom2 = stats::rnbinom(n, mu = mu, size = size),
      Gamma = stats::rgamma(n, shape = dispersion, scale = mu / dispersion),
      beta = stats::rbeta(n, mu * dispersion, (1 - mu) * dispersion),
      lognormal = stats::rlnorm(n, log(mu) - dispersion^2 / 2, dispersion),
      tweedie = rtw(mu, p = power, phi = dispersion))
  }
  bounds <- function(y) {
    lower <- y
    if (!continuous) lower <- y - if (family == "binomial") 1 / pmax(trials, 1) else 1
    if (family == "tweedie") lower[which(y == 0)] <- -Inf
    list(lower = lower, upper = y)
  }
  variance <- switch(family, gaussian = dispersion^2, poisson = mu,
    binomial = mu * (1 - mu) / pmax(trials, 1), nbinom1 = mu * (1 + dispersion),
    nbinom2 = mu + mu^2 / size, Gamma = mu^2 / dispersion,
    beta = mu * (1 - mu) / (1 + dispersion),
    lognormal = mu^2 * expm1(dispersion^2), tweedie = dispersion * mu^power)
  cdf_left <- function(q, log_p = FALSE, lower_tail = TRUE) {
    at <- q
    if (!continuous) {
      q <- rep_len(q, max(length(q), length(trials)))
      at <- if (family == "binomial")
        binomial_left_count(q, trials) / pmax(trials, 1) else ceiling(q) - 1
      if (family == "binomial") {
        zero <- which(trials == 0)
        at[zero] <- ifelse(q[zero] > 0, 0, -1)
      }
    }
    if (family == "tweedie") at[which(q == 0)] <- -Inf
    cdf(at, log_p = log_p, lower_tail = lower_tail)
  }
  response_mean <- mu
  if (family == "binomial") {
    response_mean[which(trials == 0)] <- 0
    variance[which(trials == 0)] <- 0
  }
  new_response_distribution(response_mean, rd, cdf, qf, bounds, variance, family,
    cdf_left = cdf_left)
}


# Student-t dispersion is a scale parameter. Its standard deviation also
# depends on the degrees of freedom and does not exist when df <= 2.
student_response_distribution <- function(location, scale, df) {
  n <- length(location)
  if (!is.numeric(df) || !length(df) || any(!is.finite(df) | df <= 0) ||
      !(length(df) %in% c(1L, n)))
    stop("Student-t degrees of freedom must be positive and finite, ",
      "with one value or one per response.", call. = FALSE)
  if (!is.numeric(scale) || !length(scale) || any(!is.finite(scale) | scale <= 0) ||
      !(length(scale) %in% c(1L, n)))
    stop("Student-t scale must be positive and finite, ",
      "with one value or one per response.", call. = FALSE)
  scale <- rep_len(scale, n)
  df <- rep_len(df, n)
  mean_available <- all(df > 1)
  variance_available <- all(df > 2)
  cdf <- function(q, log_p = FALSE, lower_tail = TRUE)
    stats::pt((q - location) / scale, df, lower.tail = lower_tail, log.p = log_p)
  qf <- function(p, log_p = FALSE, lower_tail = TRUE)
    location + scale * stats::qt(p, df, lower.tail = lower_tail, log.p = log_p)
  rd <- function() location + scale * stats::rt(n, df)
  new_response_distribution(
    mean = if (mean_available) location else NULL,
    rd = rd, cdf = cdf, qf = qf,
    bounds = function(y) list(lower = y, upper = y),
    variance = if (variance_available) scale^2 * df / (df - 2) else NULL,
    family = "student", cdf_left = cdf,
    mean_available = mean_available, variance_available = variance_available,
    moment_reason = paste0("Student-t degrees of freedom = ",
      paste(signif(unique(df), 5), collapse = ", "),
      "; a finite mean requires df > 1 and a finite variance requires df > 2."))
}


# The existing Tweedie solver handles one power value per call. Group rows with
# the same power to reuse it when the fitted power varies between observations.
fitted_tweedie_cdf <- function(q, mu, power, phi, log_p = FALSE, lower_tail = TRUE) {
  n <- max(length(q), length(mu), length(power), length(phi))
  q <- rep_len(q, n); mu <- rep_len(mu, n)
  power <- rep_len(power, n); phi <- rep_len(phi, n)
  out <- rep(NA_real_, n)
  for (p in unique(power[!is.na(power)])) {
    rows <- which(power == p)
    out[rows] <- if (p == 2) stats::pgamma(q[rows], shape = 1 / phi[rows],
      scale = mu[rows] * phi[rows], lower.tail = lower_tail, log.p = log_p) else
      ptweedie_mixture(q[rows], mu[rows], p, phi[rows],
        log_p = log_p, lower_tail = lower_tail)
  }
  out
}
fitted_tweedie_quantile <- function(p, mu, power, phi, log_p = FALSE,
                                    lower_tail = TRUE) {
  n <- max(length(p), length(mu), length(power), length(phi))
  p <- rep_len(p, n); mu <- rep_len(mu, n)
  power <- rep_len(power, n); phi <- rep_len(phi, n)
  out <- rep(NA_real_, n)
  for (pw in unique(power[!is.na(power)])) {
    rows <- which(power == pw)
    if (pw == 2) {
      out[rows] <- stats::qgamma(p[rows], shape = 1 / phi[rows],
        scale = mu[rows] * phi[rows], log.p = log_p, lower.tail = lower_tail)
    } else {
      target <- if (lower_tail) p[rows] else if (log_p) log1mexp(p[rows]) else 1 - p[rows]
      out[rows] <- qtweedie_mixture(target, mu[rows], pw, phi[rows], log_p = log_p)
      if (!lower_tail) check_legacy_quantile_tail(out[rows], p[rows], target, log_p, log_p)
    }
  }
  out
}


native_mgcv_hook <- function(hook, reference = NULL) {
  env <- environment(hook)
  native <- !is.null(env) && (identical(env, asNamespace("mgcv")) ||
    identical(parent.env(env), asNamespace("mgcv")))
  # A caller can change a function body while keeping its environment.
  # Match the arguments and body as well before correcting an mgcv function.
  native && (is.null(reference) || (identical(formals(hook), formals(reference)) &&
    identical(body(hook), body(reference))))
}

# At zero shape, the GEV with location 0 and scale 1 has mean Euler's constant.
# Avoid subtracting nearly equal numbers in Gamma(1-shape)-1, and skip shapes
# whose mean does not exist.
gev_standard_mean <- function(shape) {
  out <- rep(Inf, length(shape))
  valid <- which(shape < 1)
  out[valid] <- expm1(lgamma(1 - shape[valid])) / shape[valid]
  tiny <- which(abs(shape) < 1e-5)
  euler <- 0.577215664901533
  out[tiny] <- euler + (euler^2 / 2 + pi^2 / 12) * shape[tiny] +
    (euler^3 / 6 + euler * pi^2 / 12 + 1.202056903159594 / 3) * shape[tiny]^2
  out[is.na(shape)] <- NA_real_
  out
}


# For example, (7/25)*25 can round above 7, changing which CDF jump is selected.
# Recover k when q equals k/n exactly, while leaving nearby q values unchanged.
binomial_left_count <- function(q, trials) {
  scaled <- q * trials
  nearest <- round(scaled)
  support <- trials > 0 & q == nearest / pmax(trials, 1)
  scaled[which(support)] <- nearest[which(support)]
  ceiling(scaled) - 1
}

collapse_distribution_metadata <- function(distributions, field) {
  values <- unique(unlist(lapply(distributions, `[[`, field), use.names = FALSE))
  if (length(values)) paste(values, collapse = "; ") else NULL
}

gev_standard_variance <- function(shape) {
  out <- rep(NA_real_, length(shape))
  valid <- which(!is.na(shape) & shape < .5)
  x <- shape[valid]
  lg <- lgamma(1 - x)
  out[valid] <- exp(2 * lg) * expm1(lgamma(1 - 2 * x) - 2 * lg) / x^2
  small <- which(!is.na(shape) & abs(shape) < 1e-4)
  euler <- 0.577215664901533
  zeta2 <- pi^2 / 6; zeta3 <- 1.202056903159594; zeta4 <- pi^4 / 90
  x <- shape[small]
  out[small] <- zeta2 + (2 * euler * zeta2 + 2 * zeta3) * x +
    (3.5 * zeta4 + 1.5 * zeta2^2 + 4 * euler * zeta3 +
      2 * euler^2 * zeta2) * x^2
  out
}


# Replace values below lower by lower, and values above upper by upper. This
# puts probability mass at both boundaries. Apply it as a separate response
# transformation so original family functions remain unchanged.
clamp_response_distribution <- function(distribution, lower, upper,
                                         mean = NULL, variance = NULL) {
  force(distribution); force(lower); force(upper)
  clamp <- function(x) pmax(lower, pmin(upper, x))
  probability <- function(q, log_p, lower_tail, left = FALSE) {
    value <- if (left) distribution_cdf_left(distribution, q,
      log_p = log_p, lower_tail = lower_tail) else
      distribution$cdf(q, log_p = log_p, lower_tail = lower_tail)
    q <- rep_len(q, length(value))
    below <- if (left) q <= lower else q < lower
    above <- if (left) q > upper else q >= upper
    value[which(below)] <- if (log_p) if (lower_tail) -Inf else 0 else
      if (lower_tail) 0 else 1
    value[which(above)] <- if (log_p) if (lower_tail) 0 else -Inf else
      if (lower_tail) 1 else 0
    value
  }
  cdf <- function(q, log_p = FALSE, lower_tail = TRUE)
    probability(q, log_p, lower_tail)
  cdf_left <- function(q, log_p = FALSE, lower_tail = TRUE)
    probability(q, log_p, lower_tail, left = TRUE)
  bounds <- function(y) {
    b <- if (is.function(distribution$bounds)) distribution$bounds(y) else
      list(lower = y, upper = y)
    b$left_limit <- (b$left_limit %||% FALSE) | y == lower | y == upper
    b$cdf_lower <- function(log_p = FALSE, lower_tail = TRUE)
      cdf_left(y, log_p = log_p, lower_tail = lower_tail)
    b$cdf_upper <- function(log_p = FALSE, lower_tail = TRUE)
      cdf(y, log_p = log_p, lower_tail = lower_tail)
    b
  }
  result <- distribution
  result$rd <- if (is.function(distribution$rd)) function() clamp(distribution$rd()) else NULL
  result$marginal_rd <- if (is.function(distribution$marginal_rd))
    function() clamp(distribution$marginal_rd()) else NULL
  result$cdf <- cdf; result$cdf_left <- cdf_left
  result$qf <- if (is.function(distribution$qf))
    function(p, log_p = FALSE, lower_tail = TRUE)
      clamp(distribution$qf(p, log_p = log_p, lower_tail = lower_tail)) else NULL
  result$bounds <- bounds
  result["mean"] <- list(mean); result["variance"] <- list(variance)
  result$mean_available <- result$variance_available <- TRUE
  result
}

beta_clamped_moments <- function(mu, phi, eps) {
  a <- mu * phi; b <- (1 - mu) * phi
  tail_low <- stats::pbeta(eps, a, b)
  tail_high <- stats::pbeta(1 - eps, a, b, lower.tail = FALSE)
  interval <- function(power) {
    lo <- stats::pbeta(eps, a + power, b)
    hi <- stats::pbeta(1 - eps, a + power, b)
    ifelse(lo > .5, stats::pbeta(eps, a + power, b, lower.tail = FALSE) -
      stats::pbeta(1 - eps, a + power, b, lower.tail = FALSE), hi - lo)
  }
  mean <- eps * tail_low + mu * interval(1) + (1 - eps) * tail_high
  second <- eps^2 * tail_low + mu * (a + 1) / (phi + 1) * interval(2) +
    (1 - eps)^2 * tail_high
  list(mean = mean, variance = pmax(0, second - mean^2))
}


# betar functions share an environment holding their fitted parameters. Its
# parent is the global environment, so namespace checks would miss these functions.
# Recognize them by their shared environment, arguments and bodies.
native_beta_hook <- function(hook, family, reference) {
  is.function(hook) && identical(formals(hook), formals(reference)) &&
    identical(body(hook), body(reference)) &&
    identical(environment(hook), environment(family$getTheta)) &&
    exists(".betarEps", envir = environment(hook), inherits = FALSE)
}


# Shared diagnostics call this function for both observed and simulated responses.
# Use the fitted distribution's mean to correct native shash and GEV centering.
normalized_general_residuals <- function(distribution, mu, family) {
  force(distribution); force(mu); force(family)
  function(y, type) {
    check_distribution_residual_moments(distribution, type)
    if (type == "response") return(y - distribution$mean)
    if (type == "pearson") {
      if (family == "shash") stop("Pearson residuals are unavailable for shash.", call. = FALSE)
      return((y - distribution$mean) / sqrt(distribution$variance))
    }
    if (type != "deviance") stop("Unsupported native residual type.", call. = FALSE)
    if (!distribution_moment_available(distribution, "mean")) stop(
      "Signed GEV deviance residuals require a finite response mean; use PIT or quantile residuals.",
      call. = FALSE)
    if (family == "shash") {
      z <- (y - mu[, 1L]) / (exp(mu[, 2L]) * exp(mu[, 4L]))
      t <- exp(mu[, 4L]) * asinh(z) - mu[, 3L]
      log_cosh <- abs(t) + log1p(exp(-2 * abs(t))) - log(2)
      log_density <- -mu[, 2L] - .5 * log(2 * pi) + log_cosh -
        .5 * log1p(z^2) - .5 * sinh(t)^2
      # Native shash compares the log density with zero. A fitted density above
      # one gives a negative contrast; replacing it by zero would hide this
      # invalid comparison for observed or simulated responses.
      if (any(log_density > 0, na.rm = TRUE)) stop(
        "Native shash deviance is unavailable: the fitted density exceeds its ",
        "zero-log-density reference, giving a negative contrast. ",
        "Use response, PIT or quantile residuals.", call. = FALSE)
      magnitude <- sqrt(-2 * log_density)
    } else {
      xi <- mu[, 3L]
      if (any(xi < -1)) stop("Native GEV deviance residuals require shape >= -1; ",
        "use response, Pearson, PIT or quantile residuals.", call. = FALSE)
      z <- (y - mu[, 1L]) / exp(mu[, 2L])
      log_t <- z
      nonzero <- which(xi != 0)
      log_t[nonzero] <- log1p(xi[nonzero] * z[nonzero]) / xi[nonzero]
      # Keep mgcv's sqrt(log f(mode) - log f(y)) convention, without the usual
      # factor 2. Evaluate its Gumbel limit directly at xi = 0.
      log_mode <- (1 + xi) * log1p(xi)
      log_observation <- (1 + xi) * log_t
      boundary <- which(xi == -1)
      log_mode[boundary] <- log_observation[boundary] <- 0
      magnitude <- sqrt(pmax(0, log_observation + log_mode + expm1(-log_t) - xi))
    }
    magnitude * sign(y - distribution$mean)
  }
}
