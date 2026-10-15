#' Posterior predictive check of the response distribution
#'
#' Compare the observed response distribution with replicated data sets drawn
#' from the approximate posterior predictive distribution of a fitted model.
#'
#' @param model a fitted model with a posterior response sampler.
#' @param n positive integer; number of replicated data sets to draw.
#' @param seed numeric; optional random seed. With an explicit seed, the
#'   caller's random-number state is preserved.
#' @param observed_col,replicated_col colours for the observed and replicated
#'   distributions, respectively. Standalone plots default to orange-red
#'   (`"#D55E00"`) and blue (`"#0072B2"`). When called by [appraise()], the check
#'   uses that function's `line_col` and `ci_col` instead.
#' @param replicated_alpha numeric or `NULL`; opacity of replicated
#'   distributions. The default, `NULL`, uses `0.8` for discrete posterior
#'   median points and interval bars, and `0.2` for continuous density curves.
#' @param level numeric; coverage of the equal-tailed posterior predictive
#'   intervals for discrete responses. Must be strictly between zero and one.
#'   Defaults to `0.9`. Ignored for continuous density overlays.
#' @param conditioning character or `NULL`; mixed-model conditioning mode.
#' @param scalable logical or `NULL`; summarize posterior response draws on a
#'   common grid, using temporary disk storage instead of retaining all responses
#'   in memory. `NULL` enables this at `scalable_threshold` observations.
#' @param scalable_threshold positive integer; observation count for automatic
#'   scalable plotting.
#' @param batch_size positive integer; number of draws per processing batch.
#'   Batch boundaries do not alter seeded draws. Scalable discrete comparisons
#'   with more than 512 distinct response values aggregate probability into
#'   512 shared equal-width bins to bound memory and plot size.
#' @inheritParams observed_fitted_plot
#' @param xlab character or expression; label for the response axis.
#' @param ... arguments passed to [posterior_samples()], such as
#'   `unconditional` or `mvn_method`. For delta sdmTMB fits,
#'   `component = "response"`, `"binomial"`, or `"positive"` selects the
#'   complete response or a fitted component. Prediction at new data is not supported.
#'
#' @details
#' Mixed-model captions name the conditioning mode. Supply `caption` to replace this text, or
#' `caption = ""` to omit it.
#'
#' Each replicated data set includes uncertainty in the coefficients and random
#' variation in the responses, at the covariate values used to fit the model.
#' Linear models and GLMs draw coefficients from a Gaussian distribution with
#' the estimated coefficient covariance, keeping dispersion at its fitted value.
#' For GAMs that model several distribution parameters, all their coefficients
#' are drawn together. Distribution parameters outside this coefficient vector
#' remain at fitted values, including additional parameters of grouped-family
#' components.
#'
#' glmmTMB draws mean, zero-inflation and dispersion coefficients together with
#' fitted random effects, using a Gaussian (Laplace) approximation to their
#' posterior. Their covariance includes uncertainty from estimating the
#' parameters that control random-effect covariance. For conditional checks,
#' responses are generated using the drawn values of the fitted random effects.
#' Given those values, their prior covariance is not needed to generate responses.
#' Additional family parameters, such as Tweedie power, remain at fitted values.
#' For marginal checks, coefficient draws include the same estimation uncertainty,
#' but new random effects are generated using the fitted prior covariance. Thus
#' these checks keep the covariance of new effects fixed while retaining its
#' estimation uncertainty in the coefficient draws; they do not condition the
#' coefficient posterior on fixed covariance parameters.
#' sdmTMB draws parameters together from a Gaussian approximation, including
#' fixed coefficients and random fields, and uses sdmTMB's response simulator
#' to generate the complete response. These plots do not require an analytic
#' residual distribution, so native simulated families such as Student-t and
#' beta-binomial are available.
#' SCAM models currently require `scalable = FALSE`; explicit or automatic
#' scalable selection gives an error explaining this restriction.
#' Gaussian `gamm` and `gamm4` models draw fixed and random effects together,
#' keeping covariance parameters at fitted values; marginal checks draw new
#' effects.
#' For gamm fits estimated by penalized quasi-likelihood (PQL), the sampler uses
#' the Gaussian approximation used in fitting and then generates responses from
#' the fitted response family. This is supported only when the Gaussian working
#' residuals are uncorrelated. Correlation in those working residuals does not
#' specify a joint distribution for responses from the fitted family.
#' `gamm4` defaults to `conditioning = "conditional"`, checking the full fitted
#' mixed model while including uncertainty in fitted effects. Use the explicit
#' `"gam_component"` option for the extracted GAM calculations. GJRM draws the final
#' distributional coefficients jointly. These are approximate predictive checks;
#' no refitting is performed.
#'
#' Continuous responses use kernel density estimates with a common bandwidth
#' derived from the observed responses and a shared evaluation grid. Discrete
#' responses show observed empirical probability masses as points, alongside
#' posterior medians and equal-tailed intervals across replicated data sets.
#' Observed points, predicted points and intervals are placed at the actual
#' response values on the horizontal axis. Binomial
#' responses are compared as proportions, including models fitted to success
#' and failure counts. Each fitting observation contributes equally to the
#' empirical distribution. Response sampling follows the fitted family's
#' interpretation of weights; see [posterior_samples()]. Factor-valued binomial
#' observations retain one Bernoulli trial per observation, and native gaulss
#' fitting weights do not change its response precision. Explicit GAM sampling
#' weights override the defaults.
#' Multivariate normal responses are shown in separate facets.
#'
#' Excluded observations are omitted from both observed and replicated data.
#' Responses remain on the scale used in fitting, including transformations in
#' the response formula. Grouped families use separate component facets.
#' Censored responses require a model of how censoring changes the generated
#' observations and are not supported. Unsupported models or samplers produce
#' an error; no alternative simulation method is substituted. Additional sampler limitations are
#' described in [posterior_samples()].
#'
#' @return A ggplot object. Its data contain the observed responses
#'   (`.draw = 0`) and posterior replicates (`.draw > 0`), indexed by `.row`
#'   and `.parameter`. Scalable plots instead retain density or probability
#'   summaries evaluated on the same grid for all draws.
#' @seealso [appraise()], [posterior_samples()]
#' @export
#' @examples
#' load_mgcv()
#' dat <- data_sim("eg1", n = 100, seed = 2)
#' mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
#'   data = dat,
#'   method = "REML"
#' )
#' posterior_predictive_plot(mod, n = 20, seed = 42)
#' appraise(mod, ppcheck = TRUE, n_ppc = 20, seed = 42)
posterior_predictive_plot <- function(
  model,
  n = 50,
  seed = NULL,
  observed_col = "#D55E00",
  replicated_col = "#0072B2",
  replicated_alpha = NULL,
  xlab = NULL,
  ylab = NULL,
  title = "Posterior predictive check",
  subtitle = NULL,
  caption = NULL,
  level = 0.9,
  conditioning = NULL,
  scalable = NULL,
  scalable_threshold = 10000L,
  batch_size = 10L,
  ...
) {
  diagnostic_posterior_count(n)
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
    level <= 0 || level >= 1) {
    stop("'level' must be a number strictly between zero and one.",
      call. = FALSE
    )
  }
  if (!is.null(scalable) && (!is.logical(scalable) || length(scalable) != 1L ||
      is.na(scalable))) stop("'scalable' must be TRUE, FALSE, or NULL.", call. = FALSE)
  for (name in c("scalable_threshold", "batch_size")) {
    value <- get(name)
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value < 1 || value != floor(value)) {
      stop("'", name, "' must be a positive integer.", call. = FALSE)
    }
  }
  dots <- list(...)
  if (!is.null(dots$data) || !is.null(dots$newdata)) {
    stop("Posterior predictive checks use the fitting observations; ",
      "new data are not supported.", call. = FALSE)
  }
  observed <- diagnostic_posterior_observed(model, conditioning = conditioning, ...)
  discrete <- attr(observed, "discrete")
  caption <- caption %||% if (!is.null(attr(observed, "diagnostic_conditioning")))
    paste("Conditioning:", attr(observed, "diagnostic_conditioning")) else NULL
  attr(observed, "diagnostic_conditioning") <- NULL
  attr(observed, "diagnostic_description") <- NULL
  attr(observed, "discrete") <- NULL
  observed <- observed[!is.na(observed$.response), , drop = FALSE]
  if (!nrow(observed) || any(!is.finite(observed$.response))) {
    stop("The model must have finite observed responses.", call. = FALSE)
  }
  parameters <- unique(observed$.parameter)
  discrete <- if (!is.null(names(discrete)) && all(parameters %in% names(discrete))) {
    discrete[parameters]
  } else stats::setNames(rep_len(discrete %||% FALSE, length(parameters)), parameters)
  if (any(table(observed$.parameter)[!discrete[names(table(observed$.parameter))]] < 2L)) {
    stop("Density comparisons require at least two observations per response.", call. = FALSE)
  }
  if (is.null(replicated_alpha)) replicated_alpha <- if (all(discrete)) 0.8 else 0.2
  if (is.null(scalable)) scalable <- nrow(observed) >= scalable_threshold
  prepared <- diagnostic_seed(seed, {
    if (scalable) {
      sampler <- diagnostic_posterior_sampler(model, n = n, seed = seed,
        conditioning = conditioning, ...)
      posterior_predictive_stream(observed, sampler, n, discrete, batch_size)
    } else {
      sims <- diagnostic_posterior_samples(model, n = n, seed = seed,
        conditioning = conditioning, ...)
      sims <- posterior_predictive_validate(sims, observed, seq_len(n))
      data <- dplyr::bind_rows(observed, sims)
      curves <- dplyr::bind_rows(lapply(parameters, function(parameter) {
        posterior_predictive_curves(data[data$.parameter == parameter, ],
          discrete[[parameter]])
      }))
      list(data = data, curves = curves)
    }
  })
  data <- prepared$data
  if (isTRUE(prepared$binned)) attr(data, "bin_breaks") <- prepared$breaks
  curves <- prepared$curves
  replicated <- curves[curves$.draw > 0L, , drop = FALSE]
  observed_curves <- curves[curves$.draw == 0L, , drop = FALSE]
  plt <- ggplot2::ggplot(data, ggplot2::aes(
    x = .data$.response, y = .data$.density, group = .data$.draw
  ))
  if (any(discrete)) {
    discrete_names <- names(discrete)[discrete]
    intervals <- posterior_predictive_intervals(
      replicated[replicated$.parameter %in% discrete_names, ], level)
    plt <- plt + ggplot2::geom_pointrange(
      data = intervals,
      mapping = ggplot2::aes(x = .data$.response, y = .data$.density,
        ymin = .data$.lower, ymax = .data$.upper),
      inherit.aes = FALSE, colour = replicated_col, alpha = replicated_alpha
    ) + ggplot2::geom_point(
      data = observed_curves[observed_curves$.parameter %in% discrete_names, ],
      colour = observed_col)
  }
  if (any(!discrete)) {
    continuous_names <- names(discrete)[!discrete]
    plt <- plt + ggplot2::geom_line(
      data = replicated[replicated$.parameter %in% continuous_names, ],
      colour = replicated_col, alpha = replicated_alpha
    ) + ggplot2::geom_line(
      data = observed_curves[observed_curves$.parameter %in% continuous_names, ],
      colour = observed_col)
  }
  if (length(parameters) > 1L) {
    plt <- plt + ggplot2::facet_wrap(ggplot2::vars(.data$.parameter), scales = "free")
  }
  plt + ggplot2::labs(
    x = if (is.null(xlab)) "Response" else xlab,
    y = if (is.null(ylab)) {
      if (all(discrete)) {
        if (isTRUE(prepared$binned)) "Probability per bin" else "Probability"
      } else if (any(discrete)) "Density / probability" else "Density"
    } else {
      ylab
    },
    title = title, subtitle = subtitle, caption = caption
  )
}

# Summarize only posterior replicates; the observed masses are not draws.
posterior_predictive_intervals <- function(data, level) {
  alpha <- (1 - level) / 2
  data |>
    dplyr::group_by(.data$.parameter, .data$.response) |>
    dplyr::summarise(
      .lower = stats::quantile(.data$.density, probs = alpha, names = FALSE),
      .upper = stats::quantile(.data$.density, probs = 1 - alpha, names = FALSE),
      .density = stats::median(.data$.density),
      .groups = "drop"
    )
}

# All curves for a response share a bandwidth/grid or discrete support.
posterior_predictive_curves <- function(data, discrete) {
  if (discrete) {
    support <- sort(unique(data$.response))
  } else {
    bw <- stats::bw.nrd0(data$.response[data$.draw == 0L])
    limits <- range(data$.response) + c(-3, 3) * bw
  }
  dplyr::bind_rows(lapply(split(data, data$.draw), function(draw) {
    if (discrete) {
      x <- support
      density <- tabulate(match(draw$.response, support),
        nbins = length(support)
      ) / nrow(draw)
    } else {
      estimate <- stats::density(draw$.response,
        bw = bw, n = 512,
        from = limits[1L], to = limits[2L]
      )
      x <- estimate$x
      density <- estimate$y
    }
    tibble::tibble(
      .response = x, .density = density,
      .draw = draw$.draw[1L], .parameter = draw$.parameter[1L]
    )
  }))
}


# Validate a complete draw against the same retained observation identities.
posterior_predictive_validate <- function(sims, observed, draws) {
  if (!".parameter" %in% names(sims)) sims$.parameter <- "response"
  if (!setequal(unique(sims$.parameter), unique(observed$.parameter))) {
    stop("Posterior responses do not match the observed responses.", call. = FALSE)
  }
  sims <- sims[!is.na(match(paste(sims$.parameter, sims$.row),
    paste(observed$.parameter, observed$.row))),
    c(".row", ".draw", ".parameter", ".response"), drop = FALSE]
  sizes <- table(factor(sims$.parameter, levels = unique(observed$.parameter)),
    factor(sims$.draw, levels = draws))
  expected <- table(factor(observed$.parameter, levels = unique(observed$.parameter)))
  if (nrow(sims) != nrow(observed) * length(draws) ||
      any(sizes != as.numeric(expected)) || any(!is.finite(sims$.response)) ||
      anyDuplicated(sims[c(".row", ".draw", ".parameter")]) ||
      !setequal(unique(sims$.draw), draws)) {
    stop("Posterior sampling must produce one finite response per ",
      "fitting observation and draw.", call. = FALSE)
  }
  sims
}

# Two passes retain exact common grids without retaining observation-by-draw
# responses in RAM. Temporary draw files are removed on success or failure.
posterior_predictive_stream <- function(observed, sampler, n, discrete, batch_size) {
  directory <- tempfile("gratia-ppc-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  parameters <- unique(observed$.parameter)
  support <- lapply(parameters, function(p) observed$.response[observed$.parameter == p])
  names(support) <- parameters
  limits <- lapply(support, range)
  binned <- stats::setNames(rep(FALSE, length(parameters)), parameters)
  for (p in parameters) {
    if (discrete[[p]]) {
      support[[p]] <- sort(unique(support[[p]]))
      if (length(support[[p]]) > 512L) {
        binned[[p]] <- TRUE
        support[p] <- list(NULL)
      }
    }
  }
  for (start in seq.int(1L, n, by = batch_size)) {
    for (i in seq.int(start, min(n, start + batch_size - 1L))) {
      draw <- posterior_predictive_validate(sampler(i), observed, i)
      saveRDS(draw, file.path(directory, paste0(i, ".rds")), compress = FALSE)
      for (p in parameters) {
        values <- draw$.response[draw$.parameter == p]
        limits[[p]] <- range(limits[[p]], values)
        if (discrete[[p]] && !binned[[p]]) {
          support[[p]] <- sort(unique(c(support[[p]], values)))
          if (length(support[[p]]) > 512L) {
            binned[[p]] <- TRUE
            support[p] <- list(NULL)
          }
        }
      }
    }
  }
  settings <- lapply(parameters, function(p) {
    if (discrete[[p]]) {
      if (binned[[p]]) {
        breaks <- seq(limits[[p]][1L], limits[[p]][2L], length.out = 513L)
        return(list(support = (breaks[-1L] + breaks[-length(breaks)]) / 2,
          breaks = breaks))
      }
      return(list(support = sort(unique(support[[p]]))))
    }
    bw <- stats::bw.nrd0(support[[p]])
    list(bw = bw, limits = limits[[p]] + c(-3, 3) * bw)
  })
  names(settings) <- parameters
  summarise_draw <- function(draw) {
    dplyr::bind_rows(lapply(parameters, function(p) {
      values <- draw$.response[draw$.parameter == p]
      spec <- settings[[p]]
      if (discrete[[p]]) {
        x <- spec$support
        position <- if (is.null(spec$breaks)) match(values, x) else
          findInterval(values, spec$breaks, all.inside = TRUE, rightmost.closed = TRUE)
        y <- tabulate(position, nbins = length(x)) / length(values)
      } else {
        curve <- stats::density(values, bw = spec$bw, n = 512L,
          from = spec$limits[1L], to = spec$limits[2L])
        x <- curve$x
        y <- curve$y
      }
      tibble::tibble(.response = x, .density = y,
        .draw = draw$.draw[1L], .parameter = p)
    }))
  }
  curves <- dplyr::bind_rows(c(list(summarise_draw(observed)),
    lapply(seq_len(n), function(i)
      summarise_draw(readRDS(file.path(directory, paste0(i, ".rds")))))))
  curves <- curves[order(match(curves$.parameter, parameters), curves$.draw), ]
  list(data = curves, curves = curves, binned = any(binned),
    breaks = lapply(settings[binned], `[[`, "breaks"))
}
