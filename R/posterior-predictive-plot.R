#' Posterior predictive check of the response distribution
#'
#' Compare the observed response distribution with replicated data sets drawn
#' from the approximate posterior predictive distribution of a fitted GAM.
#'
#' @param model a fitted GAM supported by [posterior_samples()], including
#'   compatible `bam`, `scam`, and `gamm4` models.
#' @param n positive integer; number of replicated data sets to draw.
#' @param seed numeric; optional random seed. With an explicit seed, the
#'   caller's random-number state is preserved.
#' @param observed_col,replicated_col colours for the observed and replicated
#'   distributions, respectively.
#' @param replicated_alpha numeric or `NULL`; opacity of replicated
#'   distributions. The default, `NULL`, uses `0.8` for discrete posterior
#'   median points and interval bars, and `0.2` for continuous density curves.
#' @param level numeric; coverage of the equal-tailed posterior predictive
#'   intervals for discrete responses. Must be strictly between zero and one.
#'   Defaults to `0.9`. Ignored for continuous density overlays.
#' @inheritParams observed_fitted_plot
#' @param xlab character or expression; label for the response axis.
#' @param ... arguments passed to [posterior_samples()], such as
#'   `unconditional` or `mvn_method`. Prediction at new data is not supported.
#'
#' @details
#' Each replicated data set includes both coefficient uncertainty and response
#' variation, conditional on the fitting covariates. Other distributional
#' parameters retain the treatment used by [posterior_samples()].
#'
#' Continuous responses use kernel density estimates with a common bandwidth
#' derived from the observed responses and a shared evaluation grid. Discrete
#' responses show observed empirical probability masses as points, alongside
#' posterior medians and equal-tailed intervals across replicated data sets.
#' Observed points, predicted points and intervals are placed at the actual
#' response values on the horizontal axis. Binomial
#' responses are compared as proportions, including models fitted to success
#' and failure counts. Each fitting observation contributes equally to the
#' empirical distribution; prior weights are used by the response sampler.
#' Multivariate normal responses are shown in separate facets.
#'
#' Excluded observations are omitted from both observed and replicated data.
#' Responses remain on the scale used in fitting, including transformations in
#' the response formula. Location-scale families other than multivariate normal
#' and multinomial models, grouped families, and censored responses are not
#' supported. Unsupported models or samplers produce an error; no alternative
#' simulation method is substituted. Existing limitations of
#' [posterior_samples()], including treatment of offsets, also apply.
#'
#' @return A ggplot object. Its data contain the observed responses
#'   (`.draw = 0`) and posterior replicates (`.draw > 0`), indexed by `.row`
#'   and `.parameter`.
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
  observed_col = "black",
  replicated_col = "grey60",
  replicated_alpha = NULL,
  xlab = NULL,
  ylab = NULL,
  title = "Posterior predictive check",
  subtitle = NULL,
  caption = NULL,
  level = 0.9,
  ...
) {
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) ||
    n < 1 || n != floor(n) || n > .Machine$integer.max) {
    stop("'n' must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
    level <= 0 || level >= 1) {
    stop("'level' must be a number strictly between zero and one.",
      call. = FALSE
    )
  }
  if (inherits(model, "gamm4")) model <- gamm4_gam(model)
  if (!inherits(model, c("gam", "scam"))) {
    model_class <- if (inherits(model, "gratia_glmmtmb_diagnostic")) {
      "glmmTMB"
    } else {
      class(model)[[1L]]
    }
    stop("Posterior predictive checks are not available for <",
      model_class, "> models.",
      call. = FALSE
    )
  }
  fam <- family(model)
  ft <- family_type(fam)
  multivariate <- identical(ft, "multivariate_normal")
  if (is_gfam_family(fam) || ft %in% c("cnorm", "cpois", "clog") ||
    (inherits(fam, "general.family") &&
      !ft %in% c("multivariate_normal", "multinom"))) {
    stop("Posterior predictive checks are not available for family <",
      family_name(fam), ">.",
      call. = FALSE
    )
  }
  dots <- list(...)
  if (!is.null(dots$data) || !is.null(dots$newdata)) {
    stop("Posterior predictive checks use the fitting observations; ",
      "new data are not supported.",
      call. = FALSE
    )
  }

  original <- model
  used <- model_used_rows(model)
  y <- used$y
  if (is.null(y)) y <- stats::model.response(stats::model.frame(used))
  if (identical(ft, "binomial")) {
    if (is.matrix(y)) y <- y[, 1L] / rowSums(y)
    if (is.factor(y)) y <- as.numeric(y != levels(y)[1L])
  }
  if (!is.numeric(y) || (!multivariate && !is.null(dim(y)))) {
    stop("The model must have a numeric response on the sampling scale.",
      call. = FALSE
    )
  }
  y <- restore_model_rows(y, original)
  parameters <- if (multivariate) {
    paste0("response", seq_len(NCOL(y)))
  } else {
    "response"
  }
  observed <- tibble::tibble(
    .row = rep(seq_len(NROW(y)), times = length(parameters)),
    .draw = 0L,
    .parameter = rep(parameters, each = NROW(y)),
    .response = as.vector(y)
  )
  # Use the same retained fitting observations in every draw and response.
  observed <- observed[!is.na(observed$.response), , drop = FALSE]
  if (!nrow(observed) || any(!is.finite(observed$.response))) {
    stop("The model must have finite observed responses.", call. = FALSE)
  }
  discrete <- ft %in% c(
    "poisson", "negative_binomial", "binomial", "multinom"
  ) ||
    grepl("^ordered_categorical|^zero_inflated_poisson", ft)
  if (is.null(replicated_alpha)) {
    replicated_alpha <- if (discrete) 0.8 else 0.2
  }
  if (!discrete && any(table(observed$.parameter) < 2L)) {
    stop("Density comparisons require at least two observations per response.",
      call. = FALSE
    )
  }

  sample_responses <- function() {
    posterior_samples(model, n = n, seed = seed, ...)
  }
  # Also preserve the absence of .Random.seed in a fresh R session.
  sims <- if (is.null(seed)) {
    sample_responses()
  } else {
    withr::with_preserve_seed(sample_responses())
  }
  if (!".parameter" %in% names(sims)) sims$.parameter <- "response"
  if (!setequal(unique(sims$.parameter), parameters)) {
    stop("Posterior responses do not match the observed responses.",
      call. = FALSE
    )
  }
  sims <- sims[!is.na(match(
    paste(sims$.parameter, sims$.row),
    paste(observed$.parameter, observed$.row)
  )), c(".row", ".draw", ".parameter", ".response"), drop = FALSE]
  sizes <- table(
    factor(sims$.parameter, levels = parameters),
    factor(sims$.draw, levels = seq_len(n))
  )
  observed_sizes <- table(factor(observed$.parameter, levels = parameters))
  if (nrow(sims) != nrow(observed) * n ||
    any(sizes != as.numeric(observed_sizes)) ||
    any(!is.finite(sims$.response)) ||
    anyDuplicated(sims[c(".row", ".draw", ".parameter")]) ||
    !setequal(unique(sims$.draw), seq_len(n))) {
    stop("Posterior sampling must produce one finite response per ",
      "fitting observation and draw.",
      call. = FALSE
    )
  }
  data <- dplyr::bind_rows(observed, sims)
  curves <- dplyr::bind_rows(lapply(parameters, function(parameter) {
    posterior_predictive_curves(data[data$.parameter == parameter, ], discrete)
  }))
  replicated <- curves[curves$.draw > 0L, , drop = FALSE]
  observed_curves <- curves[curves$.draw == 0L, , drop = FALSE]
  plt <- ggplot2::ggplot(data, ggplot2::aes(
    x = .data$.response, y = .data$.density, group = .data$.draw
  ))
  if (discrete) {
    intervals <- posterior_predictive_intervals(replicated, level)
    plt <- plt +
      ggplot2::geom_pointrange(
        data = intervals,
        mapping = ggplot2::aes(x = .data$.response, y = .data$.density,
          ymin = .data$.lower, ymax = .data$.upper),
        inherit.aes = FALSE,
        colour = replicated_col, alpha = replicated_alpha
      ) +
      ggplot2::geom_point(data = observed_curves, colour = observed_col)
  } else {
    plt <- plt +
      ggplot2::geom_line(data = replicated, colour = replicated_col,
        alpha = replicated_alpha) +
      ggplot2::geom_line(data = observed_curves, colour = observed_col)
  }
  if (multivariate) {
    plt <- plt + ggplot2::facet_wrap(
      ggplot2::vars(.data$.parameter),
      scales = "free"
    )
  }
  plt + ggplot2::labs(
    x = if (is.null(xlab)) "Response" else xlab,
    y = if (is.null(ylab)) {
      if (discrete) "Probability" else "Density"
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
