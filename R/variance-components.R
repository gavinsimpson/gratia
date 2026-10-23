#' Variance components and fitted term variances for GAMs
#'
#' Extract variance components for smooth penalties and random effects, or
#' calculate the variances and covariances of fitted term contributions.
#'
#' @details With `type = "penalty"`, models fitted by [mgcv::gam()] and
#' [mgcv::bam()] have one row per penalty, including penalties supplied through
#' `paraPen`.
#' Fixed smoothing parameters are included, and terms sharing a smoothing
#' parameter each have their own rows. A smooth with several penalties has
#' several rows. Unpenalized parametric terms have no variance parameter and
#' are omitted.
#'
#' The variance component is the fitted scale parameter divided by the
#' smoothing parameter. With `rescale = TRUE`, the smoothing parameter is
#' first divided by the penalty's scaling factor. These components describe
#' the penalty on coefficients, rather than the variation in fitted term
#' values over the observations.
#'
#' Confidence intervals use the Hessian of the likelihood-based smoothness
#' selection criterion, transformed to the log standard deviation scale.
#' Shared smoothing parameters retain their shared uncertainty. A fixed
#' smoothing parameter can still have an interval when the model scale is
#' estimated. The scale is included as a separate row when its uncertainty is
#' available. Unavailable intervals are `NA`.
#'
#' For [mgcv::gamm()] fits, components are extracted from `lme`. Smooth
#' variances are expressed on the same penalty scale as for `gam()` and
#' `bam()`, with one row per penalty. Additional random effects have one row
#' per coefficient variance, labelled by grouping factor and coefficient;
#' their covariances are omitted. Intervals use [nlme::intervals()] when the
#' fitted covariance parameters support this calculation. Bounds are `NA`
#' when that information is unavailable, including fits containing `pdTens`
#' tensor-product blocks. The estimated residual scale is labelled `scale`;
#' a fixed scale is omitted. With residual variance weights, `scale` is the
#' baseline variance before those weights are applied. For models fitted by
#' penalized quasi-likelihood (PQL), these are approximate components of the
#' working mixed model.
#'
#' For [gamm4::gamm4()] fits, components are extracted from `mer`. Smooth
#' components use the same penalty scale and labels as the other methods;
#' additional random effects retain their mixed-model variances. By default,
#' intervals are computed separately for each standard deviation using
#' [stats::profile()] and [stats::confint()]. The profiling uses the fitted
#' random-effects design matrices. Gaussian models are refitted by maximum
#' likelihood for these profiles, including models fitted by REML; the
#' reported point estimates remain those of the original fit. Profiling can
#' take substantially longer than extracting estimates. Use
#' `interval_method = "none"` for estimates without intervals. Failed profiles
#' warn and return `NA` bounds for the affected component. Profiles are
#' unavailable for generalized mixed models with an estimated scale.
#'
#' With `type = "terms"`, each component is a fitted term's contribution to
#' the linear predictor, evaluated over the observations used to fit the model.
#' Parametric formula terms are included, with all coefficients of a factor,
#' polynomial or interaction combined into one term. Each smooth has one row,
#' including smooths with several penalties. Additional random effects in
#' `gamm()` and `gamm4()` contribute one component per grouping factor and
#' coefficient, evaluated at their fitted values. Formula terms follow the
#' model's contrast coding. Duplicated component labels receive numeric suffixes.
#'
#' Term variances and covariances use equal observation weights and the sample
#' denominator, the number of fitting observations minus one. Likelihood
#' weights do not change this averaging. Intercepts, offsets and residual
#' variation are excluded. `rescale` and `interval_method` apply only to
#' `type = "penalty"`; no profiling is performed for term variances.
#'
#' A final row labelled `total` gives the variance of the sum of the term
#' contributions, including additional fitted random effects. It includes
#' covariance between terms: the total is the sum of all entries of their
#' covariance matrix, rather than just its diagonal. Models with several linear
#' predictors have one total per predictor, labelled `total`, `total.1`, and
#' so on in predictor order. A term shared by several predictors contributes
#' to each of their totals. An intercept-only predictor has total variance zero.
#' These labels are reserved; a formula term with the same name receives a
#' numeric suffix so it remains distinct from the total.
#' Use `include_total = FALSE` to omit the total rows and their columns in
#' the posterior draw matrices. Totals are still calculated for proportions;
#' the individual term estimates and intervals are unchanged.
#'
#' Proportions allocate the total variance using
#' \deqn{p_j = \frac{\operatorname{Cov}(f_j, \sum_k f_k)}
#'                  {\operatorname{Var}(\sum_k f_k)}.}
#' Each term receives its own variance plus its covariance with the other
#' terms. This splits each pair's covariance contribution equally between its
#' two terms, so the individual proportions sum to one. Proportions can be
#' negative or greater than one when terms offset each other's variation;
#' they are not restricted to the interval from zero to one.
#'
#' `.proportion` reports these allocations, with equal-tailed posterior bounds
#' in `.proportion_lower_ci` and `.proportion_upper_ci`. These bounds are on the
#' proportion scale. Each posterior proportion uses the term contributions
#' and total from the same joint draw. Individual interval limits need not sum
#' to one. The total row's proportion is one. For additional linear predictors,
#' the columns receive suffixes `.1`, `.2`, and so on, matching the total labels;
#' for example, `.proportion.1` and `.proportion_lower_ci.1`. A shared term has a
#' separate allocation for each predictor. Entries for unrelated terms and
#' totals are `NA`.
#'
#' Proportions are undefined when the total variance is zero and are returned
#' as `NA`, including for an intercept-only predictor. Posterior draws with
#' zero total variance retain `NA` proportions and are omitted when computing
#' proportion intervals; if none are defined, both bounds are `NA`.
#'
#' Term intervals are equal-tailed posterior intervals, computed by drawing the
#' full coefficient vector from its approximate Gaussian posterior and
#' recalculating each term's variance over the same fitting observations.
#' All terms use the same coefficient draw in each replicate, preserving their
#' dependence. Each total is calculated by summing the term contributions
#' within that draw and then computing their variance. Additional fitted random
#' effects in mixed models are drawn
#' conditional on the GAM coefficients, retaining their covariance with the
#' GAM terms and with each other. Smoothing parameters, random-effect covariance
#' parameters and the response scale are held at their fitted values, unless
#' `unconditional = TRUE` supplies a correction for smoothing-parameter
#' uncertainty in the GAM coefficient covariance. PQL and generalized mixed
#' models use the Gaussian approximation at the fitted working model.
#' For SCAM fits, draws are made jointly on the unrestricted fitting scale
#' and transformed before evaluating the terms.
#'
#' Point estimates remain the variances of the fitted contributions. Interval
#' bounds are quantiles of the posterior term standard deviations; square the
#' bounds to obtain intervals for the term variances. Use `seed` for reproducible
#' draws and `n_draws = 0` to skip intervals and return `NA` bounds.
#'
#' The `covariance` attribute contains the covariance matrix of the fitted
#' term contributions, with rows and columns in `.component` order, excluding
#' the total rows. Its diagonal equals `.variance` for the individual terms.
#' For a model with a single linear predictor, the sum of all matrix entries
#' equals the `total` row's `.variance`.
#' Adding only the diagonal omits covariance between terms. This matrix
#' describes variation across observations, rather than estimation uncertainty.
#' When intervals are computed, the `variance_draws` attribute contains a matrix
#' with one row per posterior draw and one column per component, on the variance
#' scale. Rows are paired across terms. Its column names match `.component`;
#' the `coverage` attribute records the requested interval coverage. The total
#' columns include covariance between term contributions within each draw.
#' Summing the individual term variances would omit that covariance.
#' The `proportion_draws` attribute is a named list with one matrix per total
#' (`total`, `total.1`, and so on). Each matrix has the same rows and columns as
#' `variance_draws`, with `NA` entries for unrelated components. Its rows retain
#' the pairing with the coefficient and variance draws. Proportions and their
#' intervals are added only for `type = "terms"`.
#' The `term_groups` attribute lists the component labels belonging to each
#' predictor, named by its total label. Use [draw.variance_comp()] to plot
#' components or proportions with their stored intervals.
#'
#' @param object a model fitted by [mgcv::gam()], [mgcv::bam()], [mgcv::gamm()], or
#'   [gamm4::gamm4()]. See [gamm4_support] for mixed-model variance components.
#' @param rescale logical; for numerical stability reasons the penalty matrices
#'   of smooths are rescaled before fitting. If `rescale = TRUE`, this rescaling
#'   is undone, resulting in variance components that are on their original
#'   scale. This is needed if comparing with other mixed model software, such as
#'   `lmer()`.
#' @param coverage numeric; a value between 0 and 1 indicating the (approximate)
#'   coverage of the confidence interval that is returned.
#' @param interval_method character; for `gamm4()` fits, `"profile"` computes
#'   likelihood profile intervals and `"none"` returns estimates only.
#' @param type character; `"penalty"` (the default) returns variance parameters
#'   associated with penalties and random effects. `"terms"` returns sample
#'   variances of fitted term contributions and their covariance matrix.
#' @param n_draws integer; number of joint posterior coefficient draws for
#'   `type = "terms"`. Defaults to 1000. Use zero for point estimates only;
#'   otherwise at least two draws are required. Ignored for `type = "penalty"`.
#' @param seed optional random seed for term intervals. Supplying a seed
#'   preserves the caller's random-number state.
#' @param unconditional logical; include the available correction for
#'   smoothing-parameter uncertainty in term intervals. Defaults to `FALSE`.
#'   If the corrected covariance is unavailable, a warning is issued and the
#'   uncorrected covariance is used. SCAM fits require `FALSE`.
#' @param include_total logical; include total rows for `type = "terms"`?
#'   Defaults to `TRUE`. Ignored for `type = "penalty"`.
#' @param ... arguments passed to other methods. For `gamm4()` fits, additional
#'   arguments are passed to [stats::profile()], for example `maxpts`. With
#'   `type = "terms"`, no additional arguments are accepted.
#' @return A tibble of class `variance_comp`, with `.component`, `.variance`,
#'   `.std_dev`, `.lower_ci` and `.upper_ci`. Both confidence limits are on
#'   the standard deviation scale. For `type = "terms"`, obtain the named
#'   covariance matrix with `attr(result, "covariance")` and posterior variance
#'   draws with `attr(result, "variance_draws")` when intervals were computed.
#'   Term results also include proportion estimates and their interval columns;
#'   `attr(result, "proportion_draws")` contains the paired posterior proportions.
#' @seealso [mgcv::gam.vcomp()], [draw.variance_comp()]
#'
#' @export
`variance_comp` <- function(object, ...) {
  UseMethod("variance_comp")
}

#' @export
#' @rdname variance_comp
#'
#' @importFrom stats qnorm
`variance_comp.gam` <- function(object, rescale = TRUE, coverage = 0.95,
    type = c("penalty", "terms"), n_draws = 1000L, seed = NULL,
    unconditional = FALSE, include_total = TRUE, ...) {
  type <- match.arg(type)
  if (type == "terms") {
    variance_terms_check_dots(...)
    return(variance_comp_terms(object, coverage = coverage, n_draws = n_draws,
      seed = seed, unconditional = unconditional, include_total = include_total))
  }
  if (!is.numeric(coverage) || length(coverage) != 1L ||
      !is.finite(coverage) || coverage <= 0 || coverage >= 1) {
    stop("'coverage' must be between zero and one.", call. = FALSE)
  }
  if (!is.logical(rescale) || length(rescale) != 1L || is.na(rescale)) {
    stop("'rescale' must be TRUE or FALSE.", call. = FALSE)
  }
  penalties <- gam_variance_penalties(object)
  scale <- object$reml.scale
  if (is.null(scale) || !is.finite(scale)) scale <- object$sig2
  variance <- scale / penalties$sp
  if (rescale) variance <- variance * penalties$scaling
  intervals <- gam_variance_intervals(object, length(variance))
  component <- penalties$component
  if (intervals$scale) {
    variance <- c(variance, scale)
    component <- c(component, "scale")
  }
  std_dev <- sqrt(variance)
  width <- qnorm((1 + coverage) / 2) * intervals$se
  tbl <- tibble::tibble(
    .component = component, .variance = unname(variance),
    .std_dev = unname(std_dev),
    .lower_ci = unname(exp(log(std_dev) - width)),
    .upper_ci = unname(exp(log(std_dev) + width))
  )
  class(tbl) <- c("variance_comp", class(tbl))
  tbl
}

# full.sp follows the penalty order, with parametric penalties first. Unlike
# sp, it also contains fixed values and repeats parameters shared by smooths.
gam_variance_penalties <- function(object) {
  sp <- object$full.sp %||% object$sp
  scaling <- unlist(lapply(object$smooth, function(sm) {
    if (length(sm$S.scale) != length(sm$S)) {
      stop("Penalty scaling factors do not match the smooth penalties.",
        call. = FALSE)
    }
    sm$S.scale
  }), use.names = FALSE)
  n_parametric <- length(sp) - length(scaling)
  if (n_parametric < 0L) {
    stop("Smoothing parameters do not match the fitted penalties.",
      call. = FALSE)
  }
  component <- names(sp)
  if (is.null(component)) component <- names(object$sp)
  if (length(component) != length(sp)) {
    stop("Component names do not match the fitted penalties.", call. = FALSE)
  }
  list(sp = as.numeric(sp), component = component %||% character(),
    scaling = c(rep(1, n_parametric), scaling))
}

# The Hessian is for estimated log parameters. V.sp's L attribute maps these
# to all log penalties, including fixed rows and shared or paraPen mappings.
# Use the Hessian directly to retain gam.vcomp's interval calculation.
gam_variance_intervals <- function(object, n_penalties) {
  unavailable <- list(scale = FALSE, se = rep(NA_real_, n_penalties))
  H <- object$outer.info$hess
  if (!n_penalties || !object$method %in%
      c("ML", "P-ML", "REML", "P-REML", "fREML") || is.null(H)) {
    return(unavailable)
  }
  n_theta <- object$family$n.theta %||% 0L
  if ("perf" %in% object$optimizer) n_theta <- 0L
  if (n_theta >= ncol(H)) return(unavailable)
  keep <- seq.int(n_theta + 1L, ncol(H))
  H <- H[keep, keep, drop = FALSE]
  n_sp <- length(object$sp)
  scale_estimated <- ncol(H) > n_sp
  L <- attr(object$V.sp, "L")
  if (is.null(L)) {
    if (n_penalties != n_sp) return(unavailable)
    L <- diag(ncol(H))
  } else {
    if (n_theta) {
      L <- L[-seq_len(n_theta), keep, drop = FALSE]
    }
    # bam stores only the penalty mapping; gam includes the scale row and
    # column. Add the scale coordinate when it is absent from bam's mapping.
    if (scale_estimated && ncol(L) == n_sp) {
      L <- rbind(cbind(L, 0), c(rep(0, n_sp), 1))
    }
  }
  J <- -0.5 * L[seq_len(n_penalties), , drop = FALSE]
  if (scale_estimated) {
    scale_row <- L[n_penalties + 1L, , drop = FALSE]
    J <- rbind(sweep(J, 2L, as.numeric(0.5 * scale_row), "+"),
      0.5 * scale_row)
  }
  eig <- eigen(H, symmetric = TRUE)
  inverse <- numeric(length(eig$values))
  positive <- eig$values > 0
  inverse[positive] <- 1 / eig$values[positive]
  V <- eig$vectors %*% (inverse * t(eig$vectors))
  se <- sqrt(pmax(0, rowSums((J %*% V) * J)))
  # A component fixed through both its penalty and scale has no interval.
  se[rowSums(abs(J)) == 0] <- NA_real_
  list(scale = scale_estimated, se = se)
}

#' @export
#' @rdname variance_comp
variance_comp.gamm <- function(object, rescale = TRUE, coverage = 0.95,
    type = c("penalty", "terms"), n_draws = 1000L, seed = NULL,
    unconditional = FALSE, include_total = TRUE, ...) {
  if (!is.list(object) || !inherits(object$gam, "gam") ||
      !inherits(object$lme, "lme")) {
    stop("Expected a gamm model with valid 'gam' and 'lme' components.",
      call. = FALSE)
  }
  type <- match.arg(type)
  if (type == "terms") {
    variance_terms_check_dots(...)
    gam <- model_used_rows(object$gam)
    return(variance_comp_terms(gam, gamm_fitted_random_terms(object, gam),
      coverage = coverage, n_draws = n_draws, seed = seed,
      unconditional = unconditional, mixed = object, include_total = include_total))
  }
  if (!is.numeric(coverage) || length(coverage) != 1L ||
      !is.finite(coverage) || coverage <= 0 || coverage >= 1) {
    stop("'coverage' must be between zero and one.", call. = FALSE)
  }
  if (!is.logical(rescale) || length(rescale) != 1L || is.na(rescale)) {
    stop("'rescale' must be TRUE or FALSE.", call. = FALSE)
  }
  lme <- object$lme
  rs <- lme$modelStruct$reStruct
  intervals <- gamm_variance_intervals(lme, coverage)
  groups <- names(lme$coefficients$random)
  smooth_blocks <- gamm_smooth_blocks(object)
  smooth_groups <- character()
  rows <- list()
  for (i in seq_along(object$gam$smooth)) {
    sm <- object$gam$smooth[[i]]
    n <- length(sm$S)
    if (!n) next
    blocks <- smooth_blocks[[i]]
    smooth_groups <- c(smooth_groups, blocks)
    values <- lapply(blocks, function(group) {
      block <- rs[[group]]
      value <- unname(stats::coef(block, unconstrained = FALSE))
      bounds <- intervals$reStruct[[group]]
      if (inherits(block, "pdTens")) {
        value <- 1 / value
      } else if (!inherits(block, "pdIdnot")) {
        stop("Unsupported lme covariance class for a smooth.", call. = FALSE)
      }
      list(variance = lme$sigma^2 * value,
        lower = if (is.null(bounds)) rep(NA_real_, length(value)) else
          sqrt(bounds[, "lower"]),
        upper = if (is.null(bounds)) rep(NA_real_, length(value)) else
          sqrt(bounds[, "upper"]))
    })
    variance <- unlist(lapply(values, `[[`, "variance"), use.names = FALSE)
    if (length(variance) != n || length(sm$S.scale) != n) {
      stop("Smooth penalties do not match the fitted lme blocks.",
        call. = FALSE)
    }
    scaling <- if (rescale) sm$S.scale else rep(1, n)
    labels <- names(object$gam$sp)[seq.int(sm$first.sp, sm$last.sp)]
    rows[[length(rows) + 1L]] <- tibble::tibble(
      .component = labels, .variance = variance * scaling,
      .std_dev = sqrt(variance * scaling),
      .lower_ci = unlist(lapply(values, `[[`, "lower"), use.names = FALSE) *
        sqrt(scaling),
      .upper_ci = unlist(lapply(values, `[[`, "upper"), use.names = FALSE) *
        sqrt(scaling)
    )
  }
  for (group in setdiff(groups, smooth_groups)) {
    block <- rs[[group]]
    covariance <- nlme::pdMatrix(block) * lme$sigma^2
    bounds <- gamm_random_intervals(block, intervals$reStruct[[group]])
    rows[[length(rows) + 1L]] <- tibble::tibble(
      .component = paste(group, colnames(covariance), sep = ":"),
      .variance = unname(diag(covariance)),
      .std_dev = sqrt(unname(diag(covariance))),
      .lower_ci = bounds[, 1L], .upper_ci = bounds[, 2L]
    )
  }
  if (!isTRUE(attr(lme$modelStruct, "fixedSigma"))) {
    bounds <- intervals$sigma
    rows[[length(rows) + 1L]] <- tibble::tibble(
      .component = "scale", .variance = lme$sigma^2, .std_dev = lme$sigma,
      .lower_ci = if (is.null(bounds)) NA_real_ else unname(bounds["lower"]),
      .upper_ci = if (is.null(bounds)) NA_real_ else unname(bounds["upper"])
    )
  }
  tbl <- dplyr::bind_rows(rows)
  class(tbl) <- c("variance_comp", class(tbl))
  tbl
}

# rind indexes the flattened fitted random coefficients. It identifies the
# blocks even when gamm reorders smooths or creates several blocks per smooth.
gamm_smooth_blocks <- function(object) {
  groups <- names(object$lme$coefficients$random)
  ends <- c(0, cumsum(lengths(object$lme$coefficients$random)))
  lapply(object$gam$smooth, function(sm) {
    if (!length(sm$S)) return(character())
    index <- findInterval(sm$rind - 1L, ends)
    if (!length(index) || any(index < 1L | index > length(groups))) {
      stop("Smooth coefficients do not match the fitted lme blocks.",
        call. = FALSE)
    }
    groups[unique(index)]
  })
}

gamm_variance_intervals <- function(lme, coverage) {
  # pdTens represents a precision, whereas nlme's interval calculation treats
  # pdMat blocks as covariance matrices. Its intervals cannot be used here.
  if (!is.matrix(lme$apVar) || any(vapply(lme$modelStruct$reStruct,
      inherits, logical(1), "pdTens"))) {
    return(NULL)
  }
  tryCatch(nlme::intervals(lme, level = coverage, which = "var-cov"),
    error = function(e) NULL)
}

gamm_random_intervals <- function(block, intervals) {
  n <- nrow(nlme::pdMatrix(block))
  if (is.null(intervals)) return(matrix(NA_real_, n, 2L))
  intervals <- as.matrix(intervals)
  if (inherits(block, "pdBlocked")) {
    start <- 0L
    bounds <- vector("list", length(block))
    for (i in seq_along(block)) {
      child <- block[[i]]
      index <- start + seq_along(stats::coef(child))
      start <- max(index)
      bounds[[i]] <- gamm_random_intervals(child,
        intervals[index, , drop = FALSE])
    }
    return(do.call(rbind, bounds))
  }
  if (inherits(block, "pdIdnot")) {
    bounds <- sqrt(intervals[1L, c("lower", "upper")])
    return(matrix(rep(bounds, each = n), n, 2L))
  }
  if (inherits(block, c("pdIdent", "pdCompSymm"))) {
    bounds <- intervals[1L, c("lower", "upper")]
    return(matrix(rep(bounds, each = n), n, 2L))
  }
  labels <- nlme::Names(block)
  index <- match(paste0("sd(", labels, ")"), rownames(intervals))
  bounds <- intervals[index, c("lower", "upper"), drop = FALSE]
  variance <- match(paste0("var(", labels, ")"), rownames(intervals))
  take <- is.na(index) & !is.na(variance)
  bounds[take, ] <- sqrt(intervals[variance[take], c("lower", "upper"),
    drop = FALSE])
  unname(bounds)
}
