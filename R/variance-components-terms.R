# Use native term predictions so multi-column formula terms, by variables and
# complete smooths retain the model's fitted term definitions.
variance_comp_terms <- function(gam, random = NULL, coverage = 0.95,
    n_draws = 1000L, seed = NULL, unconditional = FALSE, mixed = NULL,
    include_total = TRUE) {
  if (!is.numeric(coverage) || length(coverage) != 1L ||
      !is.finite(coverage) || coverage <= 0 || coverage >= 1) {
    stop("'coverage' must be between zero and one.", call. = FALSE)
  }
  n_draws <- sampling_count(n_draws, "n_draws", zero = TRUE)
  if (n_draws == 1L) {
    stop("'n_draws' must be zero or an integer of at least two.", call. = FALSE)
  }
  if (!is.logical(unconditional) || length(unconditional) != 1L ||
      is.na(unconditional)) {
    stop("'unconditional' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(include_total) || length(include_total) != 1L ||
      is.na(include_total)) {
    stop("'include_total' must be TRUE or FALSE.", call. = FALSE)
  }
  gam <- model_used_rows(gam)
  values <- predict_model(gam, type = "terms", se.fit = FALSE)
  if (!is.matrix(values) || nrow(values) != nrow(gam$model)) {
    stop("Fitted term contributions are not aligned with the fitting rows.",
      call. = FALSE)
  }
  if (!is.null(random)) {
    if (nrow(random) != nrow(values)) {
      stop("Random-effect contributions are not aligned with the GAM terms.",
        call. = FALSE)
    }
    values <- cbind(values, random)
  }
  if (nrow(values) < 2L) {
    stop("Term variances require at least two fitting observations.", call. = FALSE)
  }
  if (any(!is.finite(values))) {
    stop("Fitted term contributions must be finite on all fitting rows.",
      call. = FALSE)
  }
  n_terms <- ncol(values)
  columns <- variance_terms_columns(gam)
  totals <- variance_terms_total_groups(gam, columns, n_terms)
  total_labels <- c("total", if (length(totals) > 1L) {
    paste0("total.", seq_len(length(totals) - 1L))
  })
  # Reserve the total labels so a formula term named total remains distinct.
  term_labels <- utils::tail(make.unique(c(total_labels, colnames(values))), n_terms)
  labels <- c(term_labels, total_labels)
  covariance <- if (n_terms) stats::cov(values) else matrix(0, 0, 0)
  dimnames(covariance) <- list(term_labels, term_labels)
  total_variance <- vapply(totals, function(i) {
    stats::var(rowSums(values[, i, drop = FALSE]))
  }, numeric(1L))
  variance <- c(unname(diag(covariance)), unname(total_variance))
  tbl <- tibble::tibble(.component = labels, .variance = variance,
    .std_dev = sqrt(variance), .lower_ci = rep(NA_real_, length(variance)),
    .upper_ci = rep(NA_real_, length(variance)))
  proportions <- matrix(NA_real_, length(labels), length(totals))
  for (k in seq_along(totals)) {
    i <- totals[[k]]
    if (length(i)) {
      numerator <- as.numeric(stats::cov(values[, i, drop = FALSE],
        rowSums(values[, i, drop = FALSE])))
      proportions[i, k] <- variance_terms_proportion(numerator, total_variance[k])
    }
    proportions[n_terms + k, k] <- variance_terms_proportion(total_variance[k],
      total_variance[k])
  }
  suffixes <- sub("total", "", total_labels, fixed = TRUE)
  for (k in seq_along(totals)) {
    tbl[[paste0(".proportion", suffixes[k])]] <- proportions[, k]
    tbl[[paste0(".proportion_lower_ci", suffixes[k])]] <- NA_real_
    tbl[[paste0(".proportion_upper_ci", suffixes[k])]] <- NA_real_
  }
  class(tbl) <- c("variance_comp", class(tbl))
  attr(tbl, "covariance") <- covariance
  attr(tbl, "term_groups") <- stats::setNames(lapply(totals, function(i) {
    term_labels[i]
  }), total_labels)
  if (n_draws > 0L) {
    draws <- matrix(0, n_draws, length(labels), dimnames = list(NULL, labels))
    proportion_draws <- stats::setNames(lapply(totals, function(i) {
      matrix(NA_real_, n_draws, length(labels), dimnames = list(NULL, labels))
    }), total_labels)
    if (n_terms) {
      if (!is.null(seed)) withr::local_seed(seed)
      X <- predict_model(gam, type = "lpmatrix")
      coefficients <- gaussian_draws(gam, n_draws = n_draws,
        unconditional = unconditional, mvn_method = "mgcv")
      delta <- t(sweep(coefficients, 2L, stats::coef(gam), `-`))
      designs <- lapply(columns, function(i) X[, i, drop = FALSE])
      perturbations <- lapply(columns, function(i) delta[i, , drop = FALSE])
      if (!is.null(random)) {
        extra <- variance_terms_random_draws(mixed, gam, X, delta)
        designs <- c(designs, extra$designs)
        perturbations <- c(perturbations, extra$perturbations)
      }
      if (length(designs) != n_terms) {
        stop("Posterior designs do not match the fitted terms.", call. = FALSE)
      }
      membership <- lapply(seq_len(n_terms), function(j) {
        which(vapply(totals, function(i) j %in% i, logical(1L)))
      })
      # Limit intermediate prediction matrices to 100 draws, including for bam().
      batches <- split(seq_len(n_draws), ceiling(seq_len(n_draws) / 100L))
      for (batch in batches) {
        summed <- lapply(totals, function(i) matrix(0, nrow(values), length(batch)))
        for (j in seq_len(n_terms)) {
          contribution <- variance_terms_centered_draws(designs[[j]],
            perturbations[[j]], values[, j], batch)
          draws[batch, j] <- colSums(contribution^2) / (nrow(values) - 1L)
          for (k in membership[[j]]) summed[[k]] <- summed[[k]] + contribution
        }
        for (k in seq_along(totals)) {
          draws[batch, n_terms + k] <- colSums(summed[[k]]^2) / (nrow(values) - 1L)
          proportion_draws[[k]][batch, n_terms + k] <- variance_terms_proportion(
            draws[batch, n_terms + k], draws[batch, n_terms + k])
        }
        # Re-evaluate this batch rather than retaining predictions for all terms.
        # No new coefficients are drawn: both passes use the same joint draws.
        for (j in seq_len(n_terms)) {
          contribution <- variance_terms_centered_draws(designs[[j]],
            perturbations[[j]], values[, j], batch)
          for (k in membership[[j]]) {
            numerator <- colSums(contribution * summed[[k]]) / (nrow(values) - 1L)
            proportion_draws[[k]][batch, j] <- variance_terms_proportion(numerator,
              draws[batch, n_terms + k])
          }
        }
      }
    }
    bounds <- vapply(seq_along(labels), function(j) {
      stats::quantile(sqrt(draws[, j]), c((1 - coverage) / 2,
        (1 + coverage) / 2), names = FALSE)
    }, numeric(2L))
    tbl$.lower_ci <- bounds[1L, ]
    tbl$.upper_ci <- bounds[2L, ]
    attr(tbl, "variance_draws") <- draws
    for (k in seq_along(totals)) {
      bounds <- vapply(seq_along(labels), function(j) {
        x <- proportion_draws[[k]][, j]
        if (all(is.na(x))) return(c(NA_real_, NA_real_))
        stats::quantile(x, c((1 - coverage) / 2, (1 + coverage) / 2),
          names = FALSE, na.rm = TRUE)
      }, numeric(2L))
      tbl[[paste0(".proportion_lower_ci", suffixes[k])]] <- bounds[1L, ]
      tbl[[paste0(".proportion_upper_ci", suffixes[k])]] <- bounds[2L, ]
    }
    attr(tbl, "proportion_draws") <- proportion_draws
    attr(tbl, "coverage") <- coverage
  }
  if (!include_total) {
    keep <- seq_len(n_terms)
    tbl <- tbl[keep, ]
    if (n_draws > 0L) {
      attr(tbl, "variance_draws") <- draws[, keep, drop = FALSE]
      attr(tbl, "proportion_draws") <- lapply(proportion_draws, function(x) {
        x[, keep, drop = FALSE]
      })
    }
  }
  tbl
}

variance_terms_proportion <- function(numerator, total) {
  value <- numerator / total
  value[!is.finite(value) | !is.finite(total) | total <= 0] <- NA_real_
  value
}

variance_terms_centered_draws <- function(design, perturbation, fitted, batch) {
  contribution <- as.matrix(design %*% perturbation[, batch, drop = FALSE]) + fitted
  sweep(contribution, 2L, colMeans(contribution), `-`)
}

# A distributional GAM has a separate total for each linear predictor. Shared
# terms enter each predictor that uses their coefficients.
variance_terms_total_groups <- function(gam, columns, n_terms) {
  if (!is.list(gam$pterms)) return(list(seq_len(n_terms)))
  predictors <- attr(gam$formula, "lpi")
  if (!length(predictors)) {
    stop("Cannot match GAM terms to their linear predictors.", call. = FALSE)
  }
  lapply(predictors, function(lp) {
    which(vapply(columns, function(i) any(i %in% lp), logical(1L)))
  })
}

# Match the coefficient blocks used by native type = "terms" prediction.
variance_terms_columns <- function(gam) {
  multiple <- is.list(gam$pterms)
  pterms <- if (multiple) gam$pterms else list(gam$pterms)
  assignments <- if (multiple) gam$assign else list(gam$assign)
  starts <- if (multiple) attr(gam$nsdf, "pstart") else 1L
  columns <- list()
  for (j in seq_along(pterms)) {
    for (i in seq_along(attr(pterms[[j]], "term.labels"))) {
      columns[[length(columns) + 1L]] <- which(assignments[[j]] == i) +
        starts[j] - 1L
    }
  }
  c(columns, lapply(gam$smooth, function(sm) seq.int(sm$first.para, sm$last.para)))
}

# The GAM covariance marginalises over additional random effects. Given a GAM
# coefficient draw, draw these effects from their conditional Gaussian law.
# B is their design in coordinates with an identity prior covariance. This
# also handles zero fitted variances without inverting a singular covariance.
variance_terms_random_draws <- function(object, gam, X, delta) {
  native <- if (inherits(object, "gamm")) {
    variance_terms_lme_design(object, gam)
  } else variance_terms_mer_design(object, gam)
  Z <- native$design
  root <- native$root
  B <- Z %*% root
  R <- native$precision
  K <- Matrix::forceSymmetric(Matrix::crossprod(B, R %*% B) +
    Matrix::Diagonal(ncol(B)))
  L <- Matrix::expand(Matrix::Cholesky(K, perm = FALSE, LDL = FALSE))$L
  noise <- Matrix::solve(Matrix::t(L), matrix(stats::rnorm(ncol(B) * ncol(delta)),
    nrow = ncol(B)))
  mean <- -Matrix::solve(K, Matrix::crossprod(B, R %*% X) %*% delta)
  random_delta <- root %*% (mean + noise)
  list(designs = lapply(native$columns, function(i) Z[, i, drop = FALSE]),
    perturbations = lapply(native$columns, function(i) random_delta[i, , drop = FALSE]))
}

variance_terms_lme_design <- function(object, gam) {
  model <- object$lme
  data <- lme_fitting_data(model)
  rows <- match(rownames(gam$model), rownames(data))
  smooth_groups <- unlist(gamm_smooth_blocks(object), use.names = FALSE)
  groups <- setdiff(names(model$coefficients$random), smooth_groups)
  predictors <- lme_random_predictors(model, data)
  designs <- roots <- columns <- list()
  count <- 0L
  for (group in groups) {
    b <- predictors[[group]]
    labels <- colnames(model$coefficients$random[[group]])
    z <- b$z[, labels, drop = FALSE]
    g <- match(b$group, unique(b$group))
    k <- ncol(z)
    designs[[length(designs) + 1L]] <- Matrix::sparseMatrix(
      i = rep(seq_len(nrow(z)), k),
      j = as.vector(outer((g - 1L) * k, seq_len(k), `+`)),
      x = as.vector(z), dims = c(nrow(z), max(g) * k))
    covariance <- nlme::pdMatrix(model$modelStruct$reStruct[[group]])
    covariance <- covariance[labels, labels, drop = FALSE] * model$sigma^2
    roots <- c(roots, rep(list(t(chol(covariance))), max(g)))
    columns <- c(columns, lapply(seq_len(k), function(j) {
      count + seq.int(j, max(g) * k, by = k)
    }))
    count <- count + max(g) * k
  }
  error <- lme_response_error(model)
  R <- Matrix::Diagonal(nrow(data), 1 / error$sd^2)
  for (b in error$blocks) R[b$i, b$i] <- chol2inv(t(b$factor))
  list(design = do.call(cbind, designs)[rows, , drop = FALSE],
    root = Matrix::bdiag(roots), columns = columns,
    precision = R[rows, rows, drop = FALSE])
}

variance_terms_mer_design <- function(object, gam) {
  model <- object$mer
  cnms <- lme4::getME(model, "cnms")
  smooth_groups <- unlist(lapply(gam$smooth, `[[`, "lmer.name"), use.names = FALSE)
  groups <- which(!names(cnms) %in% smooth_groups)
  starts <- lme4::getME(model, "Gp")
  indices <- unlist(lapply(groups, function(i) seq.int(starts[i] + 1L,
    starts[i + 1L])), use.names = FALSE)
  columns <- list()
  count <- 0L
  for (i in groups) {
    k <- length(cnms[[i]])
    q <- starts[i + 1L] - starts[i]
    columns <- c(columns, lapply(seq_len(k), function(j) count + seq.int(j, q, by = k)))
    count <- count + q
  }
  rows <- match(rownames(gam$model), rownames(stats::model.frame(model)))
  Z <- Matrix::t(lme4::getME(model, "Zt")[indices, , drop = FALSE])
  root <- lme4::getME(model, "Lambda")[indices, indices, drop = FALSE] * sqrt(gam$sig2)
  list(design = Z[rows, , drop = FALSE], root = root, columns = columns,
    precision = Matrix::Diagonal(nrow(gam$model), gam$weights / gam$sig2))
}

variance_terms_check_dots <- function(...) {
  if (length(list(...))) {
    stop("Additional arguments are not used with 'type = \"terms\"'.",
      call. = FALSE)
  }
}

# Add only the extra random effects supplied through random=. The GAM term
# predictions already contain the fixed and random parts of each smooth.
gamm_fitted_random_terms <- function(object, gam) {
  lme <- object$lme
  smooth_groups <- unlist(gamm_smooth_blocks(object), use.names = FALSE)
  groups <- setdiff(names(lme$coefficients$random), smooth_groups)
  if (!length(groups)) return(NULL)
  data <- lme_fitting_data(lme)
  rows <- match(rownames(gam$model), rownames(data))
  if (anyNA(rows)) {
    stop("GAM and lme fitting rows are not aligned.", call. = FALSE)
  }
  predictors <- lme_random_predictors(lme, data)
  contributions <- lapply(groups, function(group) {
    predictor <- predictors[[group]]
    coefficient <- lme$coefficients$random[[group]]
    columns <- match(colnames(coefficient), colnames(predictor$z))
    levels <- match(predictor$group, rownames(coefficient))
    if (anyNA(columns) || anyNA(levels)) {
      stop("Fitted lme random coefficients do not match their predictors.",
        call. = FALSE)
    }
    values <- predictor$z[, columns, drop = FALSE] *
      coefficient[levels, , drop = FALSE]
    colnames(values) <- paste(group, colnames(coefficient), sep = ":")
    values[rows, , drop = FALSE]
  })
  do.call(cbind, contributions)
}

gamm4_fitted_random_terms <- function(object, gam) {
  mer <- object$mer
  cnms <- lme4::getME(mer, "cnms")
  smooth_groups <- unlist(lapply(gam$smooth, `[[`, "lmer.name"),
    use.names = FALSE)
  groups <- which(!names(cnms) %in% smooth_groups)
  if (!length(groups)) return(NULL)
  frame <- stats::model.frame(mer)
  rows <- match(rownames(gam$model), rownames(frame))
  if (anyNA(rows)) {
    stop("GAM and mer fitting rows are not aligned.", call. = FALSE)
  }
  design <- lme4::getME(mer, "Zt")
  coefficients <- lme4::getME(mer, "b")
  starts <- lme4::getME(mer, "Gp")
  labels <- names(lme4::VarCorr(mer))
  contributions <- lapply(groups, function(i) {
    columns <- cnms[[i]]
    values <- vapply(seq_along(columns), function(j) {
      index <- seq.int(starts[i] + j, starts[i + 1L], by = length(columns))
      as.numeric(Matrix::crossprod(design[index, , drop = FALSE],
        coefficients[index]))
    }, numeric(nrow(frame)))
    colnames(values) <- paste(labels[i], columns, sep = ":")
    values[rows, , drop = FALSE]
  })
  do.call(cbind, contributions)
}
