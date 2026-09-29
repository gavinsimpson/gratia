# Keep mgcv's term estimates, but optionally let each term carry its own
# linear predictor's intercept uncertainty. Use the fitted assignments rather
# than coefficient names so contrasts and multi-column terms remain intact.
predict_parametric_effects <- function(object, newdata, terms,
    unconditional, overall_uncertainty) {
  # Retain mgcv's partial estimates and its term-only SEs as the fallback.
  # Only se.fit is replaced below: adding the intercept to fit would change
  # the estimand from a term contribution to an intercept-plus-term prediction.
  pred <- predict_model(object, newdata = newdata, type = "terms",
    terms = terms, se.fit = TRUE, unconditional = unconditional)
  if (!isTRUE(overall_uncertainty)) return(pred)

  # mgcv stores one terms object and coefficient assignment vector per linear
  # predictor. Assignment 0 identifies the intercept; positive integers index
  # formula terms, including all columns of factors, polynomials and interactions.
  multiple <- is.list(object$pterms)
  pterms <- if (multiple) object$pterms else list(object$pterms)
  assignments <- if (multiple) object$assign else list(object$assign)
  if (!any(vapply(assignments, function(x) any(x == 0L), logical(1)))) {
    return(pred)
  }
  # Assignments use positions within each predictor's parametric block. pstart
  # gives the block's first column in the full lpmatrix and covariance matrix.
  starts <- if (multiple) attr(object$nsdf, "pstart") else 1L
  X <- predict_model(object, newdata = newdata, type = "lpmatrix")
  # predict_model() has already warned if the corrected covariance is absent.
  # Match its fallback to Vp without issuing the same warning a second time.
  V <- get_vcov(object,
    unconditional = isTRUE(unconditional) && !is.null(object$Vc))
  for (j in seq_along(pterms)) {
    assign <- assignments[[j]]
    intercept <- which(assign == 0L)
    # Another predictor may have an intercept even when this one does not.
    # Its intercept must not contribute to this predictor's uncertainty.
    if (!length(intercept)) next
    labels <- attr(pterms[[j]], "term.labels")
    # Match mgcv's prediction column names: the second predictor uses ".1", etc.
    if (j > 1L) labels <- paste0(labels, ".", j - 1L)
    for (i in seq_along(labels)) {
      term <- labels[i]
      if (!term %in% colnames(pred$se.fit)) next
      columns <- c(intercept, which(assign == i)) + starts[j] - 1L
      Xi <- X[, columns, drop = FALSE]
      # Compute diag(Xi V Xi') without constructing the full row-by-row matrix.
      # Keeping the intercept and term in the same block includes their covariance:
      # Var(beta0 + f_term) = Var(beta0) + Var(f_term) + 2 Cov(beta0, f_term).
      # Other terms are excluded; unlike smooth evaluation, we do not use cmX
      # to include uncertainty in other model components at their column means.
      variance <- rowSums((Xi %*% V[columns, columns, drop = FALSE]) * Xi)
      # Guard against tiny negative variances caused by floating-point rounding.
      pred$se.fit[, term] <- sqrt(pmax(0, variance))
    }
  }
  pred
}
