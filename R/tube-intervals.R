# Continuous-domain calibration of a linear Gaussian uncertainty process.
# Keep this separate from model preparation and from coefficient simulation.
tube_controls <- function(control = list()) {
  defaults <- list(
    rel_tol = 1e-6, abs_tol = 1e-8,
    subdivisions = 1000L, root_tol = 1e-8
  )
  if (!is.list(control) || (length(control) &&
    (is.null(names(control)) || anyDuplicated(names(control)) ||
      any(!names(control) %in% names(defaults))))) {
    cli::cli_abort("{.arg tube_control} must be a named list of rel_tol, abs_tol, subdivisions, or root_tol.")
  }
  defaults[names(control)] <- control
  for (nm in names(defaults)) {
    x <- defaults[[nm]]
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0 ||
      (nm == "subdivisions" && (x != floor(x) || x > .Machine$integer.max))) {
      cli::cli_abort("Tube control {.val {nm}} must be a single positive finite number (an integer for subdivisions).")
    }
  }
  defaults
}

resolve_simultaneous_method <- function(method, interval, domain, control) {
  method <- match.arg(method, c("simulation", "tube"))
  if (method == "tube" && !identical(interval, "simultaneous")) {
    cli::cli_abort("Tube calibration requires simultaneous intervals.")
  }
  if (method == "simulation" && (!is.null(domain) || length(control))) {
    cli::cli_abort("{.arg simultaneous_domain} and {.arg tube_control} require simultaneous_method = 'tube'.")
  }
  if (method == "tube") tube_controls(control)
  method
}

tube_check_model <- function(model, method = "gaussian") {
  if (method != "gaussian" || inherits(model, "scam") || is_scasm_model(model)) {
    cli::cli_abort("Tube bands require unconstrained Gaussian coefficient uncertainty; use simulation for this model or interval source.")
  }
  invisible(NULL)
}

tube_domain <- function(domain, coordinate, x) {
  supplied <- !is.null(domain)
  if (is.list(domain)) {
    if (is.null(names(domain)) || anyDuplicated(names(domain)) ||
      !coordinate %in% names(domain)) {
      cli::cli_abort("Supply a simultaneous domain for coordinate {.val {coordinate}}.")
    }
    domain <- domain[[coordinate]]
  }
  if (!supplied) domain <- range(x, na.rm = TRUE)
  if (!is.numeric(domain) || length(domain) != 2L ||
    any(!is.finite(domain)) || domain[1L] >= domain[2L]) {
    cli::cli_abort("The simultaneous domain must contain two increasing finite endpoints; supply it explicitly for a single evaluation point.")
  }
  if (any(!is.finite(x)) || any(x < domain[1L] | x > domain[2L])) {
    cli::cli_abort("All tube evaluation points must be finite and inside the simultaneous domain.")
  }
  list(interval = unname(domain), source = if (supplied) "supplied" else "evaluation range")
}

# Obtain derivatives in the fitted basis, retaining its constraints and column
# transformations. Native prediction determines the change of basis once.
tube_basis <- function(model, smooth, data, domain) {
  if (smooth$dim != 1L || length(smooth$term) != 1L ||
    !inherits(smooth, c("cr.smooth", "cs.smooth", "pspline.smooth", "tprs.smooth", "ts.smooth"))) {
    cli::cli_abort("Tube bands require a supported one-dimensional cubic spline; smooth {.val {smooth$label}} is unsupported. Use simulation instead.")
  }
  coordinate <- smooth$term
  if (!is.numeric(data[[coordinate]]) || is.matrix(data[[coordinate]])) {
    cli::cli_abort("Tube bands require a numeric spline coordinate.")
  }
  by <- smooth$by
  if (!identical(by, "NA") && !is.factor(data[[by]]) &&
    (is.null(data[[by]]) || length(unique(data[[by]])) != 1L)) {
    cli::cli_abort("Tube bands require a fixed numeric by multiplier within each curve.")
  }
  if (inherits(smooth, c("cr.smooth", "cs.smooth"))) {
    knots <- as.numeric(smooth$xp)
    cardinal <- lapply(seq_along(knots), function(j) {
      stats::splinefun(knots, as.numeric(seq_along(knots) == j), method = "natural")
    })
    raw <- function(x, deriv) vapply(cardinal, function(f) f(x, deriv = deriv), numeric(length(x)))
    support <- range(knots)
  } else if (inherits(smooth, "pspline.smooth")) {
    if (smooth$m[1L] != 2 || !identical(smooth$mono, 0)) {
      cli::cli_abort("Tube bands currently require unconstrained cubic P-splines.")
    }
    knots <- sort(unique(smooth$knots))
    k <- smooth$m[1L] + 2L
    support <- range(smooth$knots[c(k, length(smooth$knots) - k + 1L)])
    raw <- function(x, deriv) {
      sm <- smooth
      sm$deriv <- deriv
      mgcv::Predict.matrix(sm, stats::setNames(list(x), coordinate))
    }
  } else {
    m <- smooth$p.order[1L]
    if (!m %in% c(0, 2) || !identical(smooth$drop.null %||% 0, 0)) {
      cli::cli_abort("Tube bands currently require the ordinary cubic one-dimensional thin plate basis.")
    }
    shift <- as.numeric(smooth$shift)
    centers <- as.numeric(smooth$Xu[, 1L])
    knots <- centers + shift
    # Thin plate knots may be subsampled. They are polynomial boundaries,
    # not the boundary of the fitted covariate domain.
    reference <- prepare_smooth_data(model, smooth)
    support <- range(reference[[coordinate]], na.rm = TRUE)
    UZ <- smooth$UZ
    raw <- function(x, deriv) {
      x <- x - shift
      delta <- outer(x, centers, "-")
      radial <- switch(deriv + 1L,
        abs(delta)^3 / 12,
        sign(delta) * delta^2 / 4,
        abs(delta) / 2,
        sign(delta) / 2
      )
      poly <- switch(deriv + 1L,
        cbind(1, x),
        cbind(0, rep(1, length(x))),
        matrix(0, length(x), 2L),
        matrix(0, length(x), 2L)
      )
      cbind(radial, poly) %*% UZ
    }
  }
  tolerance <- 1e-10 * max(1, diff(support))
  if (domain[1L] < support[1L] - tolerance || domain[2L] > support[2L] + tolerance) {
    cli::cli_abort("The tube domain must lie within the fitted spline support.")
  }
  # Include fitted coordinates as well as evenly spaced locations so the raw
  # basis has full column rank even for unevenly placed knots.
  at <- sort(unique(c(
    seq(support[1L], support[2L], length.out = max(51L, 4L * smooth$bs.dim)),
    knots[knots >= support[1L] & knots <= support[2L]]
  )))
  nd <- data[rep(1L, length(at)), , drop = FALSE]
  nd[[coordinate]] <- at
  attr(nd, "gratia.evaluated") <- TRUE
  native <- smooth_predict_matrix(smooth, nd, model)
  X <- raw(at, 0L)
  transform <- tryCatch(qr.solve(X, native, tol = 1e-12), error = function(e) {
    cli::cli_abort("Cannot recover the fitted spline basis for tube calibration: {conditionMessage(e)}")
  })
  if (max(abs(X %*% transform - native)) > 1e-8 * max(1, abs(native))) {
    cli::cli_abort("The tube basis does not reproduce fitted prediction; use simulation instead.")
  }
  list(
    coordinate = coordinate, support = support, knots = sort(unique(knots)),
    evaluate = function(x, deriv = 0L) {
      if (!deriv %in% 0:3) cli::cli_abort("Only derivatives through order three are available for the cubic tube basis.")
      raw(x, deriv) %*% transform
    }
  )
}

# A finite difference equals an integral of a derivative. Evaluating this
# identity with Gaussian quadrature on each polynomial piece avoids subtracting
# nearly equal basis values, especially for second derivatives.
tube_derivative_evaluator <- function(basis, order, type, h) {
  nodes <- c(
    -0.8611363115940526, -0.3399810435848563,
    0.3399810435848563, 0.8611363115940526
  )
  weights <- c(
    0.3478548451374539, 0.6521451548625461,
    0.6521451548625461, 0.3478548451374539
  )
  center <- if (order == 1L) {
    switch(type,
      forward = h / 2,
      backward = -h / 2,
      central = 0
    )
  } else {
    switch(type,
      forward = h,
      backward = -h,
      central = 0
    )
  }
  radius <- if (order == 1L) h / 2 else h
  function(x, deriv = 0L) {
    rows <- lapply(x, function(at) {
      middle <- at + center
      cuts <- c(
        -1, 1, if (order == 2L) 0,
        (basis$knots - middle) / radius
      )
      cuts <- sort(unique(cuts[cuts >= -1 & cuts <= 1]))
      left <- utils::head(cuts, -1L)
      right <- utils::tail(cuts, -1L)
      half <- (right - left) / 2
      z <- as.vector(outer(nodes, half) + rep((left + right) / 2, each = 4L))
      w <- rep(half, each = 4L) * rep(weights, length(half))
      w <- w * if (order == 1L) 0.5 else 1 - abs(z)
      drop(crossprod(w, basis$evaluate(middle + radius * z, order + deriv)))
    })
    do.call(rbind, rows)
  }
}

tube_covariance_root <- function(V) {
  V <- simultaneous_covariance(V)
  eig <- eigen(V, symmetric = TRUE)
  tolerance <- 100 * ncol(V) * .Machine$double.eps * max(abs(V))
  if (any(eig$values < -tolerance)) {
    cli::cli_abort("Tube bands require a positive-semidefinite covariance matrix.")
  }
  keep <- eig$values > 0
  sweep(eig$vectors[, keep, drop = FALSE], 2L, sqrt(pmax(0, eig$values[keep])), "*")
}

tube_critical <- function(length, level, components = 1L, root_tol = 1e-8) {
  alpha <- 1 - level
  tail_bound <- function(c) {
    2 * components * stats::pnorm(c, lower.tail = FALSE) +
      length / pi * exp(-c^2 / 2)
  }
  lo <- stats::qnorm(alpha / (2 * components), lower.tail = FALSE)
  if (length == 0) {
    return(list(critical = lo, residual = tail_bound(lo) - alpha))
  }
  hi <- max(1, lo * 2)
  while (tail_bound(hi) > alpha) hi <- hi * 2
  root <- stats::uniroot(function(c) tail_bound(c) - alpha,
    interval = c(lo, hi), tol = root_tol
  )$root
  # Move to the conservative side of the root within numerical tolerance.
  root <- root + root_tol
  list(critical = root, residual = tail_bound(root) - alpha)
}

# Merge boundaries differing only by rounding. Moving a subdivision boundary
# does not omit any part of the integration interval.
tube_partition <- function(x) {
  x <- sort(unique(x[is.finite(x) & x > 0 & x < 1]))
  out <- 0
  tolerance <- 64 * .Machine$double.eps
  for (at in x) {
    if (at - utils::tail(out, 1L) > tolerance && 1 - at > tolerance) out <- c(out, at)
  }
  c(out, 1)
}

tube_calibrate <- function(evaluate, V, domain, breakpoints = numeric(),
                           level = 0.95, control = list(), polynomial = FALSE) {
  validate_smooth_interval("confidence", level, 1, 1, NULL)
  control <- tube_controls(control)
  root <- tube_covariance_root(V)
  a <- domain[1L]
  width <- diff(domain)
  cuts <- tube_partition((breakpoints - a) / width)
  left <- utils::head(cuts, -1L)
  right <- utils::tail(cuts, -1L)
  sample <- sort(unique(c(
    cuts, seq(0, 1, length.out = 17L),
    as.vector(outer(c(0.2, 0.4, 0.6, 0.8), right - left) + rep(left, each = 4L))
  )))
  R <- evaluate(a + sample * width, 0L) %*% root
  if (!ncol(root) || (polynomial && max(abs(R)) == 0)) {
    return(list(
      critical = 0, length = 0, components = 0L,
      integration_error = 0, root_residual = 0, status = "deterministic"
    ))
  }
  # Exact rank-one processes share one normal deviate even if its sign changes.
  rank_one <- ncol(root) == 1L
  if (rank_one) {
    return(list(
      critical = coverage_normal(level), length = 0, components = 1L,
      integration_error = 0, root_residual = 0, status = "rank one"
    ))
  }
  # A fitted cubic basis can have an isolated common zero. Locate possible
  # zeros on each polynomial piece, then verify against the actual evaluator.
  zero <- tube_zero_points(evaluate, root, domain, breakpoints)
  cuts <- tube_partition(c(
    cuts, (zero - a) / width,
    (attr(zero, "partition_points") - a) / width
  ))
  interior_zero <- zero[zero > a & zero < a + width]
  components <- 1L + length(interior_zero)
  metric <- function(t) {
    x <- a + t * width
    r <- evaluate(x, 0L) %*% root
    dr <- (evaluate(x, 1L) %*% root) * width
    s <- sqrt(rowSums(r^2))
    if (any(s == 0)) cli::cli_abort("Unresolved zero variance in the tube integration; use simulation instead.")
    u <- r / s
    v <- dr - u * rowSums(u * dr)
    out <- sqrt(rowSums(v^2)) / s
    if (any(!is.finite(out))) cli::cli_abort("Non-finite tube geometry; use simulation instead.")
    out
  }
  answer <- lapply(seq_len(length(cuts) - 1L), function(i) {
    stats::integrate(metric, cuts[i], cuts[i + 1L],
      subdivisions = control$subdivisions,
      rel.tol = control$rel_tol, abs.tol = control$abs_tol / (length(cuts) - 1L),
      stop.on.error = FALSE
    )
  })
  if (any(vapply(answer, function(x) x$message != "OK", logical(1)))) {
    cli::cli_abort("Tube integration did not converge; tighten or increase tube controls, or use simulation.")
  }
  length <- sum(vapply(answer, `[[`, numeric(1), "value"))
  error <- sum(vapply(answer, `[[`, numeric(1), "abs.error"))
  critical <- tube_critical(length + error, level, components, control$root_tol)
  list(
    critical = critical$critical, length = length, components = components,
    integration_error = error, root_residual = critical$residual, status = "converged"
  )
}

tube_zero_points <- function(evaluate, root, domain, breakpoints) {
  cuts <- sort(unique(c(domain, breakpoints)))
  cuts <- cuts[cuts >= domain[1L] & cuts <= domain[2L]]
  nodes <- c(-1, -1 / 3, 1 / 3, 1)
  inverse <- solve(outer(nodes, 0:3, `^`))
  found <- numeric()
  partition <- numeric()
  for (i in seq_len(length(cuts) - 1L)) {
    half <- (cuts[i + 1L] - cuts[i]) / 2
    middle <- cuts[i] + half
    r <- evaluate(middle + half * nodes, 0L) %*% root
    coefficients <- inverse %*% r
    j <- which.max(colSums(abs(coefficients)))
    p <- coefficients[, j]
    while (length(p) > 1L && abs(utils::tail(p, 1L)) <= .Machine$double.eps * max(abs(p))) p <- utils::head(p, -1L)
    candidates <- if (length(p) > 1L) polyroot(p) else complex()
    candidates <- Re(candidates[abs(Im(candidates)) < 1e-7 & abs(Re(candidates)) <= 1])
    candidates <- unique(c(-1, candidates, 1))
    at <- middle + half * candidates
    partition <- c(partition, at)
    actual <- evaluate(at, 0L) %*% root
    magnitude <- apply(abs(actual), 1L, max)
    if (any(magnitude > 0 & magnitude < sqrt(.Machine$double.eps) * max(abs(r)))) {
      cli::cli_abort("Near-zero variance prevents reliable tube integration; use simulation or a smaller regular domain.")
    }
    found <- c(found, at[magnitude == 0])
  }
  structure(sort(unique(found)), partition_points = sort(unique(partition)))
}

tube_band <- function(estimate, se, X, V, evaluate, domain, breakpoints,
                      level, control = list()) {
  expected <- sqrt(pmax(0, rowSums((X %*% V) * X)))
  if (any(!is.finite(estimate)) || any(!is.finite(se)) ||
    max(abs(expected - se)) > 1e-7 * max(1, se)) {
    cli::cli_abort("Tube calibration and returned standard errors must use the same finite uncertainty design.")
  }
  info <- tube_calibrate(evaluate, V, domain, breakpoints, level, control, polynomial = TRUE)
  list(
    critical = info$critical, lower = estimate - info$critical * se,
    upper = estimate + info$critical * se, info = info
  )
}

tube_info <- function(records, scope, unconditional, frequentist) {
  list(
    calibration = "tube", calculation = "tube_crossing_bound",
    domain = "continuous interval", scope = scope, curves = records,
    unconditional = unconditional, frequentist = frequentist,
    coverage = "at least the requested probability under the selected Gaussian approximation, subject to numerical accuracy"
  )
}
