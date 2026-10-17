# Run from the package root with Rscript notes/tube-bands/validate.R.
# N_DRAWS changes Monte Carlo precision; the recorded default is 50000.
pkgload::load_all(quiet = TRUE)
library("mgcv")
library("gratia")
ns <- asNamespace("gratia")
internal <- function(name) get(name, ns)
draw_count <- as.integer(Sys.getenv("N_DRAWS", "50000"))
set.seed(29017)
out_dir <- "notes/tube-bands"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

poly_product <- function(a, b) {
  out <- numeric(length(a) + length(b) - 1L)
  for (j in seq_along(a)) out[j - 1L + seq_along(b)] <-
    out[j - 1L + seq_along(b)] + a[j] * b
  out
}
poly_derivative <- function(a) if (length(a) < 2L) 0 else a[-1L] * seq_len(length(a) - 1L)
poly_value <- function(a, x) {
  y <- rep(tail(a, 1L), length(x))
  if (length(a) > 1L) for (j in (length(a) - 1L):1L) y <- a[j] + x * y
  y
}

# On each cubic piece, all stationary points of p(x)/sqrt(q(x)) solve
# 2 p'(x) q(x) - p(x) q'(x) = 0. Check every real root and both endpoints.
# This reference does not use tube length, its metric, or a grid maximum.
prepare_supremum <- function(evaluate, V, domain, breakpoints) {
  root <- internal("tube_covariance_root")(V)
  cuts <- sort(unique(c(domain, breakpoints)))
  cuts <- cuts[cuts >= domain[1] & cuts <= domain[2]]
  nodes <- c(-1, -1 / 3, 1 / 3, 1)
  inverse <- solve(outer(nodes, 0:3, `^`))
  pieces <- lapply(seq_len(length(cuts) - 1L), function(i) {
    half <- diff(cuts[i + 0:1]) / 2
    middle <- cuts[i] + half
    C <- inverse %*% (evaluate(middle + half * nodes) %*% root)
    q <- Reduce(`+`, lapply(seq_len(ncol(C)), function(j) poly_product(C[, j], C[, j])))
    H <- vapply(seq_len(ncol(C)), function(j) {
      2 * poly_product(poly_derivative(C[, j]), q) -
        poly_product(C[, j], poly_derivative(q))
    }, numeric(9L))
    list(C = C, H = H)
  })
  list(pieces = pieces, rank = ncol(root))
}

continuous_maxima <- function(reference, z) {
  maximum <- numeric(nrow(z))
  for (piece in reference$pieces) {
    numerators <- z %*% t(piece$C)
    polynomials <- z %*% t(piece$H)
    for (b in seq_len(nrow(z))) {
      p <- polynomials[b, ]
      while (length(p) > 1L && abs(tail(p, 1L)) < 1e-12 * max(abs(p))) p <- head(p, -1L)
      roots <- if (length(p) > 1L && any(p != 0)) polyroot(p) else complex()
      at <- Re(roots[abs(Im(roots)) < 1e-7 & abs(Re(roots)) <= 1])
      at <- c(-1, at, 1)
      r <- outer(at, 0:3, `^`) %*% piece$C
      se <- sqrt(rowSums(r^2))
      value <- abs(poly_value(numerators[b, ], at)) / se
      maximum[b] <- max(maximum[b], value[is.finite(value)])
    }
  }
  maximum
}

d <- data.frame(x = seq(0, 1, length.out = 150))
d$y <- sin(6 * d$x) + rnorm(nrow(d), sd = .3)
model <- gam(y ~ s(x, bs = "cr", k = 5), data = d, method = "REML")
sm <- model$smooth[[1]]
basis <- internal("tube_basis")(model, sm, d, c(0, 1))
V <- vcov(model)
indices <- sm$first.para:sm$last.para
smooth_evaluate <- function(x, deriv = 0L) {
  X <- matrix(0, length(x), length(coef(model)))
  X[, indices] <- basis$evaluate(x, deriv)
  if (deriv == 0L) X[, 1] <- 1
  X
}
fixtures <- list(smooth = list(evaluate = smooth_evaluate, V = V,
  domain = c(.05, .95), breaks = basis$knots))
for (order in 1:2) {
  h <- 1e-3
  evaluate <- internal("tube_derivative_evaluator")(basis, order, "central", h)
  shifts <- if (order == 1) c(-h / 2, h / 2) else c(-h, 0, h)
  fixtures[[paste0("derivative", order)]] <- list(evaluate = evaluate,
    V = V[indices, indices], domain = c(.05, .95),
    breaks = as.vector(outer(basis$knots, shifts, "-")))
}
gd <- transform(d, group = factor(rep(c("a", "b"), 75)))
gd$y <- gd$y + ifelse(gd$group == "a", 0, .5 + gd$x)
group_model <- gam(y ~ group + s(x, by = group, bs = "cr", k = 5), data = gd, method = "REML")
group_evaluate <- function(x, deriv = 0L) {
  X <- matrix(0, length(x), length(coef(group_model)))
  if (deriv == 0L) X[, 2] <- -1
  for (j in 1:2) {
    sm <- group_model$smooth[[j]]
    b <- group_basis[[j]]
    X[, sm$first.para:sm$last.para] <- (if (j == 1) 1 else -1) * b$evaluate(x, deriv)
  }
  X
}
group_basis <- lapply(group_model$smooth, function(sm) {
  internal("tube_basis")(group_model, sm, gd[gd$group == sm$by.level, ], c(0, 1))
})
fixtures$difference <- list(evaluate = group_evaluate, V = vcov(group_model),
  domain = c(.05, .95), breaks = unique(unlist(lapply(group_basis, `[[`, "knots"))))

results <- lapply(names(fixtures), function(name) {
  f <- fixtures[[name]]
  t <- system.time(band <- internal("tube_calibrate")(f$evaluate, f$V,
    f$domain, f$breaks, polynomial = TRUE))["elapsed"]
  tight <- internal("tube_calibrate")(f$evaluate, f$V, f$domain, f$breaks,
    control = list(rel_tol = 1e-9, abs_tol = 1e-10), polynomial = TRUE)
  reference <- prepare_supremum(f$evaluate, f$V, f$domain, f$breaks)
  z <- matrix(rnorm(draw_count * reference$rank), draw_count, reference$rank)
  chunks <- split(seq_len(draw_count), rep(1:2, length.out = draw_count))
  elapsed <- system.time(parts <- parallel::mclapply(chunks, function(idx) {
    list(index = idx, maximum = continuous_maxima(reference, z[idx, , drop = FALSE]))
  }, mc.cores = 2L))["elapsed"]
  maxima <- numeric(draw_count)
  for (part in parts) maxima[part$index] <- part$maximum
  # Independently check the polynomial reference against a fine grid for a
  # subset of the same draws. A continuous maximum cannot be smaller.
  grid <- seq(f$domain[1], f$domain[2], length.out = 5001)
  X <- f$evaluate(grid) %*% internal("tube_covariance_root")(f$V)
  grid_max <- apply(abs((X / sqrt(rowSums(X^2))) %*% t(z[1:100, , drop = FALSE])), 2, max)
  stopifnot(all(maxima[1:100] >= grid_max - 1e-6),
    max(maxima[1:100] - grid_max) < .01)
  coverage <- mean(maxima <= band$critical)
  ci <- binom.test(sum(maxima <= band$critical), draw_count)$conf.int
  answer <- data.frame(quantity = name, draws = draw_count,
    tube_critical = band$critical, continuous_simulation_critical = unname(quantile(maxima, .95)),
    coverage = coverage, lower_95 = ci[1], upper_95 = ci[2],
    integration_error = band$integration_error,
    tighter_critical_difference = tight$critical - band$critical,
    tube_seconds = unname(t), reference_seconds = unname(elapsed))
  print(answer)
  answer
})
write.csv(do.call(rbind, results), file.path(out_dir, "conditional-coverage.csv"), row.names = FALSE)
writeLines(sub("[[:blank:]]+$", "", capture.output(sessionInfo())), file.path(out_dir, "session-info.txt"))

# Rendering checks: ordinary plots retain their existing annotation format.
smooth <- smooth_estimates(model, interval = "simultaneous", simultaneous_method = "tube")
rate <- derivatives(model, order = 2, type = "central", interval = "simultaneous", simultaneous_method = "tube")
comparison <- difference_smooths(group_model, select = "s(x)", group_means = TRUE,
  interval = "simultaneous", simultaneous_method = "tube")
png(file.path(out_dir, "tube-plots.png"), width = 1500, height = 600, res = 130)
print(draw(smooth) + draw(rate) + draw(comparison))
invisible(dev.off())
