# Research adapter: capture the design representation used by predict.bamd
# without modifying mgcv's namespace or allocating its full lpmatrix.
compressed_design <- function(model) {
  stopifnot(inherits(model, "bam"), !is.null(model$dinfo))
  captured <- new.env(parent = emptyenv())
  prediction <- get("predict.bamd", asNamespace("mgcv"))
  environment(prediction) <- new.env(parent = environment(prediction))
  environment(prediction)$Xbd <- function(X, beta, k, ks, ts, dt, v, qc,
                                         drop = NULL, lt = NULL) {
    captured$design <- list(X = X, k = k, ks = ks, ts = ts, dt = dt,
      v = v, qc = qc, drop = drop, lt = lt)
    mgcv::Xbd(X, beta, k, ks, ts, dt, v, qc, drop = drop, lt = lt)
  }
  prediction(model, type = "link")
  captured$design
}

compressed_evaluate <- function(design, target, beta) {
  eta <- do.call(mgcv::Xbd, c(design, list(beta = beta))) + target$offset
  result <- get("sampling_likelihood", asNamespace("gratia"))(
    eta, target$y, target$w, target$kind, target$phi, target$theta)
  args <- design
  args$lt <- NULL
  args$drop <- NULL
  raw_gradient <- do.call(mgcv::XWyd, c(args,
    list(w = rep(1, length(eta)), y = result$score)))
  # Xbd expands dropped coefficients and selects the term-specific lpip map.
  # Its transpose must sum repeated map entries before dropping coefficients.
  index <- unlist(attr(design$X, "lpip"))
  stopifnot(length(index) == length(raw_gradient))
  gradient <- numeric(length(beta) + length(design$drop))
  for (i in seq_along(index)) gradient[index[i]] <- gradient[index[i]] + raw_gradient[i]
  if (length(design$drop)) gradient <- gradient[-design$drop]
  gradient <- gradient - drop(target$S %*% beta) / target$phi
  list(value = sum(result$value) - sum(beta * (target$S %*% beta)) / (2 * target$phi),
    gradient = unname(gradient), eta = eta)
}
