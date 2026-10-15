# Common model and evaluation data for the LSS transformation benchmarks.
lss_benchmark_fixture <- function(name) {
  set.seed(281)
  n <- 2000L
  d <- data.frame(x = runif(n), o1 = runif(n, -.1, .1), o2 = runif(n, -.1, .1),
    o3 = runif(n, -.05, .05), o4 = runif(n, -.05, .05))
  f <- get(name, asNamespace("mgcv"))()
  beta <- switch(name,
    gaulss = c(.4, .3, -.4, .2), gammals = c(.4, .3, -1, .2),
    gumbls = c(.4, .3, -1, .2), gevlss = c(.4, .3, -1, .2, -.5, .1),
    shash = c(.4, .3, -1, .2, .4, .1, .1, .1),
    twlss = c(.4, .3, .1, .2, -.4, .1), ziplss = c(.4, .3, -.1, .2))
  eta <- do.call(cbind, lapply(seq_len(f$nlp), function(j)
    beta[2 * j - 1] + beta[2 * j] * d$x + if (name == "shash") 0 else d[[paste0("o", j)]]))
  f <- gratia:::fix_family_rd(f)
  d$y <- f$rd(gratia:::family_prediction_parameters(f, eta), rep(1, n), 1)
  formulas <- list(y ~ x + offset(o1), ~x + offset(o2), ~x + offset(o3),
    ~x + offset(o4))[seq_len(f$nlp)]
  if (name == "shash") formulas <- list(y ~ x, ~x, ~x, ~x)
  m <- withCallingHandlers(gam(formulas, family = f, data = d, method = "REML"),
    warning = function(w) {
      if (name == "gevlss" && conditionMessage(w) == "NaNs produced")
        invokeRestart("muffleWarning")
    })
  # Genuinely new covariates and formula offsets; no observed response.
  nd <- d[1:200, setdiff(names(d), "y")]
  nd$x <- seq(.001, .999, length.out = 200)
  list(model = m, data = nd)
}
