# Strong nonlinear signals keep the smoothing covariance identifiable in the
# family tests. Censoring limits and grouped labels remain in the raw response.
smoothing_family_fixture <- function(name, engine = "gam", method = "ML",
                                     linked = FALSE, fixed = FALSE, tensor = FALSE,
                                     missing = FALSE, call_offset = FALSE,
                                     censored = TRUE) {
  withr::local_seed(if (name == "ziP") 943 else 941)
  n <- if (name == "ziP") 600L else 320L
  d <- data.frame(x = runif(n), z = runif(n), w = runif(n, .8, 1.2))
  d$o <- .1 + .15 * d$x
  eta <- .2 + 1.2 * sin(6 * d$x) + cos(7 * d$z) + d$o
  family <- switch(name,
    gaussian_log = gaussian("log"), poisson_sqrt = poisson("sqrt"),
    binomial_probit = binomial("probit"), binomial_matrix = binomial(),
    binomial_factor = binomial(), Tweedie = mgcv::Tweedie(p = 1.5),
    negbin = mgcv::negbin(theta = 3), ocat = mgcv::ocat(R = 4),
    ziP = mgcv::ziP(b = .3),
    gfam = mgcv::gfam(list(gaussian(), mgcv::nb())),
    gfam_ocat = mgcv::gfam(list(gaussian(), mgcv::ocat(R = 4))),
    inverse.gaussian = inverse.gaussian("log"),
    if (name %in% c("poisson", "binomial", "Gamma", "quasipoisson", "quasibinomial"))
      get(name, asNamespace("stats"))(link = if (name == "Gamma") "log" else
        if (name %in% c("binomial", "quasibinomial")) "logit" else "log") else
      get(name, asNamespace("mgcv"))())
  # Native families can refer to exported mgcv helpers through the search
  # path. Supply them locally when fitting namespace-only test fixtures.
  family <- sampling_smoothing_family_bindings(family)
  y <- switch(name,
    gaussian_log = exp(eta) + rnorm(n, sd = .12),
    poisson = , poisson_sqrt = , cpois = , quasipoisson = rpois(n, exp(eta)),
    binomial = , quasibinomial = , binomial_matrix = , binomial_probit = {
      trials <- sample(8:15, n, replace = TRUE)
      probability <- if (name == "binomial_probit") pnorm(eta) else plogis(eta)
      success <- rbinom(n, trials, probability)
      d$w <- sample(1:2, n, replace = TRUE)
      if (name == "binomial_matrix") cbind(success, trials - success) else {
        d$w <- d$w * trials
        success / trials
      }
    },
    binomial_factor = {d$w <- sample(1:2, n, replace = TRUE)
      factor(rbinom(n, 1, plogis(eta)), labels = c("no", "yes"))},
    Gamma = rgamma(n, shape = 8, scale = exp(eta) / 8),
    nb = , negbin = rnbinom(n, mu = exp(eta), size = 3),
    betar = rbeta(n, plogis(eta) * 15, (1 - plogis(eta)) * 15),
    scat = eta + .3 * rt(n, df = 5),
    tw = , Tweedie = mgcv::rTweedie(exp(eta), p = 1.5, phi = .2),
    cnorm = eta + rnorm(n, sd = .3),
    clog = eta + rlogis(n, scale = .2),
    bcg = exp(eta + rnorm(n, sd = .3)),
    ocat = , gfam_ocat = 1L + rowSums(outer(eta + rlogis(n), c(-1, .5, 2), `>`)),
    gfam = rnbinom(n, mu = exp(eta), size = 3),
    ziP = {
      d$w <- rep(1, n)
      lambda <- exp(eta)
      probability <- -expm1(-exp(-.5 + 1.5 * eta))
      presence <- runif(n) < probability
      y <- numeric(n)
      y[presence] <- qpois(runif(sum(presence), dpois(0, lambda[presence]), 1),
        lambda[presence])
      y
    },
    inverse.gaussian = {
      mu <- exp(eta); v <- rnorm(n)^2
      a <- mu + mu^2 * v / 20 - mu / 20 * sqrt(40 * mu * v + mu^2 * v^2)
      ifelse(runif(n) <= mu / (mu + a), a, mu^2 / a)
    })
  if (censored && name %in% c("cnorm", "clog", "cpois", "bcg")) {
    limits <- cbind(y, y)
    # Include left, right and interval censoring, without censoring all rows.
    right <- seq_len(n) %% 8L == 0L
    left <- seq_len(n) %% 8L == 1L
    interval <- seq_len(n) %% 8L == 2L
    limits[right, 1] <- y[right] - if (name == "cpois") .5 else .1
    limits[right, 2] <- Inf
    limits[left, 1] <- y[left] + if (name == "cpois") .5 else .1
    limits[left, 2] <- if (name == "bcg") 0 else -Inf
    limits[interval, 1] <- pmax(if (name == "bcg") 1e-4 else -Inf,
      y[interval] - if (name == "cpois") .5 else .05)
    limits[interval, 2] <- y[interval] + if (name == "cpois") .5 else .05
    if (name == "cpois") {
      limits <- cbind(y, y)
      right <- right & y > 4.5
      left <- left & y < 2.5
      interval <- interval & y > 1.5 & y < 5.5
      limits[right, ] <- cbind(rep(4.5, sum(right)), rep(Inf, sum(right)))
      limits[left, ] <- cbind(rep(2.5, sum(left)), rep(-Inf, sum(left)))
      limits[interval, ] <- cbind(rep(1.5, sum(interval)), rep(5.5, sum(interval)))
    }
    y <- limits
  }
  if (name %in% c("gfam", "gfam_ocat")) {
    d$group <- factor(rep(1:2, length.out = n))
    first <- d$group == 1
    y[first] <- eta[first] + rnorm(sum(first), sd = .3)
    y <- cbind(y, as.integer(d$group))
  }
  d$y <- y
  if (missing) d$x[c(5, 18)] <- NA_real_
  f <- if (name %in% c("gfam", "gfam_ocat"))
    y ~ group + s(x, by = group, k = 5) + s(z, by = group, k = 5) else
    if (tensor) y ~ te(x, z, k = c(4, 4)) else
    if (linked) y ~ s(x, k = 5, id = "same") + s(z, k = 5, id = "same") else
    if (fixed) y ~ s(x, k = 5, sp = .1) + s(z, k = 5) else
      y ~ s(x, k = 5) + s(z, k = 5)
  if (!call_offset) f <- update(f, . ~ . + offset(o))
  fitter <- get(engine, asNamespace("mgcv"))
  fit <- fitter(f, data = d, weights = w, family = family, method = method,
    offset = if (call_offset) d$o else NULL, na.action = na.exclude,
    control = mgcv::gam.control(epsilon = 1e-8, maxit = 100L))
  list(model = fit, data = d, formula = f)
}
