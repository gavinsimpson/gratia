# Natural parameters from the documented default family definitions. These
# functions do not use the implementation's adapters or native inverse links.
ale_parameter_functions <- list(
  gaulss = list(location = identity, scale = function(e) .01 + exp(e)),
  gammals = list(location = exp, scale = function(e) {
    exp(-7 + log1p(exp(e + 7)))
  }),
  gumbls = list(location = identity, scale = function(e) {
    exp(-7 + log1p(exp(e + 7)))
  }),
  gevlss = list(location = identity, scale = exp, shape = function(e) {
    1.5 / (1 + exp(-e)) - 1
  }),
  shash = list(
    location = identity,
    scale = function(e) .01 + exp(e),
    skewness = identity,
    kurtosis = exp
  ),
  twlss = list(
    location = exp,
    power = function(e) (1.99 + 1.01 * exp(-e)) / (1 + exp(-e)),
    scale = exp
  ),
  ziplss = list(location = exp, pi = function(e) 1 - exp(-exp(e)))
)

# An independent calculation using whole data frames and one interval at a time.
# This deliberately does not use the implementation's blocking or gradients.
ale_reference <- function(
  model,
  data,
  focal,
  breaks,
  transform = identity,
  index = 1L,
  center = TRUE,
  exclude = NULL
) {
  bin <- cut(data[[focal]], breaks, include.lowest = TRUE, labels = FALSE)
  increments <- vapply(
    seq_len(length(breaks) - 1L),
    function(k) {
      a <- b <- data[bin == k, , drop = FALSE]
      a[[focal]] <- breaks[k]
      b[[focal]] <- breaks[k + 1L]
      predict_one <- function(d) {
        eta <- predict(model, newdata = d, type = "link", exclude = exclude)
        if (is.matrix(eta)) {
          eta <- eta[, index]
        }
        transform(eta)
      }
      mean(predict_one(b) - predict_one(a))
    },
    numeric(1L)
  )
  value <- c(0, cumsum(increments))
  if (center) {
    value <- value - mean(approx(breaks, value, xout = data[[focal]])$y)
  }
  value
}

ale_test_data <- function(n = 120L) {
  withr::with_seed(410, {
    d <- data.frame(x = sort(runif(n, .2, 1.8)), z = runif(n, -.5, .5))
    d$off <- .2 + .1 * d$z
    d$y <- 1 + 2 * d$x + .3 * d$z + rnorm(n, sd = .4)
    d
  })
}

test_that("linear ALE and its uncertainty agree with analytic results", {
  d <- ale_test_data()
  m <- mgcv::gam(y ~ x + z + offset(off), data = d, method = "REML")
  m <- with_model_envir(m, environment())
  a <- ale(m, "x", n_bins = 6)
  expect_identical(class(a), c("ale", "tbl_df", "tbl", "data.frame"))
  expect_named(
    a,
    c(
      ".focal",
      ".parameter",
      ".value",
      ".ale",
      ".se",
      ".crit",
      ".lower_ci",
      ".upper_ci",
      ".bin",
      ".n"
    )
  )
  dx <- a$.value - mean(d$x)
  expect_equal(a$.ale, unname(coef(m)["x"] * dx))
  expect_equal(a$.se, unname(abs(dx) * sqrt(vcov(m)["x", "x"])))
  expect_equal(a$.lower_ci, a$.ale - qnorm(.975) * a$.se)
  expect_equal(a$.upper_ci, a$.ale + qnorm(.975) * a$.se)
  expect_equal(
    mean(approx(a$.value, a$.ale, xout = d$x)$y),
    0,
    tolerance = 1e-12
  )
  expect_identical(a$.bin, 0:6)
  expect_equal(sum(a$.n), nrow(d))
  expect_identical(a$.n[1], 0L)
  expect_identical(attr(a, "ale_info")$n_reference, nrow(d))
  expect_identical(a, ale(m, "x", n_bins = 6, scale = "linear_predictor"))
  response <- ale(m, "x", n_bins = 6, scale = "response")
  expect_equal(response$.ale, a$.ale)
  expect_equal(response$.se, a$.se)
  for (freq in c(FALSE, TRUE)) {
    b <- ale(m, "x", n_bins = 6, center = FALSE, frequentist = freq)
    expect_equal(b$.ale, unname(coef(m)["x"] * (b$.value - min(d$x))))
    expect_equal(
      b$.se,
      abs(b$.value - min(d$x)) *
        sqrt(unname(get_vcov(m, frequentist = freq)[2L, 2L]))
    )
    expect_identical(b$.ale[1], 0)
    expect_identical(b$.se[1], 0)
  }
  weighted <- mgcv::gam(y ~ x, data = d, weights = seq_len(nrow(d)))
  w <- ale(weighted, "x", n_bins = 5)
  expect_equal(w$.ale, unname(coef(weighted)["x"] * (w$.value - mean(d$x))))
  # One coefficient and one bin must retain a two-row, one-column gradient.
  one <- mgcv::gam(y ~ 0 + x, data = d)
  expect_equal(
    ale(one, "x", n_bins = 1)$.ale,
    unname(coef(one)[1] * (range(d$x) - mean(d$x)))
  )
})

test_that("smooths, interactions, factors and blocks use whole-model changes", {
  d <- ale_test_data()
  d$fac <- factor(rep(c("a", "b"), length.out = nrow(d)))
  smooth <- mgcv::gam(y ~ s(x, k = 6) + s(z, k = 5), data = d, method = "REML")
  a <- ale(smooth, "x", n_bins = 8)
  terms <- predict(
    smooth,
    newdata = data.frame(x = a$.value, z = 0),
    type = "terms"
  )[, "s(x)"]
  expect_equal(
    a$.ale,
    unname(terms - mean(approx(a$.value, terms, xout = d$x)$y))
  )
  models <- list(
    mgcv::gam(y ~ te(x, z, k = c(4, 4)), data = d),
    mgcv::gam(y ~ s(x, k = 4) + ti(x, z, k = c(4, 4)), data = d),
    mgcv::gam(y ~ fac + s(x, by = fac, k = 4) + z, data = d)
  )
  for (m in models) {
    a <- ale(m, "x", n_bins = 7)
    expect_equal(a$.ale, ale_reference(m, d, "x", a$.value))
    b <- ale(m, "x", n_bins = 7, block_size = 13)
    expect_equal(a, b, tolerance = 1e-10)
  }
  e <- ale(models[[2]], "x", n_bins = 7, exclude = "ti(x,z)")
  expect_equal(
    e$.ale,
    ale_reference(models[[2]], d, "x", e$.value, exclude = "ti(x,z)")
  )
})

test_that("response effects and centred gradients agree with independent calculations", {
  d <- ale_test_data()
  d$y <- withr::with_seed(
    411,
    rpois(nrow(d), exp(.4 + .6 * d$x + .3 * d$x * d$z + d$off))
  )
  m <- mgcv::gam(y ~ x * z + offset(off), family = poisson(), data = d)
  m <- with_model_envir(m, environment())
  a <- ale(m, "x", n_bins = 5, scale = "response")
  expected <- ale_reference(m, d, "x", a$.value, transform = exp)
  expect_equal(a$.ale, expected)
  link <- ale(m, "x", n_bins = 5)
  expect_false(isTRUE(all.equal(a$.ale, exp(link$.ale))))
  # Differentiate the complete independent calculation, including its centring.
  h <- 1e-5
  J <- vapply(
    seq_along(coef(m)),
    function(j) {
      hi <- lo <- m
      hi$coefficients[j] <- coef(m)[j] + h
      lo$coefficients[j] <- coef(m)[j] - h
      (ale_reference(hi, d, "x", a$.value, exp) -
        ale_reference(lo, d, "x", a$.value, exp)) /
        (2 * h)
    },
    numeric(nrow(a))
  )
  se <- sqrt(diag(J %*% vcov(m) %*% t(J)))
  expect_equal(a$.se, se, tolerance = 1e-7)
  b <- ale(
    m,
    "x",
    n_bins = 5,
    scale = "response",
    interval = "simultaneous",
    seed = 15,
    n_sim = 400
  )
  draws <- withr::with_seed(15, mvnfast::rmvn(400, rep(0, ncol(J)), vcov(m)))
  maxima <- apply(abs(sweep(J %*% t(draws), 1L, se, "/")), 2L, max)
  critical <- unname(quantile(maxima, .95, type = 8))
  expect_equal(b$.crit, rep(critical, nrow(b)), tolerance = 1e-7)
  expect_equal(b$.lower_ci, expected - critical * se, tolerance = 1e-7)
})

test_that("distributional adapters return natural parameters and their derivatives", {
  for (id in c(
    "gaulss",
    "gammals",
    "gumbls",
    "gevlss",
    "shash",
    "twlss",
    "ziplss"
  )) {
    f <- get(id, asNamespace("mgcv"))()
    # No model fit is required to check parameter transformations themselves.
    m <- structure(list(family = f), class = "gam")
    eta <- c(-1, .1, .7)
    functions <- ale_parameter_functions[[id]]
    par <- names(functions)
    for (j in seq_along(par)) {
      natural <- functions[[j]]
      target <- ale_target(m, par[j], NULL, "response")
      result <- target$evaluate(eta)
      expect_identical(target$name, par[j], info = id)
      expect_equal(result$value, natural(eta), info = paste(id, par[j]))
      derivative <- (natural(eta + 1e-6) - natural(eta - 1e-6)) / 2e-6
      expect_equal(
        result$slope,
        derivative,
        tolerance = 1e-7,
        info = paste(id, par[j])
      )
    }
  }
  f <- mgcv::gaulss(link = list("log", "logb"), b = .3)
  m <- structure(list(family = f), class = "gam")
  expect_equal(ale_target(m, "mu", NULL, "response")$evaluate(0)$value, 1)
  expect_equal(ale_target(m, "sigma", NULL, "response")$evaluate(0)$value, 1.3)
  target <- ale_target(
    structure(list(family = mgcv::ziplss()), class = "gam"),
    "pi",
    NULL,
    "response"
  )
  expect_identical(target$evaluate(1000)$value, 1)
  expect_identical(target$evaluate(1000)$slope, 0)
})

test_that("distributional ALE selects the correct coefficient block and offsets", {
  d <- ale_test_data(220)
  d$off2 <- -.2 + .1 * d$z
  d$y <- withr::with_seed(
    412,
    rnorm(
      nrow(d),
      exp(.4 + .3 * d$x + d$off),
      sd = .1 + exp(-.5 + .2 * d$z + d$off2)
    )
  )
  m <- mgcv::gam(
    list(y ~ x + offset(off), ~ z + offset(off2)),
    data = d,
    family = mgcv::gaulss(link = list("log", "logb"), b = .1)
  )
  m <- with_model_envir(m, environment())
  for (j in 1:2) {
    focal <- c("x", "z")[j]
    a <- ale(m, focal, which_eta = j, n_bins = 5, scale = "response")
    transform <- if (j == 1) exp else function(e) .1 + exp(e)
    expect_equal(a$.ale, ale_reference(m, d, focal, a$.value, transform, j))
    h <- 1e-5
    J <- vapply(
      seq_along(coef(m)),
      function(k) {
        hi <- lo <- m
        hi$coefficients[k] <- coef(m)[k] + h
        lo$coefficients[k] <- coef(m)[k] - h
        (ale_reference(hi, d, focal, a$.value, transform, j) -
          ale_reference(lo, d, focal, a$.value, transform, j)) /
          (2 * h)
      },
      numeric(nrow(a))
    )
    expect_equal(a$.se, sqrt(diag(J %*% vcov(m) %*% t(J))), tolerance = 1e-7)
  }
  expect_identical(ale(m, "z", parameter = "sigma"), ale(m, "z", which_eta = 2))
  expect_error(
    ale(m, "z", parameter = "sigma", which_eta = 1),
    "different linear predictors"
  )
  # Shared coefficients belong to both predictors and must not be counted twice.
  shared <- mgcv::gam(
    list(y ~ x, ~x, 1 + 2 ~ z - 1),
    data = d,
    family = mgcv::gaulss()
  )
  for (j in 1:2) {
    a <- ale(shared, "z", which_eta = j, n_bins = 5)
    expect_equal(a$.ale, ale_reference(shared, d, "z", a$.value, index = j))
    ix <- lss_eta_index(shared)[[j]]
    zcol <- ix[grepl("z", names(coef(shared))[ix])]
    expect_equal(
      a$.se,
      abs(a$.value - mean(d$z)) * sqrt(vcov(shared)[zcol, zcol])
    )
  }
})

test_that("raw expressions, reference rows and BAM predictions are respected", {
  d <- ale_test_data()
  d$y[2] <- NA
  m <- mgcv::gam(
    y ~ s(log(x), k = 5) + I(x * z) + offset(log(x)),
    data = d,
    subset = seq_len(nrow(d)) > 4,
    na.action = na.exclude
  )
  m <- with_model_envir(m, environment())
  a <- ale(m, "x", n_bins = 5)
  expect_identical(attr(a, "ale_info")$n_reference, nrow(d) - 4L)
  nd <- d[c(2, 10, 20, 80, 115), ]
  b <- ale(m, "x", data = nd, n_bins = 3)
  expect_equal(b$.ale, ale_reference(m, nd, "x", b$.value))
  d$y[2] <- 1
  names(d)[names(d) == "x"] <- "température"
  unusual <- mgcv::gam(y ~ s(I(`température`), k = 5) + z, data = d)
  expect_s3_class(ale(unusual, "température", data = d), "ale")
  d$x <- d[["température"]]
  for (discrete in c(FALSE, TRUE)) {
    mb <- mgcv::bam(
      y ~ s(x, k = 5) + z + offset(off),
      data = d,
      discrete = discrete,
      method = "fREML"
    )
    mb <- with_model_envir(mb, environment())
    b <- ale(mb, "x", n_bins = 5, block_size = 7)
    expect_equal(b$.ale, ale_reference(mb, d, "x", b$.value), tolerance = 1e-7)
  }
  supplied_offset <- mgcv::gam(y ~ x + z, offset = off, data = d)
  supplied_offset <- with_model_envir(supplied_offset, environment())
  a <- ale(supplied_offset, "x", scale = "response", n_bins = 5)
  expect_equal(a$.ale, ale_reference(supplied_offset, d, "x", a$.value))
})

test_that("bins, invalid inputs, unsupported targets and RNG are handled", {
  d <- ale_test_data()
  m <- mgcv::gam(y ~ x + z, data = d)
  nd <- d
  nd$x <- rep(1:3, length.out = nrow(nd))
  a <- ale(m, "x", data = nd, n_bins = 40)
  expect_identical(a$.value, c(1, 2, 3))
  expect_identical(attr(a, "ale_info")$n_bins, 2L)
  custom <- ale(m, "x", breaks = c(min(d$x), 1, max(d$x)), n_bins = NA)
  expect_identical(custom$.value, c(min(d$x), 1, max(d$x)))
  expect_error(
    ale(
      m,
      "x",
      breaks = c(min(d$x), min(d$x) + 1e-8, min(d$x) + 2e-8, max(d$x))
    ),
    "empty intervals"
  )
  for (breaks in list(
    c(0, 1),
    c(NA, 1),
    rev(range(d$x)),
    c(min(d$x), 1, 1, max(d$x))
  )) {
    expect_error(ale(m, "x", breaks = breaks), "breaks")
  }
  for (value in list(0, NA_real_, 1.5, Inf)) {
    expect_error(ale(m, "x", n_bins = value), "n_bins")
    expect_error(ale(m, "x", block_size = value), "block_size")
  }
  for (bad in list(
    transform(d, x = NA_real_),
    transform(d, z = Inf),
    transform(d, x = 1)
  )) {
    expect_error(ale(m, "x", data = bad), "non-finite|constant")
  }
  expect_error(ale(m, "missing"), "focal")
  expect_error(ale(m, "x", data = d[FALSE, ]), "at least one")
  expect_error(ale(m, "x", data = d["x"]), "missing")
  expect_error(ale(m, "x", parameter = "shape"), "Unknown")
  expect_error(ale(m, "x", which_eta = 2), "exceeds")
  expect_error(ale(m, "x", type = "response"), "Reserved")
  expect_error(ale(m, "x", se.f = TRUE), "Reserved")
  expect_error(ale(m, "x", na.action = na.omit), "Reserved")
  expect_error(ale(m, "x", discrete = TRUE), "discrete")
  expect_error(ale(m, "x", scale = "bad"), "scale|arg")
  expect_error(ale_prediction_controls(list(1)), "unique, non-empty")
  expect_error(ale(m, "x", center = NA), "center")
  expect_error(ale(m, "x", interval = "simultaneous", n_sim = 0), "n_sim")
  expect_error(ale(lm(y ~ x, d), "x"), "Don't know")
  ord <- structure(
    list(family = mgcv::ocat(R = 3), formula = y ~ x),
    class = "gam"
  )
  expect_error(ale_target(ord, NULL, NULL, "response"), "Response-scale ALE")
  multi <- structure(list(family = mgcv::multinom(K = 2)), class = "gam")
  expect_error(
    ale_target(multi, NULL, NULL, "response"),
    "distribution parameters"
  )
  withr::local_seed(413)
  before <- .Random.seed
  a <- ale(m, "x", interval = "simultaneous", n_sim = 200, seed = 10)
  expect_identical(.Random.seed, before)
  expect_identical(
    a,
    ale(m, "x", interval = "simultaneous", n_sim = 200, seed = 10)
  )
  b <- ale(
    m,
    "x",
    exclude = "x",
    interval = "simultaneous",
    n_sim = 200,
    seed = 10
  )
  expect_equal(b$.ale, rep(0, nrow(b)))
  expect_equal(b$.se, rep(0, nrow(b)))
  expect_equal(b$.lower_ci, rep(0, nrow(b)))
})


test_that("ALE labels describe the selected distribution parameter", {
  expected <- list(
    gaulss = c(location = "mean", scale = "standard deviation"),
    gammals = c(location = "mean", scale = "dispersion"),
    gumbls = c(location = "location", scale = "scale"),
    gevlss = c(location = "location", scale = "scale", shape = "shape"),
    shash = c(
      location = "location",
      scale = "scale",
      skewness = "skewness",
      kurtosis = "kurtosis"
    ),
    twlss = c(location = "mean", power = "power", scale = "dispersion"),
    ziplss = c(location = "Poisson rate", pi = "presence probability")
  )
  for (id in names(expected)) {
    m <- structure(list(family = get(id, asNamespace("mgcv"))()), class = "gam")
    aliases <- link_parameter_map(stats::family(m))
    for (alias in names(aliases)) {
      j <- aliases[[alias]]
      for (scale in c("link", "response")) {
        target <- ale_target(m, alias, NULL, scale)
        expect_identical(
          target$label,
          unname(expected[[id]][j]),
          info = paste(id, alias, scale)
        )
      }
    }
  }
  for (f in list(
    stats::gaussian(),
    stats::poisson(),
    stats::binomial(),
    stats::Gamma(),
    mgcv::nb(),
    mgcv::betar()
  )) {
    m <- structure(list(family = f), class = "gam")
    expect_identical(ale_target(m, NULL, NULL, "link")$label, "mean")
  }
  ordinal <- structure(list(family = mgcv::ocat(R = 3)), class = "gam")
  expect_identical(ale_target(ordinal, NULL, NULL, "link")$label, "location")
})

test_that("draw.ale uses saved values and computation labels", {
  d <- ale_test_data()
  m <- mgcv::gam(y ~ x + z, data = d)
  a <- ale(m, "x", scale = "response", n_bins = 5)
  p <- draw(a)
  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$x, "x")
  expect_identical(p$labels$y, "ALE of mean")
  expect_identical(p$labels$caption, "Response scale")
  expect_null(p$labels$title)
  expect_null(p$labels$subtitle)
  expect_identical(draw(ale(m, "x"))$labels$caption, "Link scale")
  expect_null((p + ggplot2::labs(caption = NULL))$labels$caption)
  expect_identical(length(p$layers), 3L)
  expect_equal(ggplot2::ggplot_build(p)$data[[3]]$y, a$.ale)
  q <- draw(
    a,
    ci = FALSE,
    ref_line = FALSE,
    xlab = "Temperature",
    ylab = "Effect"
  )
  expect_identical(length(q$layers), 1L)
  expect_identical(q$labels$x, "Temperature")
  expect_identical(q$labels$y, "Effect")
  expect_error(draw(a, scale = "link"), "must be empty")
  expect_error(draw(a, alpha = NA), "alpha")
  expect_error(draw(a, ci = NA), "ci")
  expect_error(draw(a[, c(".value", ".ale")]), "required columns")
  expect_doppelganger("draw numeric ALE on response scale", p)
})

test_that("distributional fits give natural-parameter ALE for each predictor", {
  d <- ale_test_data(220)
  u <- withr::with_seed(416, runif(nrow(d), .01, .99))
  for (id in c("gammals", "gumbls", "gevlss", "shash", "twlss", "ziplss")) {
    f <- get(id, asNamespace("mgcv"))()
    formula <- list(y ~ x, ~z)
    if (id == "gammals") {
      d$y <- withr::with_seed(
        417,
        rgamma(nrow(d), shape = 3, scale = exp(.3 * d$x) / 3)
      )
    } else if (id %in% c("gumbls", "gevlss")) {
      d$y <- .2 + .3 * d$x - exp(-.5 + .1 * d$z) * log(-log(u))
      if (id == "gevlss") {
        d$y <- .2 + .3 * d$x + exp(-.5 + .1 * d$z) * ((-log(u))^(-.3) - 1) / .3
        formula[[3]] <- ~1
      }
    } else if (id == "shash") {
      d$y <- .3 * d$x + .4 * qnorm(u)
      formula[[3]] <- formula[[4]] <- ~1
    } else if (id == "twlss") {
      d$y <- withr::with_seed(
        418,
        mgcv::rTweedie(exp(.3 * d$x), p = 1.5, phi = .5)
      )
      formula <- list(y ~ x, ~1, ~z)
    } else {
      d$y <- withr::with_seed(
        419,
        ifelse(u < .25, 0, 1 + rpois(nrow(d), exp(.2 * d$x)))
      )
    }
    m <- mgcv::gam(formula, family = f, data = d, method = "REML")
    functions <- ale_parameter_functions[[id]]
    for (j in seq_along(functions)) {
      transform <- functions[[j]]
      for (scale in c("link", "response")) {
        a <- ale(m, "x", which_eta = j, scale = scale, n_bins = 4)
        expected <- ale_reference(
          m,
          d,
          "x",
          a$.value,
          transform = if (scale == "response") transform else identity,
          index = j
        )
        expect_equal(
          a$.ale,
          expected,
          tolerance = 1e-9,
          info = paste(id, j, scale)
        )
        expect_true(all(is.finite(a$.se)), info = paste(id, j, scale))
        expect_identical(
          draw(a)$labels$y,
          paste0("ALE of ", ale_target(m, NULL, j, scale)$label)
        )
      }
    }
  }
})

test_that("scalar mean families and smoothing uncertainty use the selected scale", {
  d <- ale_test_data()
  families <- list(
    binomial(),
    Gamma(link = "inverse"),
    mgcv::betar(link = "probit")
  )
  for (j in seq_along(families)) {
    d$y <- withr::with_seed(
      420,
      switch(
        j,
        rbinom(nrow(d), 1, plogis(-1 + d$x)),
        rgamma(nrow(d), 8, scale = 1 / (8 * (1 + .3 * d$x))),
        rbeta(nrow(d), 4 + d$x, 5)
      )
    )
    m <- mgcv::gam(
      y ~ s(x, k = 4) + z,
      data = d,
      family = families[[j]],
      method = "REML"
    )
    for (unconditional in c(FALSE, TRUE)) {
      a <- ale(
        m,
        "x",
        n_bins = 5,
        scale = "response",
        unconditional = unconditional
      )
      transform <- family(m)$linkinv
      expect_equal(a$.ale, ale_reference(m, d, "x", a$.value, transform))
      h <- 1e-5
      J <- vapply(
        seq_along(coef(m)),
        function(k) {
          hi <- lo <- m
          hi$coefficients[k] <- coef(m)[k] + h
          lo$coefficients[k] <- coef(m)[k] - h
          (ale_reference(hi, d, "x", a$.value, transform) -
            ale_reference(lo, d, "x", a$.value, transform)) /
            (2 * h)
        },
        numeric(nrow(a))
      )
      V <- vcov(m, unconditional = unconditional)
      expect_equal(a$.se, sqrt(diag(J %*% V %*% t(J))), tolerance = 1e-7)
    }
  }
})
