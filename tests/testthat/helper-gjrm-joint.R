# Reuse fitted joint models across the smooth and diagnostic regression tests.
gjrm_joint_fixture <- local({
  models <- list()
  function(kind = c("normal", "binary_cont", "binary", "trivariate",
      "parametric", "empty", "ordinal", "ordinal_pair", "fixed", "selection", "count_cont", "omitted")) {
    kind <- match.arg(kind)
    if (is.null(models[[kind]])) {
      s <- mgcv::s
      d <- withr::with_seed(2084, {
        n <- 180L
        d <- data.frame(x = runif(n), z = runif(n), a = runif(n),
          group = factor(rep(letters[1:9], each = 20)))
        e <- matrix(rnorm(3 * n), ncol = 3)
        d$y1 <- .3 + sin(5 * d$x) + .4 * d$a +
          rep(seq(-.4, .4, length.out = 9), each = 20) + e[, 1]
        d$y2 <- -.2 + cos(4 * d$x) + exp(.2 * d$z) *
          (.25 * e[, 1] + e[, 2])
        d$y3 <- as.numeric(.2 + d$x + .2 * e[, 1] + e[, 3] > 0)
        d$y_count <- rnbinom(n, mu = exp(.3 + .4 * sin(5 * d$x)), size = 2)
        d
      })
      margins <- c("N", "N")
      model <- "B"
      formulas <- switch(kind,
        omitted =,
        normal = list(y1 ~ a + s(x, k = 4) + s(group, bs = "re"),
          y2 ~ s(x, k = 4), ~ s(z, k = 4), ~ s(z, k = 4),
          ~ s(z, k = 4)),
        count_cont = list(y1 ~ s(x, k = 4), y2 ~ s(z, k = 4),
          ~ s(z, k = 4), ~ s(z, k = 4), ~ 1),
        binary_cont = list(y1 ~ a + s(x, k = 4),
          y2 ~ s(x, k = 4), ~ s(z, k = 4), ~ s(z, k = 4)),
        binary = list(y1 ~ s(x, k = 4), y2 ~ s(x, k = 4),
          ~ s(z, k = 4)),
        trivariate = list(y1 ~ s(x, k = 4), y2 ~ s(x, k = 4),
          y3 ~ s(z, k = 4)),
        parametric = list(y1 ~ a, y2 ~ 1),
        empty = list(y1 ~ 1, y2 ~ 1),
        ordinal = list(y1 ~ s(x, k = 4), y2 ~ s(z, k = 4)),
        ordinal_pair = list(y1 ~ s(x, k = 4), y2 ~ s(z, k = 4)),
        fixed = list(y1 ~ 1, y2 ~ 1, ~ s(x, k = 4)),
        selection = list(y1 ~ s(x, k = 4), y2 ~ s(z, k = 4)))
      if (kind %in% c("binary_cont", "binary", "trivariate")) {
        d$y1 <- as.numeric(d$y1 > .6)
        margins[1] <- "probit"
      }
      if (kind %in% c("binary", "trivariate")) {
        d$y2 <- as.numeric(d$y2 > -.2)
        margins[2] <- "logit"
      }
      if (kind == "trivariate") {
        margins <- c("probit", "probit", "probit")
        model <- "T"
      }
      if (kind %in% c("ordinal", "ordinal_pair")) {
        d$y1 <- as.integer(cut(d$y1, c(-Inf, 0, 1, Inf)))
        margins[1] <- "ord.probit"
      }
      if (kind == "ordinal_pair") {
        d$y2 <- as.integer(cut(d$y2, c(-Inf, -.5, .5, Inf)))
        margins[2] <- "ord.logit"
      }
      if (kind == "selection") {
        d$y1 <- as.numeric(d$y1 > .6)
        d$y2[d$y1 == 0] <- NA_real_
        margins[1] <- "probit"
        model <- "BSS"
      }
      if (kind == "count_cont") {
        d$y1 <- d$y_count
        margins[1] <- "NBI"
      }
      if (kind == "omitted") d$x[c(3, 11)] <- NA_real_
      if (kind == "fixed") {
        d$y1 <- pnorm(d$y1)
        d$y2 <- pnorm(d$y2)
      }
      models[[kind]] <<- withr::with_seed(2085, do.call(GJRM::gjrm,
        list(formula = formulas, data = d, margins = margins, model = model,
          fixed.margins = kind == "fixed")))
    }
    models[[kind]]
  }
})
