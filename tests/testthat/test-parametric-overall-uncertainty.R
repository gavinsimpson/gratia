test_that("parametric intervals include intercept covariance without shifting effects", {
  withr::local_seed(52)
  d <- data.frame(x = rnorm(200), z = runif(200),
    a = factor(rep(c("A", "B"), 100)))
  d$y <- 2 + d$x + as.integer(d$a) + rnorm(200)
  nd <- d[1:8, ]
  nd$x[1] <- 0
  for (form in list(y ~ x * a + poly(z, 2), y ~ 0 + x * a + poly(z, 2),
      y ~ x * a + s(z, k = 5))) {
    m <- gam(form, data = d, method = "REML")
    for (unconditional in c(FALSE, TRUE)) {
      if (unconditional && is.null(m$Vc)) next
      X <- predict(m, nd, type = "lpmatrix")
      V <- vcov(m, unconditional = unconditional)
      for (term in names(parametric_terms(m))) {
        pe <- parametric_effects(m, terms = term, data = nd,
          unconditional = unconditional)
        old <- parametric_effects(m, terms = term, data = nd,
          unconditional = unconditional, overall_uncertainty = FALSE)
        expect_equal(pe$.partial, old$.partial)
        # Select rows retained by the factor-only component.
        rows <- if (term == "a") match(pe$.level, nd$a) else seq_len(nrow(nd))
        index <- match(term, attr(m$pterms, "term.labels"))
        cols <- which(m$assign %in% c(0L, index))
        Xi <- X[rows, cols, drop = FALSE]
        expected <- sqrt(rowSums((Xi %*% V[cols, cols, drop = FALSE]) * Xi))
        expect_equal(pe$.se, unname(expected))
        reference <- predict(m, nd, type = "terms", se.fit = TRUE,
          unconditional = unconditional)
        expect_equal(old$.se, unname(reference$se.fit[rows, term]))
        if (!any(m$assign == 0L)) expect_equal(pe$.se, old$.se)
        if (term == "x" && any(m$assign == 0L)) {
          expect_equal(old$.se[1], 0)
          expect_gt(pe$.se[1], 0)
        }
      }
    }
  }
  m <- gam(y ~ x + a, data = d)
  pe <- parametric_effects(m, terms = "a", data = nd)
  expect_gt(pe$.se[pe$.level == "A"], 0)
  for (overall in c(FALSE, TRUE)) {
    pe <- parametric_effects(m, terms = "x", data = nd,
      overall_uncertainty = overall)
    assembled <- assemble(m, terms = "x", data = nd,
      overall_uncertainty = overall)[["x"]]$data
    plotted <- draw(m, terms = "x", data = nd,
      overall_uncertainty = overall)[[1]]$data
    expect_equal(assembled$.se, pe$.se)
    expect_equal(plotted$.se, pe$.se)
  }
})

test_that("each linear predictor supplies only its own intercept uncertainty", {
  withr::local_seed(12)
  d <- data.frame(x = rnorm(200), z = runif(200))
  d$y <- d$x + rnorm(200)
  for (form in list(list(y ~ x * z, ~ x * z), list(y ~ x * z, ~ 0 + x * z))) {
    m <- gam(form, family = gaulss(), data = d)
    X <- predict(m, d, type = "lpmatrix")
    V <- vcov(m)
    pe <- parametric_effects(m, data = d)
    for (j in 1:2) {
      labs <- attr(m$pterms[[j]], "term.labels")
      for (i in seq_along(labs)) {
        term <- if (j == 1L) labs[i] else paste0(labs[i], ".1")
        cols <- which(m$assign[[j]] %in% c(0L, i)) +
          attr(m$nsdf, "pstart")[j] - 1L
        Xi <- X[, cols, drop = FALSE]
        expect_equal(pe$.se[pe$.term == term],
          unname(sqrt(rowSums((Xi %*% V[cols, cols, drop = FALSE]) * Xi))))
      }
    }
    generated <- parametric_effects(m, terms = "x:z.1", n_2d = 4)
    supplied <- parametric_effects(m, terms = "x:z.1", data = generated)
    expect_equal(generated$.se, supplied$.se)
  }
})

test_that("unavailable smoothing covariance warns once and uses Bayesian covariance", {
  withr::local_seed(7)
  d <- data.frame(x = rnorm(80), y = rnorm(80))
  m <- gam(y ~ x, data = d)
  warnings <- character()
  pe <- withCallingHandlers(parametric_effects(m, unconditional = TRUE),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_length(warnings, 1L)
  expect_match(warnings, "[Cc]ovariance")
  expect_equal(pe$.se, parametric_effects(m)$.se)
})
