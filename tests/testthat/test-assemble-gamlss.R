skip_if_not_installed("GJRM", minimum_version = "0.2-6")

assemble_gamlss_fixture <- local({
  models <- list()
  function(kind = c("repeated", "polar", "parametric", "single")) {
    kind <- match.arg(kind)
    if (is.null(models[[kind]])) {
      d <- withr::with_seed(1052, {
        d <- data.frame(x = runif(120), z = runif(120), a = runif(120))
        d$y <- sin(2 * pi * d$x) + d$a +
          rnorm(120, sd = exp(.3 * d$z))
        d
      })
      formulas <- switch(kind,
        repeated = list(y ~ a + s(x, k = 5), ~ s(x, k = 4)),
        polar = list(y ~ s(x, bs = "cc", k = 5), ~ s(z, k = 4)),
        parametric = list(y ~ a, ~ 1),
        single = list(y ~ a + s(x, k = 5))
      )
      models[[kind]] <<- GJRM::gamlss(formulas, data = d)
    }
    models[[kind]]
  }
})

test_that("assemble gamlss returns named plots using final model estimates", {
  m <- assemble_gamlss_fixture()
  expect_identical(getS3method("assemble", "gamlss"), assemble.gamlss)
  plots <- assemble(m, n = 9, rug = FALSE)
  expect_identical(names(plots), c("s(x)", "a", "s.1(x)"))
  expect_true(all(vapply(plots, inherits, logical(1), "ggplot")))
  expect_identical(vapply(plots[c(1, 3)], function(p) p$labels$subtitle,
    character(1)), c(`s(x)` = "Mean (mu)", `s.1(x)` = "Standard deviation (sigma)"))
  components <- gjrm_gamlss_components(m)
  for (i in seq_along(components)) {
    p <- plots[[c(1, 3)[i]]]
    sm <- components[[i]]$smooth[[1]]
    X <- mgcv::PredictMat(sm, p$data)
    cols <- gjrm_gamlss_ind(m, i)[sm$first.para:sm$last.para]
    expect_equal(p$data$.estimate, drop(X %*% m$coefficients[cols]))
    expect_silent(ggplot2::ggplotGrob(p))
  }
  expect_s3_class(draw(m, n = 9, rug = FALSE), "patchwork")
  drawn <- draw(m, n = 9, rug = FALSE, wrap = FALSE)
  expect_identical(names(drawn), names(plots))
  for (i in seq_along(plots)) {
    expect_equal(drawn[[i]]$data, plots[[i]]$data)
    expect_equal(drawn[[i]]$labels, plots[[i]]$labels)
  }
})

test_that("assemble gamlss selects smooth and parametric terms across equations", {
  m <- assemble_gamlss_fixture()
  for (selection in list("s.1(x)", 2, c(FALSE, TRUE))) {
    plots <- assemble(m, select = selection, n = 9, rug = FALSE)
    expect_identical(names(plots), "s.1(x)")
    expect_equal(plots, draw(m, select = selection, n = 9,
      rug = FALSE, wrap = FALSE))
  }
  expect_identical(names(assemble(m, select = "s.1", partial_match = TRUE,
    n = 9)), "s.1(x)")
  expect_identical(names(assemble(m, select = 2, parametric = TRUE,
    terms = "a", n = 9)), c("a", "s.1(x)"))
  expect_identical(names(assemble(m, parametric = FALSE, n = 9)), smooths(m))
  expect_identical(names(assemble(m, select = 2:1, n = 9)), smooths(m))
})

test_that("assemble gamlss forwards prediction rows and interval controls", {
  m <- assemble_gamlss_fixture()
  data <- data.frame(x = c(.8, .2, .8, .4))
  env <- new.env(parent = environment())
  plots <- assemble(m, data = data, envir = env, parametric = FALSE,
    interval = "simultaneous", n_sim = 100, seed = 27, rug = FALSE)
  components <- gjrm_gamlss_components(m)
  for (i in seq_along(components)) {
    expected <- assemble(components[[i]], data = data, envir = env,
      parametric = FALSE, interval = "simultaneous", n_sim = 100,
      seed = 27, rug = FALSE)[[1]]
    expect_equal(plots[[i]]$data, expected$data)
    expect_equal(plots[[i]]$data$x, data$x)
  }
  expect_equal(draw(m, data = data, envir = env, parametric = FALSE,
    interval = "simultaneous", n_sim = 100, seed = 27,
    rug = FALSE, wrap = FALSE), plots)
})

test_that("assemble gamlss shares fixed effect ranges and polar options", {
  m <- assemble_gamlss_fixture("polar")
  args <- list(start = -pi / 2, r.axis.inside = FALSE)
  plots <- assemble(m, cyclic = "polar", polar_args = args,
    scales = "fixed", n = 9, rug = FALSE)
  expect_s3_class(plots[[1]]$coordinates, "CoordRadial")
  expect_false(plots[[1]]$coordinates$r_axis_inside)
  expect_s3_class(plots[[2]]$coordinates, "CoordCartesian")
  expect_equal(range(plots[[1]]$data$x), range(m$gam1$smooth[[1]]$xp))
  expect_equal(plots[[1]]$scales$get_scales("y")$limits,
    plots[[2]]$scales$get_scales("y")$limits)
  expect_equal(draw(m, cyclic = "polar", polar_args = args, scales = "fixed",
    n = 9, rug = FALSE, wrap = FALSE), plots)
  expect_silent(patchwork::patchworkGrob(patchwork::wrap_plots(plots)))
})

test_that("assemble gamlss handles parametric-only single and empty plot lists", {
  m <- assemble_gamlss_fixture("parametric")
  expect_identical(names(assemble(m, n = 9)), "a")
  expect_equal(assemble(m, n = 9), draw(m, n = 9, wrap = FALSE))
  for (fun in list(assemble, draw)) {
    result <- withVisible(fun(m, parametric = FALSE))
    expect_null(result$value)
    expect_false(result$visible)
  }
  single <- assemble(assemble_gamlss_fixture("single"), n = 9)
  expect_identical(names(single), c("s(x)", "a"))
  expect_null(single[[1]]$labels$subtitle)
})

test_that("assemble and draw gamlss validate the same controls", {
  m <- assemble_gamlss_fixture()
  message_for <- function(fun, arguments) {
    tryCatch(do.call(fun, c(list(object = m), arguments)),
      error = conditionMessage)
  }
  for (arguments in list(list(select = "missing"), list(select = 0),
    list(select = NA_integer_), list(select = 1.5), list(select = 3),
    list(select = TRUE), list(terms = "missing"), list(scales = "invalid"),
    list(cyclic = "invalid"), list(polar_args = list(start = 0)))) {
    actual <- message_for(assemble, arguments)
    expect_type(actual, "character")
    expect_identical(actual, message_for(draw, arguments))
  }
})
