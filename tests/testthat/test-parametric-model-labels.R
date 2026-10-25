# The full suite checks combinations across models, families and plot options.
# Small function checks remain in test-function-core.R and focused test files.
skip_on_cran()

parametric_label_data <- function() {
  withr::with_seed(430, {
    d <- data.frame(x = runif(200), z = runif(200), w = runif(200),
      f = factor(rep(c("a", "b"), 100)))
    d$y <- 1 + d$x + sin(6 * d$z) +
      rnorm(nrow(d), sd = exp(.2 * d$x))
    d
  })
}

parametric_label_model <- local({
  model <- NULL
  function() {
    if (is.null(model)) {
      d <- parametric_label_data()
      model <<- mgcv::gam(list(y ~ x + f + s(z, k = 4),
        ~ x + f + s(z, k = 4)), family = mgcv::gaulss(), data = d)
    }
    model
  }
})

test_that("parametric predictor labels survive selection and saved estimates", {
  m <- parametric_label_model()
  labels <- c(x = "Mean (mu)", f = "Mean (mu)",
    x.1 = "Standard deviation (sigma)", f.1 = "Standard deviation (sigma)")
  direct <- assemble(m, n = 8, rug = FALSE)
  drawn <- draw(m, n = 8, rug = FALSE, wrap = FALSE)
  for (term in names(labels)) {
    expect_identical(direct[[term]]$labels$subtitle, unname(labels[term]))
    expect_identical(drawn[[term]]$labels$subtitle, unname(labels[term]))
  }
  for (unnest in c(TRUE, FALSE)) {
    e <- parametric_effects(m, unnest = unnest)
    expect_identical(e$.model, unname(labels[e$.term]))
    if (!unnest) expect_false(any(vapply(e$data, function(x)
      ".model" %in% names(x), logical(1))))
    saved <- draw(e, rug = FALSE)
    for (i in seq_len(length(saved))) {
      term <- unique(saved[[i]]$data$.term)
      expect_identical(saved[[i]]$labels$subtitle, unname(labels[term]))
      expect_equal(saved[[i]]$data$.partial, direct[[term]]$data$.partial)
      expect_equal(saved[[i]]$data$.se, direct[[term]]$data$.se)
      expect_identical(saved[[i]]$labels$caption, direct[[term]]$labels$caption)
    }
    selected <- parametric_effects(m, terms = "x.1", unnest = unnest)
    expect_identical(draw(selected)[[1]]$labels$subtitle, unname(labels["x.1"]))
    # Older saved estimates have no predictor metadata.
    e$.model <- NULL
    expect_null(draw(e)[[1]]$labels$subtitle)
  }
  expect_identical(assemble(m, select = 2, parametric = TRUE,
    terms = "x.1", n = 8)$x.1$labels$subtitle, unname(labels["x.1"]))
})

test_that("parametric subtitle overrides work through both drawing routes", {
  m <- parametric_label_model()
  for (subtitle in list("Custom", "", expression(sigma), NULL)) {
    expected <- if (is.null(subtitle)) "Standard deviation (sigma)" else subtitle
    expect_identical(assemble(m, terms = "x.1", n = 8,
      subtitle = subtitle)$x.1$labels$subtitle, expected)
    expect_identical(draw(m, terms = "x.1", n = 8, subtitle = subtitle,
      wrap = FALSE)$x.1$labels$subtitle, expected)
    expect_identical(draw(parametric_effects(m, terms = "x.1"),
      subtitle = subtitle)[[1]]$labels$subtitle, expected)
  }
})

test_that("parametric labels use coefficient membership rather than suffixes", {
  d <- parametric_label_data()
  d$category <- withr::with_seed(431, sample(0:2, nrow(d), replace = TRUE))
  m <- mgcv::gam(list(category ~ 1, ~ 1, 1 + 2 ~ x - 1),
    family = mgcv::multinom(K = 2), data = d)
  expected <- "Category 1 (log odds); Category 2 (log odds)"
  expect_named(parametric_terms(m), "x.2")
  expect_identical(assemble(m)$x.2$labels$subtitle, expected)
  expect_identical(draw(parametric_effects(m))[[1]]$labels$subtitle, expected)
  m$family$family <- "unknown family"
  expect_identical(unname(parametric_model_labels(m, "x.2")),
    "Linear predictor 1; Linear predictor 2")
  m <- mgcv::gam(list(y ~ 1, ~ x - 1), family = mgcv::gaulss(), data = d)
  expect_identical(assemble(m)$x.1$labels$subtitle, "Standard deviation (sigma)")
})

test_that("parametric interactions and reserved covariates retain model labels", {
  d <- parametric_label_data()
  d$.model <- d$x
  d$g <- factor(rep(c("c", "d"), each = 100))
  for (term in c("x:z", "x:f", "f:g", ".model:z")) {
    m <- mgcv::gam(list(stats::as.formula(paste("y ~", term)), ~ x),
      family = mgcv::gaulss(), data = d)
    direct <- assemble(m, n = 8, n_2d = 4, rug = FALSE)[[term]]
    expect_identical(direct$labels$subtitle, "Mean (mu)")
    for (unnest in c(TRUE, FALSE)) {
      e <- parametric_effects(m, terms = term, n = 8, n_2d = 4, unnest = unnest)
      p <- draw(e, rug = FALSE)[[1]]
      expect_identical(p$labels$subtitle, "Mean (mu)")
      expect_silent(ggplot2::ggplotGrob(p))
      if (term == ".model:z") {
        expect_identical(unname(attr(e, "term_info")[[term]]$columns[".model"]),
          ".model.1")
      }
    }
  }
})

test_that("older saved covariates named .model are not predictor subtitles", {
  # Before predictor labels were retained, `.model` was an ordinary coordinate.
  e <- tibble::tibble(.term = ".model:z", .type = "interaction",
    .model = c(0, 0, 1, 1), z = c(0, 1, 0, 1),
    .partial = c(0, 0, 0, 1), .se = .1)
  info <- list(variables = c(".model", "z"),
    columns = c(.model = ".model", z = "z"), numeric = c(".model", "z"),
    discrete = character(), levels = list(), observed = NULL, dist = 0)
  for (unnest in c(TRUE, FALSE)) {
    saved <- if (unnest) e else tidyr::nest(e,
      data = c(".model", "z", ".partial", ".se"))
    attr(saved, "term_info") <- list(`.model:z` = info)
    class(saved) <- c("parametric_effects", class(saved))
    p <- draw(saved)[[1]]
    expect_null(p$labels$subtitle)
    expect_equal(p$data$.model, e$.model)
    expect_silent(ggplot2::ggplotGrob(p))
    expect_identical(draw(saved, subtitle = "Custom")[[1]]$labels$subtitle,
      "Custom")
  }
})

test_that("mgcv related models keep single predictor plots and custom subtitles", {
  d <- parametric_label_data()
  d$y <- withr::with_seed(435, 2 * d$z + d$x + rnorm(nrow(d), sd = .3))
  fits <- list(
    mgcv::gam(y ~ x + s(z, k = 6), data = d),
    mgcv::bam(y ~ x + s(z, k = 6), data = d),
    mgcv::gamm(y ~ x + s(z, k = 6), data = d),
    gamm4::gamm4(y ~ x + s(z, k = 6), data = d),
    scam::scam(y ~ x + s(z, bs = "mpi", k = 6), data = d),
    mgcv::scasm(y ~ x + s(z, bs = "sc", xt = "m+", k = 6), data = d)
  )
  for (m in fits) {
    expect_null(assemble(m, n = 8)$x$labels$subtitle)
    expect_identical(assemble(m, n = 8, subtitle = "Custom")$x$labels$subtitle,
      "Custom")
  }
  m <- mgcv::scasm(list(y ~ x + s(z, bs = "sc", xt = "m+", k = 6), ~ x),
    family = mgcv::gaulss(), data = d)
  expect_identical(assemble(m, n = 8)$x.1$labels$subtitle,
    "Standard deviation (sigma)")
  expect_false(".model" %in% names(parametric_effects(fits[[1]])))
})

test_that("GJRM parametric labels use parent equations and preserve plot names", {
  skip_if_not_installed("GJRM", minimum_version = "0.2-6")
  d <- parametric_label_data()
  d$y2 <- withr::with_seed(432, .3 * d$y + rnorm(nrow(d)))
  models <- list(
    GJRM::gamlss(list(y ~ x, ~ x), data = d),
    do.call(GJRM::gjrm, list(formula = list(y ~ x, y2 ~ x),
      margins = c("N", "N"), model = "B", data = d))
  )
  for (i in seq_along(models)) {
    m <- models[[i]]
    expected <- if (i == 1L) c("Mean (mu)", "Standard deviation (sigma)") else
      c("Response 1: Mean (mu)", "Response 2: Mean (mu)")
    p <- assemble(m)
    expect_named(p, c("x (equation 1)", "x (equation 2)"))
    for (j in 1:2) {
      expect_identical(p[[j]]$labels$subtitle, expected[j])
      component <- gjrm_gamlss_components(m)[[j]]
      for (unnest in c(TRUE, FALSE)) {
        e <- parametric_effects(component, unnest = unnest)
        expect_identical(draw(e)[[1]]$labels$subtitle, expected[j])
      }
    }
    expect_identical(assemble(m, subtitle = "")[[2]]$labels$subtitle, "")
    expect_identical(draw(m, subtitle = "Custom", wrap = FALSE)[[2]]$labels$subtitle,
      "Custom")
  }
  single <- GJRM::gamlss(list(y ~ x), data = d)
  expect_null(assemble(single)[[1]]$labels$subtitle)
})

test_that("glmmTMB parametric labels identify components without smooths", {
  skip_if_not_installed("glmmTMB", minimum_version = "1.1.15.2")
  d <- parametric_label_data()
  d$y <- withr::with_seed(433, rnbinom(nrow(d), mu = exp(d$x), size = 2))
  m <- glmmTMB::glmmTMB(y ~ x, ziformula = ~x, dispformula = ~x,
    family = glmmTMB::nbinom2(), data = d)
  expected <- c(x = "Conditional model", x.1 = "Zero-inflation model",
    x.2 = "Dispersion model")
  p <- assemble(m)
  for (term in names(expected)) {
    expect_identical(p[[term]]$labels$subtitle, unname(expected[term]))
    for (unnest in c(TRUE, FALSE)) {
      e <- parametric_effects(m, terms = term, unnest = unnest)
      expect_identical(draw(e)[[1]]$labels$subtitle, unname(expected[term]))
    }
  }
  m <- glmmTMB::glmmTMB(y ~ x, family = glmmTMB::nbinom2(), data = d)
  expect_null(assemble(m)$x$labels$subtitle)
  m <- glmmTMB::glmmTMB(y ~ x, ziformula = ~1, family = poisson(), data = d)
  expect_identical(assemble(m)$x$labels$subtitle, "Conditional model")
})

test_that("sdmTMB ordinary terms retain delta labels", {
  skip_if_not_installed("sdmTMB", minimum_version = "1.2.0")
  d <- parametric_label_data()
  d$positive <- withr::with_seed(434, ifelse(runif(nrow(d)) < .65,
    exp(d$x + rnorm(nrow(d), sd = .3)), 0))
  m <- sdmTMB::sdmTMB(positive ~ x, data = d, spatial = "off",
    family = sdmTMB::delta_gamma(),
    control = sdmTMB::sdmTMBcontrol(backend = "rtmb"))
  expected <- c("Delta component 1", "Delta component 2")
  direct <- assemble(m, n = 8)
  for (unnest in c(TRUE, FALSE)) {
    e <- parametric_effects(m, n = 8, unnest = unnest)
    p <- draw(e)
    for (j in 1:2) {
      expect_identical(direct[[j]]$labels$subtitle, expected[j])
      expect_identical(p[[j]]$labels$subtitle, expected[j])
    }
  }
})
