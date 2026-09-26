# Check coefficient blocks independently of predict(type = "terms").
check_parametric_block <- function(model, pe, data, term) {
  X <- predict(model, newdata = data, type = "lpmatrix")
  mm <- model.matrix(delete.response(model$pterms), data,
    contrasts.arg = model$contrasts)
  j <- which(attr(mm, "assign") == match(term, attr(model$pterms, "term.labels")))
  X <- X[, j, drop = FALSE]
  V <- vcov(model)[j, j, drop = FALSE]
  expect_equal(pe$.partial, as.numeric(X %*% coef(model)[j]), tolerance = 1e-10)
  expect_equal(pe$.se, unname(sqrt(rowSums((X %*% V) * X))), tolerance = 1e-10)
}

interaction_data <- function() {
  withr::with_seed(247, {
    d <- expand.grid(a = factor(c("B", "A"), levels = c("B", "A")),
      b = factor(c("low", "high")), c = factor(c("yes", "no")), i = 1:35)
    for (v in c("x", "z", "w", "v", "q")) d[[v]] <- runif(nrow(d), 0.2, 1)
    d$flag <- d$i %% 2L == 0L
    d$y <- d$x * d$z + as.integer(d$a) * d$x + rnorm(nrow(d))
    d
  })
}

test_that("interaction components agree with coefficient blocks and fitted contrasts", {
  d <- interaction_data()
  for (contrast in c("contr.treatment", "contr.sum", "contr.helmert")) {
    m <- gam(y ~ x * z + x * a + a * b, data = d,
      contrasts = list(a = contrast, b = contrast))
    for (term in c("x:z", "x:a", "a:b")) {
      pe <- parametric_effects(m, terms = term, data = d)
      info <- attr(pe, "term_info")[[term]]
      # Reconstruct rows, including factor terms deduplicated by covariates.
      nd <- d[rep(1L, nrow(pe)), ]
      for (v in info$variables) nd[[v]] <- pe[[info$columns[[v]]]]
      check_parametric_block(m, pe, nd, term)
    }
  }
  d$a <- ordered(d$a)
  m <- gam(y ~ x * a, data = d)
  pe <- parametric_effects(m, terms = "x:a", data = d)
  check_parametric_block(m, pe, d, "x:a")
  expect_true(is.ordered(pe$a))
})

test_that("transformed and polynomial interactions retain raw axes", {
  d <- interaction_data()
  m <- gam(y ~ poly(x, 3) * a + log(z):w + x + I(x^2), data = d)
  for (term in c("poly(x, 3):a", "log(z):w")) {
    pe <- parametric_effects(m, terms = term, data = d)
    check_parametric_block(m, pe, d, term)
  }
  pe <- parametric_effects(m, terms = c("poly(x, 3)", "x", "I(x^2)"), data = d)
  expect_identical(unique(pe$.term), c("poly(x, 3)", "x", "I(x^2)"))
  expect_error(parametric_effects(m, terms = "poly(x, 3):a",
    data = d, transform = TRUE),
    "not supported for multivariate")
})

test_that("higher-dimensional grids allocate numeric axes and facets", {
  d <- interaction_data()
  cases <- list("x:z" = c(5, 5), "x:z:w" = c(5, 5, 3),
    "x:z:w:v" = c(5, 5, 2, 2), "x:z:w:v:q" = c(5, 5, 2, 2, 2),
    "x:z:a:b" = c(5, 5, 2, 2), "a:x" = c(7, 2),
    "a:x:b:c" = c(7, 2, 2, 2), "a:b:c" = c(2, 2, 2))
  for (term in names(cases)) {
    m <- gam(as.formula(paste("y ~", term)), data = d)
    pe <- parametric_effects(m, n = 7, n_2d = 5, n_3d = 3, n_4d = 2)
    info <- attr(pe, "term_info")[[term]]
    expect_equal(nrow(pe), prod(cases[[term]]), info = term)
    expect_equal(unname(vapply(pe[unname(info$columns)],
      function(x) length(unique(x)), integer(1))), cases[[term]], info = term)
    plt <- draw(pe, contour = FALSE, rug = FALSE)[[1]]
    expect_s3_class(ggplot_build(plt), "ggplot_built")
    if (length(info$numeric) >= 2L) {
      expect_true(inherits(plt$layers[[1]]$geom, "GeomRaster"))
    }
    if (length(info$variables) > 2L) {
      expect_gt(nrow(ggplot_build(plt)$layout$layout), 1L)
    }
  }
})

test_that("multiple linear predictors include and select interactions", {
  df <- data_sim("eg1", seed = 42)
  m <- gam(list(y ~ x0 * x1, ~ x0 * x1), family = gaulss(), data = df)
  expect_silent(pe <- parametric_effects(m, data = df))
  expect_setequal(unique(pe$.term), names(parametric_terms(m)))
  pred <- predict(m, newdata = df, type = "terms", se.fit = TRUE)
  for (term in colnames(pred$fit)) {
    effect <- pe[pe$.term == term, ]
    expect_equal(effect$.partial, unname(pred$fit[, term]))
    expect_equal(effect$.se, unname(pred$se.fit[, term]))
  }
  expect_identical(unique(parametric_effects(m, terms = "x0:x1.1")$.term),
    "x0:x1.1")
  m <- gam(list(y ~ x0:x1, ~ 1), family = gaulss(), data = df)
  expect_s3_class(draw(parametric_effects(m), contour = FALSE), "patchwork")
  df$count <- withr::with_seed(42, rpois(nrow(df), exp(1 + df$x0 * df$x1)))
  m <- gam(count ~ x0 * x1, family = nb(), data = df)
  expect_identical(unique(parametric_effects(m, terms = "x0:x1")$.term), "x0:x1")
})

test_that("supplied grids, reserved names and metadata survive selection and nesting", {
  d <- interaction_data()
  d$.partial <- d$x
  d$`odd.name` <- d$z
  m <- gam(y ~ .partial * `odd.name` + a:b, data = d)
  nd <- d[1:12, ]
  pe <- parametric_effects(m, data = nd)
  info <- attr(pe, "term_info")
  term <- names(info)[1L]
  expect_false(info[[term]]$columns[[".partial"]] == ".partial")
  selected <- pe[pe$.term == term, ]
  expect_identical(names(attr(selected, "term_info")), term)
  expect_equal(nrow(selected), nrow(nd))
  expect_equal(selected[[info[[term]]$columns[[".partial"]]]], nd$.partial)
  plt <- draw(selected, contour = FALSE, geom = "tile")[[1]]
  expect_s3_class(ggplot_build(plt), "ggplot_built")
  nested <- parametric_effects(m, unnest = FALSE, n_2d = 5)
  expect_identical(names(attr(nested, "term_info")), names(info))
  expect_s3_class(draw(nested, contour = FALSE), "patchwork")
  m <- gam(y ~ x:flag, data = d)
  pe <- parametric_effects(m, n = 7)
  expect_equal(nrow(pe), 14L)
  expect_true(is.factor(pe$flag))
})

test_that("assembly and direct drawing share interaction displays and transforms", {
  d <- interaction_data()
  m <- gam(y ~ x:z:a + x:a + a:b + s(w, k = 5), data = d)
  terms <- names(parametric_terms(m))
  pe <- parametric_effects(m, terms = terms, n = 7, n_2d = 5)
  direct <- draw(pe, contour = FALSE, constant = 2, fun = exp, rug = FALSE)
  assembled <- assemble(m, parametric = TRUE, terms = terms,
    n = 7, n_2d = 5, contour = FALSE, constant = 2, fun = exp, rug = FALSE)
  for (i in seq_along(terms)) {
    term <- unique(direct[[i]]$data$.term)
    expect_equal(direct[[i]]$data$.partial, assembled[[term]]$data$.partial)
    expect_s3_class(ggplot_build(assembled[[term]]), "ggplot_built")
  }
  info <- attr(pe, "term_info")
  surface <- names(info)[vapply(info, function(x) length(x$numeric) >= 2L,
    logical(1))]
  tile <- draw(pe[pe$.term == surface, ], geom = "tile", contour = FALSE)[[1]]
  expect_true(inherits(tile$layers[[1]]$geom, "GeomTile"))
  expect_error(parametric_effects(m, n_2d = 0), "Grid resolutions")
})

test_that("interaction uncertainty includes smoothing selection when requested", {
  d <- interaction_data()
  m <- gam(y ~ x * a + s(z, k = 6), data = d, method = "REML")
  pe <- parametric_effects(m, terms = "x:a", data = d, unconditional = TRUE)
  X <- predict(m, newdata = d, type = "lpmatrix")
  mm <- model.matrix(m$pterms, d)
  j <- which(attr(mm, "assign") == match("x:a", attr(m$pterms, "term.labels")))
  X <- X[, j, drop = FALSE]
  V <- vcov(m, unconditional = TRUE)[j, j, drop = FALSE]
  expect_equal(pe$.se, unname(sqrt(rowSums((X %*% V) * X))))
})

test_that("interaction rugs use observations and intervals transform together", {
  d <- interaction_data()
  m <- gam(y ~ x:a, data = d)
  pe <- parametric_effects(m, n = 7)
  p <- draw(pe, constant = 2, fun = exp)[[1]]
  expect_equal(p$data$.partial, exp(pe$.partial + 2))
  expect_equal(p$data$.lower_ci, exp(pe$.partial - qnorm(0.975) * pe$.se + 2))
  expect_equal(p$data$.upper_ci, exp(pe$.partial + qnorm(0.975) * pe$.se + 2))
  expect_equal(p$layers[[3]]$data$x, d$x)
  expect_equal(nrow(p$layers[[3]]$data), nrow(d))
  pe <- parametric_effects(m, data = d[1:8, ], n = 0)
  expect_equal(nrow(pe), 8L) # grid resolution is ignored for explicit rows
})

test_that("parametric surface masking changes plots but not estimates", {
  d <- interaction_data()
  d$z <- d$x
  m <- gam(y ~ x:z, data = d)
  pe <- parametric_effects(m, n_2d = 9)
  p <- draw(pe, contour = FALSE)[[1]]
  expect_false(anyNA(pe$.partial))
  expect_true(anyNA(p$data$.partial))
  pe <- parametric_effects(m, n_2d = 9, dist = 0)
  expect_false(anyNA(draw(pe, contour = FALSE)[[1]]$data$.partial))
})

test_that("interaction plots have representative visual coverage", {
  d <- interaction_data()
  for (term in c("x:z", "x:z:w", "x:z:w:v", "x:a:b", "a:b:c")) {
    m <- gam(as.formula(paste("y ~", term)), data = d)
    pe <- parametric_effects(m, n = 20, n_2d = 12, n_3d = 3, n_4d = 2,
      dist = 0)
    p <- draw(pe, contour = FALSE, rug = FALSE)
    expect_doppelganger(paste("parametric component", gsub(":", "-", term)), p)
  }
})


test_that("zero interaction slices retain surfaces without contour warnings", {
  d <- interaction_data()
  m <- gam(y ~ x * z * a, data = d)
  pe <- parametric_effects(m, terms = "x:z:a", n_2d = 15, dist = 0)
  p <- draw(pe, rug = FALSE)[[1]]
  expect_silent(built <- ggplot_build(p))
  expect_length(unique(built$data[[1]]$PANEL), 2L)
  expect_length(unique(built$data[[2]]$PANEL), 1L)
  expect_true(all(p$layers[[2]]$data$a == "A"))
})
