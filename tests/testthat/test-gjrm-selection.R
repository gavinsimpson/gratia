skip_if_not_installed("GJRM", minimum_version = "0.2-6")

gjrm_selection_fixture <- local({
  models <- list()
  function(repeated = FALSE) {
    key <- as.character(repeated)
    if (is.null(models[[key]])) {
      d <- withr::with_seed(82, {
        d <- data.frame(x = runif(100), z = runif(100), a = runif(100))
        d$y <- sin(6 * d$x) + .4 * d$a + rnorm(100, sd = exp(.3 * d$z))
        d
      })
      second <- if (repeated) ~ s(x, k = 4) else ~ s(z, k = 4)
      models[[key]] <<- GJRM::gamlss(list(y ~ a + s(x, k = 4), second), data = d)
    }
    models[[key]]
  }
})

test_that("GJRM smooth selection is resolved across equations before drawing", {
  model <- gjrm_selection_fixture()
  expect_identical(smooths(model), c("s(x)", "s(z)"))
  all <- draw(model, parametric = FALSE, wrap = FALSE, n = 8)
  expect_length(all, 2L)
  expect_true(all(vapply(all, inherits, logical(1), "ggplot")))
  for (select in list("s(z)", 2, c(FALSE, TRUE))) {
    selected <- draw(model, select = select, wrap = FALSE, n = 8)
    expect_length(selected, 1L)
    expect_identical(names(selected), "s(z)")
    expect_equal(selected[[1]]$data, all[[2]]$data)
    expect_identical(selected[[1]]$labels$subtitle, all[[2]]$labels$subtitle)
  }
  expect_length(draw(model, select = "s(z", partial_match = TRUE,
    wrap = FALSE, n = 8), 1L)
  expect_error(draw(model, select = "s(nope)"), "Failed to match")
  expect_error(draw(model, select = c("s(x)", "s(nope)")), "not found")
  expect_error(draw(model, select = 3), "number of smooths")
  expect_error(draw(model, select = 0), "positive integers")
  expect_error(draw(model, select = 1.5), "positive integers")
  expect_error(draw(model, select = NA_integer_), "without missing values")
  expect_error(draw(model, select = TRUE), "length\\(select\\)")
})

test_that("GJRM repeated smooth labels have discoverable distinct selection keys", {
  model <- gjrm_selection_fixture(TRUE)
  expect_identical(smooths(model), c("s(x)", "s.1(x)"))
  all <- draw(model, parametric = FALSE, wrap = FALSE, n = 8)
  expect_identical(names(all), smooths(model))
  for (select in list("s.1(x)", 2)) {
    selected <- draw(model, select = select, wrap = FALSE, n = 8)
    expect_length(selected, 1L)
    expect_equal(selected[[1]]$data, all[[2]]$data)
  }
  first <- draw(model, select = "s(x)", wrap = FALSE, n = 8)
  expect_length(first, 1L)
  expect_equal(first[[1]]$data, all[[1]]$data)
  # Reserve labels already present in another equation before adding suffixes.
  collision <- model
  collision$gam1$smooth <- c(collision$gam1$smooth, collision$gam1$smooth)
  collision$gam1$smooth[[2]]$label <- "s.1(x)"
  expect_identical(smooths(collision), c("s(x)", "s.1(x)", "s.2(x)"))
})

test_that("GJRM wrap and parametric choices preserve plots and simultaneous intervals", {
  model <- gjrm_selection_fixture()
  plain <- draw(model, wrap = FALSE, n = 8)
  expect_length(plain, 3L)
  expect_true(all(vapply(plain, inherits, logical(1), "ggplot")))
  expect_s3_class(draw(model, n = 8), "patchwork")
  wrapped <- draw(model, wrap = TRUE, n = 8, guides = "collect")
  expect_length(wrapped, length(plain))
  expect_identical(wrapped$patches$layout$guides, "collect")
  expect_silent(patchwork::patchworkGrob(wrapped))
  selected <- draw(model, select = "s(z)", parametric = TRUE, terms = "a",
    wrap = FALSE, n = 8)
  expect_length(selected, 2L)
  expect_setequal(names(selected), c("s(z)", "a"))
  expect_error(draw(model, parametric = TRUE, terms = "missing"), "No parametric terms")
  expect_length(draw(model, parametric = FALSE, terms = "missing",
    wrap = FALSE, n = 8), 2L)
  for (wrap in c(FALSE, TRUE)) {
    selected <- draw(model, select = 2, interval = "simultaneous", n_sim = 100,
      seed = 9, wrap = wrap, n = 8)
    component <- gjrm_gamlss_components(model)[[2]]
    expected <- draw(component, parametric = FALSE, interval = "simultaneous",
      n_sim = 100, seed = 9, wrap = FALSE, n = 8)
    expect_equal(selected[[1]]$data, expected[[1]]$data)
  }
})

test_that("GJRM models without smooths can still draw parametric effects", {
  d <- withr::with_seed(83, data.frame(x = runif(70), y = rnorm(70)))
  model <- GJRM::gamlss(list(y ~ x, ~ 1), data = d)
  expect_identical(smooths(model), character())
  expect_length(draw(model, wrap = FALSE, n = 8), 1L)
  expect_s3_class(draw(model, n = 8), "patchwork")
})
