skip_if_not_installed("scam", minimum_version = "1.2-22")

scam_empty_fixture <- local({
  fixture <- NULL
  function() {
    if (is.null(fixture)) {
      s <- mgcv::s
      d <- withr::with_seed(515, {
        d <- data.frame(x = runif(100, .1, .9),
          f = factor(rep(c("a", "b"), each = 50)))
        d$y <- 2 + exp(d$x) + .4 * as.numeric(d$f) + rnorm(100, sd = .02)
        d
      })
      model <- scam::scam(y ~ f + s(x, by = f, bs = "mpi", k = 6),
        data = d, sp = c(.1, .1))
      fixture <<- list(model = model, data = d)
    }
    fixture
  }
})

test_that("zero-row SCAM inputs retain typed flat and nested smooth results", {
  fixture <- scam_empty_fixture()
  d <- fixture$data[0, ]
  flat <- smooth_estimates(fixture$model, data = d, overall_uncertainty = FALSE)
  nested <- smooth_estimates(fixture$model, data = d, unnest = FALSE,
    overall_uncertainty = FALSE)
  expect_s3_class(flat, "smooth_estimates")
  expect_equal(nrow(flat), 0L)
  expect_false("data" %in% names(flat))
  expect_identical(flat$.estimate, double())
  expect_identical(flat$.se, double())
  expect_identical(flat$x, double())
  expect_identical(flat$f, d$f)
  expect_identical(nested$.smooth, smooths(fixture$model))
  expect_identical(vapply(nested$data, nrow, integer(1)), c(0L, 0L))
  for (values in nested$data) {
    expect_identical(values$.estimate, double())
    expect_identical(values$.se, double())
    expect_identical(values$x, double())
    expect_identical(values$f, d$f)
  }
  expect_equal(tidyr::unnest(nested, "data"), flat, ignore_attr = TRUE)
})

test_that("SCAM factor-by levels absent from prediction data keep an empty record", {
  fixture <- scam_empty_fixture()
  d <- fixture$data[1:20, ]
  flat <- smooth_estimates(fixture$model, select = 2, data = d,
    overall_uncertainty = FALSE)
  nested <- smooth_estimates(fixture$model, select = 2, data = d, unnest = FALSE,
    overall_uncertainty = FALSE)
  expect_equal(nrow(flat), 0L)
  expect_false("data" %in% names(flat))
  expect_identical(flat$.estimate, double())
  expect_identical(flat$.se, double())
  expect_identical(flat$x, double())
  expect_identical(flat$f, d$f[0L])
  expect_equal(nrow(nested), 1L)
  expect_identical(nested$.smooth, smooths(fixture$model)[2])
  expect_identical(nested$.by, "f")
  expect_equal(nrow(nested$data[[1L]]), 0L)
  expect_identical(nested$data[[1L]]$.estimate, double())
  expect_identical(nested$data[[1L]]$.se, double())
  expect_identical(nested$data[[1L]]$f, d$f[0L])
  expect_equal(tidyr::unnest(nested, "data"), flat, ignore_attr = TRUE)
})
