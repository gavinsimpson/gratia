skip_if_not_installed("mirai")
skip_if_not_installed("carrier")

# set up daemons
# ensures only 1 additional process on CRAN
mirai::daemons(1, dispatcher = FALSE, autoexit = NA)
on.exit(mirai::daemons(0), add = TRUE)

# Namespaced functions sent to workers resolve against the worker's package
# library. Match the parent instead of silently using an older installed build
# when the parent is testing a development checkout with load_all().
worker_setup <- mirai::everywhere({
  .libPaths(gratia_test_libraries)
  if (gratia_test_development) {
    pkgload::load_all(gratia_test_path, quiet = TRUE, helpers = FALSE)
  } else {
    loadNamespace("gratia", lib.loc = dirname(gratia_test_path))
  }
  invisible(NULL)
}, gratia_test_libraries = .libPaths(),
  gratia_test_development = pkgload::is_dev_package("gratia"),
  gratia_test_path = getNamespaceInfo(asNamespace("gratia"), "path"))
invisible(mirai::collect_mirai(worker_setup, options = ".stop"))

test_that("parallel workers use the package under test", {
  path <- mirai::mirai(getNamespaceInfo(asNamespace("gratia"), "path"))[]
  expect_identical(normalizePath(path),
    normalizePath(getNamespaceInfo(asNamespace("gratia"), "path")))
})

test_that("parallel smooth estimates works", {
  sm <- smooth_estimates(m_gam)
  expect_s3_class(sm, "smooth_estimates")
})

test_that("parallel smooth estimates works", {
  bf <- basis(m_gam)
  expect_s3_class(bf, "basis")
})

test_that("parallel evaluation forwards local expression environments", {
  d <- transformed_data()
  fun <- function(x) log(x + 1)
  m <- mgcv::gam(y ~ s(fun(x), k = 5) + s(sqrt(z), k = 5), data = d)
  a <- smooth_estimates(m, data = d, envir = environment(), overall_uncertainty = FALSE)
  reference <- d; reference[["fun(x)"]] <- fun(d$x)
  reference[["sqrt(z)"]] <- sqrt(d$z)
  ref <- predict(m, reference, type = "terms", newdata.guaranteed = TRUE)
  for (j in seq_along(m$smooth)) {
    expect_equal(a$.estimate[a$.smooth == m$smooth[[j]]$label], as.numeric(ref[, j]))
  }
  expect_s3_class(basis(m, data = d, envir = environment()), "basis")
})


test_that("parallel automatic raw grids retain custom tensor coordinates", {
  d <- transformed_data()
  foo <- sqrt
  m <- mgcv::gam(y ~ te(foo(x), sqrt(z), k = c(4, 4)), data = d)
  sm <- smooth_estimates(m, n_2d = 6, back_transform = TRUE, envir = environment())
  p <- draw(sm, back_transform = TRUE, contour = FALSE)[[1L]]
  expect_equal(sort(unique(p$data$x)), seq(min(d$x), max(d$x), length.out = 6))
  expect_equal(sort(unique(p$data$z)), seq(min(d$z), max(d$z), length.out = 6))
  expect_equal(sm$`foo(x)`, foo(p$data$x))
  expect_equal(sm$`sqrt(z)`, sqrt(p$data$z))
})
