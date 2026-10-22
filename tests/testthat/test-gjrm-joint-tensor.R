skip_if_not_installed("GJRM", minimum_version = "0.2-6")

gjrm_joint_tensor_fixture <- local({
  model <- NULL
  function() {
    if (is.null(model)) {
      te <- mgcv::te
      s <- mgcv::s
      d <- withr::with_seed(2086, {
        n <- 180L
        d <- data.frame(x = runif(n), z = runif(n))
        d$y1 <- sin(5 * d$x) * cos(3 * d$z) + rnorm(n, sd = .5)
        d$y2 <- .3 + cos(4 * d$z) + .2 * d$y1 + rnorm(n)
        d
      })
      formulas <- list(y1 ~ te(x, z, k = c(4, 4)), y2 ~ s(z, k = 4))
      model <<- withr::with_seed(2087, do.call(GJRM::gjrm,
        list(formula = formulas, data = d, margins = c("N", "N"), model = "B")))
    }
    model
  }
})

test_that("joint tensor estimates use the final tensor coefficients and covariance", {
  m <- gjrm_joint_tensor_fixture()
  d <- data.frame(x = c(.7, .2, .7, .4), z = c(.8, .3, .1, .6))
  sm <- m$gam1$smooth[[1]]
  j <- sm$first.para:sm$last.para
  X <- mgcv::PredictMat(sm, d)
  for (frequentist in c(FALSE, TRUE)) {
    values <- smooth_estimates(m, select = "te(x,z)", data = d,
      overall_uncertainty = FALSE, frequentist = frequentist)
    V <- if (frequentist) m$Ve else m$Vb
    expect_equal(values$.estimate, drop(X %*% m$coefficients[j]))
    expect_equal(values$.se,
      unname(sqrt(rowSums((X %*% V[j, j, drop = FALSE]) * X))))
    expect_identical(values$x, d$x)
    expect_identical(values$z, d$z)
    expect_identical(unique(values$.model), "Response 1: Mean (mu)")
  }
})

test_that("joint tensor estimates retain plotting metadata and reuse surface plots", {
  withr::local_pdf(NULL)
  m <- gjrm_joint_tensor_fixture()
  component <- gjrm_gamlss_components(m)[[1]]
  for (unnest in c(TRUE, FALSE)) {
    values <- smooth_estimates(m, select = "te(x,z)", n_2d = 6,
      dist = 0, unnest = unnest)
    reference <- smooth_estimates(component, n_2d = 6, dist = 0,
      unnest = unnest)
    expect_true(is.list(attr(values, "coordinate_info")))
    for (attribute in c("tensor_term_order", "surface_support", "coordinate_info")) {
      expect_identical(attr(values, attribute), attr(reference, attribute))
    }
    saved <- draw(values, contour = FALSE)
    expect_identical(saved[[1]]$labels$subtitle, "Response 1: Mean (mu)")
    expect_s3_class(saved[[1]]$layers[[1]]$geom, "GeomRaster")
    expect_silent(patchwork::patchworkGrob(saved))
  }
  joint <- assemble(m, select = "te(x,z)", n_2d = 6,
    dist = 0, rug = FALSE, contour = FALSE)
  reference <- assemble(component, n_2d = 6, dist = 0,
    rug = FALSE, contour = FALSE)
  expect_identical(names(joint), "te(x,z)")
  expect_equal(joint[[1]]$data, reference[[1]]$data)
  expect_equal(joint[[1]]$labels, reference[[1]]$labels)
  expect_s3_class(joint[[1]]$layers[[1]]$geom, "GeomRaster")
  expect_silent(ggplot2::ggplotGrob(joint[[1]]))
  expect_silent(patchwork::patchworkGrob(draw(m, n_2d = 6, n = 8,
    dist = 0, rug = FALSE, contour = FALSE)))
})
