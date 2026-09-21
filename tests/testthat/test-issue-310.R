test_that("issue 310: discrete bam parametric effects respect the fitting subset", {
  withr::local_seed(310)
  d <- data.frame(
    x = rnorm(100),
    f1 = factor(sample(1:2, size = 100, replace = TRUE)),
    f2 = factor(sample(paste0("T_", 1:4), size = 100, replace = TRUE)),
    y = rnorm(100)
  )
  m <- mgcv::bam(
    y ~ f1 + s(x, by = f1, k = 10, bs = "tp") +
      s(x, f2, k = 10, bs = "fs", xt = list(bs = "cr")),
    data = d, method = "fREML", subset = f2 != "T_1", discrete = TRUE
  )
  fitting_data <- model.frame(m)
  expect_lt(nrow(fitting_data), nrow(d))
  expect_false("T_1" %in% levels(fitting_data$f2))

  # Recovering the full data reintroduces an unseen factor level. Older mgcv
  # drops those prediction rows; newer mgcv warns, so require silence too.
  expect_silent(effects <- parametric_effects(m))
  expect_equal(effects, parametric_effects(m, data = fitting_data))
  expect_equal(effects$.level, unique(as.character(fitting_data$f1)))
  expect_true(all(is.finite(effects$.partial)))
  expect_true(all(is.finite(effects$.se)))

  expect_silent(p <- draw(m, parametric = TRUE, n = 10))
  expect_s3_class(p, "patchwork")
  withr::local_pdf(NULL)
  expect_silent(built <- patchwork::patchworkGrob(p))
  expect_s3_class(built, "gtable")
})
