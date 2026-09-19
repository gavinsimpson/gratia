# Test fitted_values()

fv_nms <- c(".fitted", ".se", ".lower_ci", ".upper_ci")

test_that("fitted_values() works for a GAM", {
  expect_silent(fv <- fitted_values(m_gam))

  expect_named(fv, expected = c(".row", "x0", "x1", "x2", "x3", fv_nms))

  expect_s3_class(fv, c("tbl_df", "tbl", "data.frame"))

  expect_identical(nrow(su_eg1), nrow(fv))
})

test_that("fitted_values() scale='response' works for a GAM", {
  expect_silent(fv <- fitted_values(m_gam, scale = "response"))
  expect_silent(fv2 <- fitted_values(m_gam, scale = "linear predictor"))

  expect_named(fv, expected = c(".row", "x0", "x1", "x2", "x3", fv_nms))

  expect_s3_class(fv, c("tbl_df", "tbl", "data.frame"))

  expect_identical(nrow(su_eg1), nrow(fv))

  expect_identical(fv, fv2)
})

test_that("fitted_values() scale='link' works for a GAM", {
  expect_silent(fv <- fitted_values(m_gam, scale = "link"))

  expect_named(fv, expected = c(".row", "x0", "x1", "x2", "x3", fv_nms))

  expect_s3_class(fv, c("tbl_df", "tbl", "data.frame"))

  expect_identical(nrow(su_eg1), nrow(fv))
})

test_that("fitted_values() works for a GAM", {
  new_df <- data_sim("eg1", n = 100, dist = "normal", scale = 2, seed = 1)
  expect_silent(fv <- fitted_values(m_gam, data = new_df))

  expect_named(fv, expected = c(".row", names(new_df), fv_nms))

  expect_s3_class(fv, c("tbl_df", "tbl", "data.frame"))

  expect_identical(nrow(new_df), nrow(fv))
})

test_that("fitted_values() works for an ocat GAM", {
  expect_silent(fv <- fitted_values(m_ocat))

  expect_named(fv, expected = c(
    ".row", "x0", "x1", "x2", "x3", ".category",
    fv_nms
  ))
  expect_s3_class(fv, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(su_eg1_ocat) * 4L, nrow(fv))

  new_df <- data_sim("eg1", n = 50, dist = "ocat", seed = 1)
  expect_silent(fv <- fitted_values(m_ocat, data = new_df))
  expect_named(fv, expected = c(".row", names(new_df), ".category", fv_nms))
  expect_s3_class(fv, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(new_df) * 4L, nrow(fv))

  # link scale
  expect_silent(fv <- fitted_values(m_ocat, scale = "link"))
  expect_named(fv, expected = c(".row", "x0", "x1", "x2", "x3", fv_nms))
  expect_s3_class(fv, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(su_eg1_ocat), nrow(fv))

  new_df <- data_sim("eg1", n = 50, dist = "ocat", seed = 1)
  expect_silent(fv <- fitted_values(m_ocat, data = new_df, scale = "link"))
  expect_named(fv, expected = c(".row", names(new_df), fv_nms))
  expect_s3_class(fv, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(new_df), nrow(fv))
})

test_that("fitted values works for a univariate scam model", {
  expect_silent(fv <- fitted_values(m_scam))
  expect_named(fv, expected = c(".row", "x1", "x2", fv_nms))
})

test_that("fitted values works for a ziplss model", {
  expect_silent(fv <- fitted_values(m_ziplss))
  expect_named(fv, expected = c(".row", ".parameter", fv_nms))
})

test_that("fitted values works for a gaulss model", {
  expect_silent(fv <- fitted_values(m_gaulss))
  expect_named(fv, expected = c(".row", ".parameter", fv_nms))
})

test_that("fitted values works for a twlss model", {
  expect_silent(fv <- fitted_values(m_twlss))
  expect_named(fv, expected = c(".row", ".parameter", fv_nms))
  expect_identical(
    pull(fv, ".parameter") |> unique(),
    c("location", "power", "scale")
  )
})

test_that("fitted values works for a gamm model", {
  expect_silent(fv <- fitted_values(m_gamm))
  expect_named(fv, expected = c(".row", "x0", "x1", "x2", "x3", fv_nms))
})

test_that("fitted values works for a ziP() model", {
  # compare predict(m_ziP, type = "response") with fitted_values(m_ziP)
  f_mgcv <- predict(m_ziP, type = "response") |> as.vector()
  f_fv   <- fitted_values(m_ziP)$.fitted
  expect_identical(f_fv, f_mgcv)

  skip_on_cran()
  skip_on_ci()
  expect_snapshot(fitted_values(m_ziP))
})

test_that("fitted values works for a mvn() model", {
  # compare predict(m_ziP, type = "response") with fitted_values(m_ziP)
  f_mgcv <- predict(m_mvn, type = "response") |> as.vector()
  f_fv   <- fitted_values(m_mvn)$.fitted
  expect_identical(f_fv, f_mgcv)

  skip_on_cran()
  skip_on_ci()
  expect_snapshot(fitted_values(m_mvn))
})

test_that("fitted values works for a multinom() model", {
  # compare predict(m_ziP, type = "response") with fitted_values(m_ziP)
  f_mgcv <- predict(m_multinom, type = "response") |> as.vector()
  f_fv   <- fitted_values(m_multinom)$.fitted
  expect_identical(f_fv, f_mgcv)

  skip_on_cran()
  skip_on_ci()
  expect_snapshot(fitted_values(m_multinom))
})

test_that("distributional fitted intervals match link-scale predictions", {
  models <- list(gaulss = m_gaulss, ziplss = m_ziplss, twlss = m_twlss)
  # Independent response transformations for these default model families.
  transforms <- list(
    gaulss = list(identity, function(x) 1 / (0.01 + exp(x))),
    ziplss = list(exp, function(x) -expm1(-exp(x))),
    twlss = list(exp, function(x) 1.01 + 0.98 * plogis(x), exp)
  )
  for (fam in names(models)) {
    model <- models[[fam]]
    nd <- model.frame(model)[seq_len(6L), , drop = FALSE]
    prediction <- predict(model, newdata = nd, type = "link", se.fit = TRUE)
    for (level in c(0.8, 0.95)) {
      critical <- qnorm((1 + level) / 2)
      lower <- prediction$fit - critical * prediction$se.fit
      upper <- prediction$fit + critical * prediction$se.fit
      fv <- fitted_values(model, data = nd, scale = "link", ci_level = level)
      expect_equal(fv$.fitted, as.vector(t(prediction$fit)))
      expect_equal(fv$.se, as.vector(t(prediction$se.fit)))
      expect_equal(fv$.lower_ci, as.vector(t(lower)))
      expect_equal(fv$.upper_ci, as.vector(t(upper)))
      expect_true(all(fv$.lower_ci <= fv$.fitted & fv$.fitted <= fv$.upper_ci))
      expect_identical(fv, fitted_values(model, data = nd,
        scale = "linear predictor", ci_level = level))

      response <- fitted_values(model, data = nd, ci_level = level)
      parameters <- unique(response$.parameter)
      for (j in seq_along(parameters)) {
        rows <- response$.parameter == parameters[j]
        transform <- transforms[[fam]][[j]]
        # Gaussian LSS's second predictor is precision, a decreasing transform.
        decreasing <- fam == "gaulss" && j == 2L
        expected_lower <- transform(if (decreasing) upper[, j] else lower[, j])
        expected_upper <- transform(if (decreasing) lower[, j] else upper[, j])
        expect_equal(response$.fitted[rows], unname(transform(prediction$fit[, j])))
        expect_equal(response$.lower_ci[rows], unname(expected_lower))
        expect_equal(response$.upper_ci[rows], unname(expected_upper))
      }
      expect_identical(response$.se, fv$.se)
      expect_true(all(response$.lower_ci <= response$.fitted &
        response$.fitted <= response$.upper_ci))
    }
  }
})

test_that("fitted intervals respect increasing and decreasing inverse links", {
  withr::local_seed(1001)
  d <- data.frame(x = seq(0, 1, length.out = 100))
  d$y <- rgamma(nrow(d), shape = 100, scale = exp(d$x) / 100)
  for (link_name in c("log", "inverse")) {
    model <- mgcv::gam(y ~ s(x, k = 5), data = d,
      family = Gamma(link = link_name), method = "REML")
    nd <- d[c(1, 50, 100), , drop = FALSE]
    prediction <- predict(model, newdata = nd, type = "link", se.fit = TRUE)
    for (level in c(0.8, 0.95)) {
      critical <- qnorm((1 + level) / 2)
      lower <- as.vector(prediction$fit - critical * prediction$se.fit)
      upper <- as.vector(prediction$fit + critical * prediction$se.fit)
      fv <- fitted_values(model, data = nd, scale = "link", ci_level = level)
      expect_equal(fv$.lower_ci, lower)
      expect_equal(fv$.upper_ci, upper)
      response <- fitted_values(model, data = nd, ci_level = level)
      if (link_name == "inverse") {
        expect_true(all(lower > 0)) # stay within the monotone link domain
        expect_equal(response$.lower_ci, 1 / upper)
        expect_equal(response$.upper_ci, 1 / lower)
      } else {
        expect_equal(response$.lower_ci, exp(lower))
        expect_equal(response$.upper_ci, exp(upper))
      }
      expect_identical(response$.se, fv$.se)
      expect_true(all(response$.lower_ci <= response$.fitted &
        response$.fitted <= response$.upper_ci))
    }
  }
})
