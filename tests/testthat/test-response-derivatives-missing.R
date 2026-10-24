# Fixed coefficient draws keep comparisons independent of random-number order.
response_missing_args <- function(model, uncertainty) {
  list(uncertainty = uncertainty, method = "user",
    draws = rbind(coef(model) - .01, coef(model), coef(model) + .02),
    type = "central")
}

response_result_columns <- c(".derivative", ".se", ".lower_ci", ".upper_ci")

test_that("both methods retain missing focal and other covariates in input order", {
  cases <- list(
    list(response_delta_fixture(gaussian()), data.frame(x = c(.5, NA, -.4, .5)), "x"),
    list(sampling_test_model(), data.frame(x = c(.5, .3, .2, NA, .5),
      off = c(.2, NA, .2, .2, .2)), "x"))
  for (case in cases) for (uncertainty in c("simulation", "delta")) {
    m <- case[[1]]
    nd <- case[[2]]
    args <- c(list(m, data = nd, focal = case[[3]]),
      response_missing_args(m, uncertainty))
    valid <- complete.cases(nd)
    out <- do.call(response_derivatives, args)
    expect_identical(out$.row, seq_len(nrow(nd)))
    expect_identical(out$.focal, rep(case[[3]], nrow(nd)))
    for (nm in names(nd)) expect_equal(out[[nm]], nd[[nm]])
    expect_true(all(is.na(as.matrix(out[!valid, response_result_columns]))))
    args$data <- nd[valid, , drop = FALSE]
    compact <- do.call(response_derivatives, args)
    expect_equal(out[valid, response_result_columns],
      compact[response_result_columns], ignore_attr = TRUE, tolerance = 1e-7)

    args$data <- nd
    omit <- do.call(response_derivatives, c(args, list(na.action = na.omit)))
    expect_identical(omit$.row, which(valid))
    expect_equal(omit[response_result_columns], compact[response_result_columns],
      ignore_attr = TRUE, tolerance = 1e-7)
    for (action in list(na.pass, na.exclude)) {
      expect_equal(do.call(response_derivatives, c(args, list(na.action = action))), out)
    }
    expect_error(do.call(response_derivatives, c(args, list(na.action = na.fail))),
      "missing values")

    # No template prediction may survive restoration of an unavailable set.
    args$data <- nd[!valid, , drop = FALSE]
    for (action in list(na.pass, na.exclude)) {
      empty <- do.call(response_derivatives, c(args, list(na.action = action)))
      expect_identical(empty$.row, seq_len(sum(!valid)))
      expect_identical(empty$.focal, rep(case[[3]], sum(!valid)))
      expect_true(all(is.na(as.matrix(empty[response_result_columns]))))
    }
    empty <- do.call(response_derivatives, c(args, list(na.action = na.omit)))
    expect_equal(nrow(empty), 0L)
  }
})

test_that("missing rows preserve categories, responses and grouped families", {
  cases <- list(list(m_ocat, su_eg1_ocat, "x0"),
    list(m_multinom, multinom_df, "x1"), list(m_mvn, mvn_df, "x0"),
    list(gfam_model, gfam_data, "x"),
    list(gfam_ordinal_model, gfam_ordinal_data, "x"))
  for (case in cases) {
    m <- case[[1]]
    focal <- case[[3]]
    nd <- case[[2]][c(3, 1, 2, 3), , drop = FALSE]
    args <- c(list(m, data = nd, focal = focal), response_missing_args(m, "simulation"))
    reference <- do.call(response_derivatives, args)
    ids <- intersect(c(".row", ".focal", ".category", ".y", ".family"), names(reference))
    for (missing in list(2L, seq_len(nrow(nd)))) {
      args$data <- nd
      args$data[[focal]][missing] <- NA_real_
      out <- do.call(response_derivatives, args)
      expect_equal(out[ids], reference[ids])
      unavailable <- out$.row %in% missing
      expect_true(all(is.na(as.matrix(out[unavailable, response_result_columns]))))
      expect_equal(out[!unavailable, response_result_columns],
        reference[!unavailable, response_result_columns],
        ignore_attr = TRUE, tolerance = 1e-6)
      if (length(missing) == 1L) {
        compact <- do.call(response_derivatives,
          c(args, list(na.action = na.omit)))
        expect_equal(compact[ids], reference[!unavailable, ids], ignore_attr = TRUE)
      }
    }
  }
})

test_that("undefined draws at available rows still fail", {
  m <- response_delta_fixture()
  b <- rbind(coef(m), coef(m))
  b[2, ] <- 1e308
  for (all_bad in c(FALSE, TRUE)) {
    if (all_bad) b[1, ] <- b[2, ]
    expect_error(response_derivatives(m, data = data.frame(x = c(.3, NA)),
      focal = "x", method = "user", draws = b), "Non-finite derivative")
  }
  # Delta predictions must also fail when the response calculation is invalid.
  m$family$linkinv <- function(eta) rep(NaN, length(eta))
  expect_error(response_derivatives(m, data = data.frame(x = c(.3, NA)),
    focal = "x", uncertainty = "delta"), "Non-finite response")
})

test_that("scaled t derivatives require a mean only on the response scale", {
  nd <- data.frame(x = c(-.7, -.8))
  for (df in c(.5, 1, 1.5, 3)) {
    m <- response_scat_fixture(df)
    for (uncertainty in c("simulation", "delta")) {
      args <- c(list(m, data = nd, focal = "x"), response_missing_args(m, uncertainty))
      # An independent five-point difference uses native mgcv predictions.
      args$scale <- "linear_predictor"
      out <- do.call(response_derivatives, args)
      ref <- as.numeric(native_response_slope(m, nd, "x", type = "link"))
      # Non-identical draws shift both coefficients, so their median is coef(m).
      expect_equal(out$.derivative, ref, tolerance = 1e-6)
      args$scale <- "response"
      if (df <= 1) {
        expect_error(do.call(response_derivatives, args), "expected response.*df <= 1")
        args$data$x[] <- NA_real_
        expect_error(do.call(response_derivatives, args), "expected response.*df <= 1")
      } else {
        out <- do.call(response_derivatives, args)
        expect_equal(out$.derivative, as.numeric(native_response_slope(m, nd, "x")),
          tolerance = 1e-6)
        expect_true(all(is.finite(as.matrix(out[response_result_columns]))))
      }
    }
  }
  m <- response_scat_fixture(.5, estimated = TRUE)
  expect_lt(family(m)$getTheta(TRUE)[1], 1)
  for (uncertainty in c("simulation", "delta")) {
    args <- c(list(m, data = nd, focal = "x"), response_missing_args(m, uncertainty))
    expect_error(do.call(response_derivatives, args), "expected response.*df <= 1")
    args$scale <- "linear_predictor"
    out <- do.call(response_derivatives, args)
    expect_equal(out$.derivative,
      as.numeric(native_response_slope(m, nd, "x", type = "link")),
      tolerance = 1e-6)
  }
  # The ordinary estimated-df fixture retains valid mean derivatives.
  expect_gt(family(m_scat)$getTheta(TRUE)[1], 1)
  for (uncertainty in c("simulation", "delta")) {
    args <- c(list(m_scat, data = su_eg1[1:2, ], focal = "x0"),
      response_missing_args(m_scat, uncertainty))
    out <- do.call(response_derivatives, args)
    expect_equal(out$.derivative, as.numeric(native_response_slope(m_scat, args$data, "x0")),
      tolerance = 1e-5)
  }
})

test_that("row restoration covers both derivative orders and difference types", {
  m <- response_delta_fixture(gaussian())
  nd <- data.frame(x = c(.3, NA, Inf, -.4, .3))
  for (uncertainty in c("simulation", "delta")) for (order in 1:2) {
    for (type in c("forward", "backward", "central")) {
      args <- c(list(m, focal = "x", data = nd, order = order),
        response_missing_args(m, uncertainty))
      args$type <- type
      out <- do.call(response_derivatives, args)
      expect_identical(out$.row, 1:5)
      expect_true(all(is.na(as.matrix(out[2:3, response_result_columns]))))
      args$data <- nd[c(1, 4, 5), , drop = FALSE]
      ref <- do.call(response_derivatives, args)
      expect_equal(out[c(1, 4, 5), response_result_columns],
        ref[response_result_columns], ignore_attr = TRUE)
    }
  }
})

test_that("explicit omissions require all shifted predictions", {
  at <- function(args) {
    warnings <- 0L
    out <- withCallingHandlers(do.call(response_derivatives, args),
      warning = function(w) {
        if (identical(conditionMessage(w), "NaNs produced")) {
          warnings <<- warnings + 1L
          invokeRestart("muffleWarning")
        }
      })
    expect_gt(warnings, 0L)
    out
  }
  nd <- df_2_fac[1, , drop = FALSE]
  nd$x0 <- .01
  for (uncertainty in c("simulation", "delta")) {
    args <- c(list(m_poly, focal = "x0", data = nd, eps = .1),
      response_missing_args(m_poly, uncertainty))
    # The lower shift takes log(x0) outside its domain. The warning comes
    # from evaluating that model term, before unavailable rows are restored.
    out <- at(args)
    expect_identical(out$.row, 1L)
    expect_true(all(is.na(as.matrix(out[response_result_columns]))))
    out <- at(c(args, list(na.action = na.omit)))
    expect_equal(nrow(out), 0L)
  }
})
