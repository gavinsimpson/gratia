expect_fixed_response_derivatives <- function(model, data, focal, expected,
                                               scale = "response") {
  # Two identical coefficient draws, one in each chain, have no uncertainty.
  b <- post_draws(model, n_draws = 2, chains = 2, seed = 431)
  b[,] <- rep(coef(model), each = 2)
  args <- list(model, data = data, focal = focal, method = "user",
    draws = b, type = "central", eps = 1e-5, scale = scale)
  samples <- do.call(derivative_samples, args)
  out <- do.call(response_derivatives, args)
  ids <- names(expected)
  ids <- ids[ids != ".derivative"]
  # Check both public functions against the independent expected values.
  i <- do.call(order, unname(samples[ids]))
  reference <- expected[rep(seq_len(nrow(expected)), each = 2), ]
  j <- do.call(order, unname(reference[ids]))
  expect_equal(as.data.frame(samples[i, ids]),
    as.data.frame(reference[j, ids]), ignore_attr = TRUE)
  expect_equal(samples$.derivative[i], reference$.derivative[j],
    tolerance = 1e-6)
  expect_s3_class(out, "response_derivatives")
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), nrow(expected))
  expect_equal(as.data.frame(out[ids]), as.data.frame(expected[ids]),
    ignore_attr = TRUE)
  expect_equal(out$.derivative, expected$.derivative, tolerance = 1e-6)
  expect_equal(out$.se, rep(0, nrow(expected)), tolerance = 0)
  expect_equal(unname(out$.lower_ci), out$.derivative, tolerance = 0)
  expect_equal(unname(out$.upper_ci), out$.derivative, tolerance = 0)
  expect_false(any(c(".draw", ".chain", ".iteration") %in% names(out)))
  expect_null(attr(out, "sampling"))
  expect_false(inherits(out, "grouped_df"))
  expect_identical(out$.focal, rep(focal, nrow(expected)))
  expect_equal(out[[focal]], data[[focal]][expected$.row])
  invisible(out)
}

test_that("scalar summaries retain input rows and pool draws across chains", {
  m <- response_delta_fixture(gaussian())
  nd <- data.frame(x = c(.5, -.4, .5))
  expected <- data.frame(.row = 1:3, .derivative = unname(coef(m)[2]))
  expect_fixed_response_derivatives(m, nd, "x", expected)

  # Known slopes give independent checks of median, SD and interval summaries.
  b <- rbind(coef(m), coef(m), coef(m))
  b[, 2] <- c(-2, 1, 4)
  out <- response_derivatives(m, data = nd, focal = "x", method = "user",
    draws = b, type = "central", eps = 1e-5, level = .5)
  expect_identical(out$.row, 1:3)
  expect_equal(out$.derivative, rep(1, 3), tolerance = 1e-8)
  expect_equal(out$.se, rep(3, 3), tolerance = 1e-8)
  expect_equal(unname(out$.lower_ci), rep(-.5, 3), tolerance = 1e-8)
  expect_equal(unname(out$.upper_ci), rep(2.5, 3), tolerance = 1e-8)
})

test_that("ordinal and multinomial summaries keep each response category", {
  for (case in list(list(m_ocat, su_eg1_ocat, "x0"),
                   list(m_multinom, multinom_df, "x1"))) {
    m <- case[[1]]
    for (rows in list(40L, c(40L, 15L, 40L))) {
      nd <- case[[2]][rows, , drop = FALSE]
      slopes <- native_response_slope(m, nd, case[[3]])
      expected <- data.frame(
        .row = rep(seq_len(nrow(nd)), each = ncol(slopes)),
        .category = rep(paste0("category", seq_len(ncol(slopes))), nrow(nd)),
        .derivative = as.vector(t(slopes)))
      out <- expect_fixed_response_derivatives(m, nd, case[[3]], expected)
      expect_equal(as.numeric(tapply(out$.derivative, out$.row, sum)),
        rep(0, nrow(nd)), tolerance = 1e-7)
    }
  }
})

test_that("multivariate summaries keep each response on both scales", {
  nd <- mvn_df[c(40, 15, 40), ]
  slopes <- native_response_slope(m_mvn, nd, "x0")
  expected <- data.frame(.row = rep(1:3, each = 2),
    .y = rep(c("response1", "response2"), 3),
    .derivative = as.vector(t(slopes)))
  for (scale in c("response", "linear_predictor")) {
    expect_fixed_response_derivatives(m_mvn, nd, "x0", expected, scale)
  }
})

test_that("grouped summaries retain family and category identities in row order", {
  nd <- gfam_data[c(3, 1, 2, 3), ]
  expected <- data.frame(.row = 1:4, .family = nd$family_id,
    .derivative = native_response_slope(gfam_model, nd, "x"))
  expect_fixed_response_derivatives(gfam_model, nd, "x", expected)

  nd <- gfam_ordinal_data[c(2, 7, 1, 2), ]
  eta <- unname(predict(gfam_ordinal_model, nd, type = "link"))
  slope <- native_response_slope(gfam_ordinal_model, nd, "x", type = "link")
  cuts <- c(-Inf, family(gfam_ordinal_model)$getfl()[[1]]$getTheta(TRUE), Inf)
  # Differentiate the cumulative logistic probabilities analytically.
  expected <- do.call(rbind, lapply(seq_len(nrow(nd)), function(i) {
    ordinal <- nd$family_id[i] == 1L
    value <- if (ordinal) {
      -diff(dlogis(cuts - eta[i])) * slope[i]
    } else exp(eta[i]) * slope[i]
    data.frame(.row = i,
      .category = if (ordinal) paste0("category", seq_along(value)) else NA_character_,
      .family = nd$family_id[i], .derivative = value)
  }))
  expect_fixed_response_derivatives(gfam_ordinal_model, nd, "x", expected)
})
