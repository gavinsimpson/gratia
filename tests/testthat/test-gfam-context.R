test_that("grouped-family membership follows requested observation rows", {
  context <- gfam_prediction_context(gfam_model)
  expect_identical(context$index, gfam_data$family_id)
  expect_length(context$families, 3L)
  expect_length(context$components, 3L)
  expect_equal(context$components[[1L]]$scale, 1)
  expect_equal(context$components[[3L]]$scale, 1)
  expect_gt(context$components[[2L]]$scale, 0)
  expect_identical(context$components[[2L]]$theta_index, integer())

  rows <- c(13, 2, 14, 2, 6)
  data <- gfam_data[rows, ]
  data$y <- NULL
  expect_identical(gfam_prediction_context(gfam_model, data)$index,
    gfam_data$family_id[rows])
  data$family_id <- NULL
  expect_error(gfam_prediction_context(gfam_model, data), "needs family membership")
  expect_identical(gfam_prediction_context(gfam_model, data,
    family_index = gfam_data$family_id[rows])$index, gfam_data$family_id[rows])
  expect_error(gfam_prediction_context(gfam_model, gfam_data,
    family_index = rep(1, nrow(gfam_data))), "conflicts")
  for (index in list(1, rep(0, 5), rep(4, 5), rep(1.5, 5), rep(Inf, 5),
      rep("1", 5), factor(rep(1, 5)))) {
    expect_error(gfam_prediction_context(gfam_model, data,
      family_index = index), "one integer family-list position")
  }
})

test_that("response matrix and response-name indices follow mgcv conventions", {
  rows <- c(12, 1, 5)
  data <- gfam_matrix_data[rows, ]
  expect_identical(gfam_prediction_context(gfam_matrix_model, data)$index,
    gfam_data$family_id[rows])
  data$response <- gfam_data$family_id[rows]
  expect_identical(gfam_prediction_context(gfam_matrix_model, data)$index,
    gfam_data$family_id[rows])
  data$response <- matrix(1, nrow = 3, ncol = 3)
  expect_error(gfam_prediction_context(gfam_matrix_model, data), "two columns")
})

test_that("grouped-family membership shares missing-row prediction layout", {
  context <- gfam_prediction_context(gfam_missing_model)
  expect_length(context$family_index, 165)
  expect_true(all(is.na(context$family_index[c(7, 16)])))
  expect_identical(context$index,
    gfam_data$family_id[setdiff(seq_len(165), c(7, 16))])
  expect_identical(context$layout$rows, seq_len(165))

  data <- gfam_data[1:8, ]
  data$family_id[c(2, 5)] <- NA_integer_
  data$x[3] <- NA_real_
  context <- gfam_prediction_context(gfam_model, data)
  expect_identical(context$layout$input_rows, c(1L, 4L, 6L, 7L, 8L))
  expect_identical(context$index, gfam_data$family_id[c(1, 4, 6, 7, 8)])
  expect_identical(context$layout$map, c(1L, NA_integer_, NA_integer_, 2L,
    NA_integer_, 3L, 4L, 5L))
  context <- gfam_prediction_context(gfam_model, data, na.action = na.omit)
  expect_identical(context$layout$rows, c(1L, 4L, 6L, 7L, 8L))
  data$family_id[] <- NA_integer_
  context <- gfam_prediction_context(gfam_model, data)
  expect_true(all(is.na(context$index)))
  expect_true(all(is.na(context$layout$map)))
})

test_that("grouped-family scale metadata respects packed parameter positions", {
  family <- family(gfam_gaussian_model)
  before <- family$getfl()
  context <- gfam_prediction_context(gfam_gaussian_model)
  expect_equal(vapply(context$components, `[[`, numeric(1), "scale"),
    exp(family$getTheta()))
  expect_gt(context$components[[2L]]$scale, context$components[[1L]]$scale * 4)
  expect_identical(family$getfl(), before)
})


test_that("extended component parameters and scales use distinct packed positions", {
  family <- family(gfam_extended_model)
  before <- family$getfl()
  before_membership <- attr(before, "fi")
  before_theta <- lapply(before, function(f) if (is.function(f$getTheta)) f$getTheta())
  theta <- family$getTheta()
  context <- gfam_prediction_context(gfam_extended_model)
  expect_identical(context$components[[1L]]$theta_index, 1L)
  expect_equal(context$components[[1L]]$theta, theta[[1L]])
  expect_equal(context$components[[1L]]$scale, 1)
  expect_equal(context$components[[2L]]$scale, exp(theta[[2L]]))
  expect_identical(context$components[[3L]]$theta_index, 3L)
  expect_equal(context$components[[3L]]$theta, theta[[3L]])
  expect_identical(context$components[[4L]]$theta_index, 4L)
  expect_equal(context$components[[4L]]$scale, exp(theta[[5L]]))
  expect_identical(family$getfl(), before)
  expect_identical(family$getTheta(), theta)
  expect_identical(attr(family$getfl(), "fi"), before_membership)
  expect_identical(lapply(family$getfl(), function(f) {
    if (is.function(f$getTheta)) f$getTheta()
  }), before_theta)
  expect_equal(context$components[[3L]]$family$getTheta(), theta[[3L]])
})

test_that("fixed parameters in native closures do not consume packed positions", {
  fixed <- mgcv::nb(theta = 3)
  components <- list(fixed, gaussian())
  components[[2L]]$scale <- -1
  family <- list(getfl = function() components, getTheta = function() log(0.4))
  result <- gfam_components(family)
  expect_identical(result[[1L]]$theta_index, integer())
  expect_equal(result[[1L]]$theta, log(3))
  expect_equal(result[[2L]]$scale, 0.4)
})
