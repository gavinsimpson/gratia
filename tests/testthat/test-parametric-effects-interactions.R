test_that("multiple linear predictors exclude parametric interactions (#369)", {
  df <- data_sim("eg1", seed = 42)
  formulas <- list(
    list(y ~ x0 * x1, ~ 1),
    list(y ~ x0 * x1, ~ x0 * x1),
    list(y ~ 1, ~ x0 * x1)
  )
  expected <- list(
    c("x0", "x1"), c("x0", "x1", "x0.1", "x1.1"),
    c("x0.1", "x1.1")
  )

  for (i in seq_along(formulas)) {
    m <- gam(formulas[[i]], family = gaulss(), data = df, method = "REML")
    expect_message(
      pe <- parametric_effects(m),
      "Interaction terms are not currently supported.", fixed = TRUE
    )
    expect_identical(unique(pe$.term), expected[[i]])

    # Retained effects must match the same prediction call used before filtering.
    pred <- predict(m, newdata = df, type = "terms", se.fit = TRUE,
      terms = names(parametric_terms(m)))
    for (term in expected[[i]]) {
      effect <- pe[pe$.term == term, ]
      expect_equal(unname(effect$.partial), unname(pred$fit[, term]))
      expect_equal(unname(effect$.se), unname(pred$se.fit[, term]))
    }
    expect_s3_class(draw(pe), "patchwork")
  }
})

test_that("interaction-only multiple-predictor models return NULL (#369)", {
  df <- data_sim("eg1", seed = 42)
  m <- gam(list(y ~ x0:x1, ~ 1), family = gaulss(), data = df,
    method = "REML")
  expect_message(
    expect_message(pe <- parametric_effects(m),
      "Interaction terms are not currently supported.", fixed = TRUE),
    "The model doesn't contain any non-interaction parametric terms",
    fixed = TRUE
  )
  expect_null(pe)
})

test_that("single-predictor extended families exclude interactions (#369)", {
  df <- data_sim("eg1", seed = 42)
  set.seed(42)
  df$count <- rpois(nrow(df), exp(1 + df$x0 * df$x1))
  m <- gam(count ~ x0 * x1, family = nb(), data = df, method = "REML")
  expect_message(pe <- parametric_effects(m),
    "Interaction terms are not currently supported.", fixed = TRUE)
  expect_identical(unique(pe$.term), c("x0", "x1"))
  expect_message(
    expect_message(pe <- parametric_effects(m, terms = "x0:x1"),
      "Interaction terms are not currently supported.", fixed = TRUE),
    "The model doesn't contain any non-interaction parametric terms",
    fixed = TRUE)
  expect_null(pe)
})
