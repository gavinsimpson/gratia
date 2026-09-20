test_that("smooth differences align covariates across factor levels", {
  withr::local_seed(4004)
  d <- data.frame(x = runif(240), z = runif(240), w = runif(240),
    group = factor(rep(letters[1:3], 80)))
  d$y <- as.integer(d$group) * sin(3 * d$x) + d$x * d$z + d$w +
    rnorm(nrow(d), sd = 0.1)
  models <- list(
    univariate = mgcv::gam(y ~ group + s(x, by = group, k = 6) + s(w, k = 5),
      data = d, method = "REML"),
    multivariate = mgcv::gam(y ~ group + te(x, z, by = group, k = c(4, 4)) +
        s(w, k = 5), data = d, method = "REML")
  )
  for (kind in names(models)) {
    model <- models[[kind]]
    vars <- if (kind == "univariate") "x" else c("x", "z")
    select <- if (kind == "univariate") "s(x)" else "te(x,z)"
    grid <- if (kind == "univariate") {
      data.frame(x = c(0.2, 0.4, 0.6, 0.8))
    } else {
      expand.grid(x = c(0.2, 0.8), z = c(0.3, 0.7))
    }
    nd <- do.call(rbind, lapply(levels(d$group), function(g) {
      transform(grid, group = factor(g, levels = levels(d$group)),
        w = match(g, levels(d$group)) / 4)
    }))
    rownames(nd) <- NULL
    # Each group has a different order; the first group's order is retained.
    shuffled <- nd[c(3, 1, 4, 2, 8, 7, 6, 5, 10, 12, 9, 11), ]
    for (group_means in c(FALSE, TRUE)) {
      for (prediction_data in list(nd, shuffled, nd[c(1, 5, 9), ])) {
        result <- difference_smooths(model, select = select,
          data = prediction_data, group_means = group_means, ci_level = 0.8)
        for (pair in combn(levels(d$group), 2, simplify = FALSE)) {
          actual <- result[result$.level_1 == pair[1] &
            result$.level_2 == pair[2], ]
          first <- prediction_data[prediction_data$group == pair[1], , drop = FALSE]
          # Construct the second prediction grid from the first rather than
          # matching rows, independently specifying the intended comparison.
          second <- first
          second$group <- factor(pair[2], levels = levels(d$group))
          second$w <- match(pair[2], levels(d$group)) / 4
          X <- predict(model, first, type = "lpmatrix") -
            predict(model, second, type = "lpmatrix")
          keep <- rep(FALSE, ncol(X))
          for (sm in model$smooth) {
            if (sm$by == "group" && sm$by.level %in% pair) {
              keep[sm$first.para:sm$last.para] <- TRUE
            }
          }
          if (group_means) {
            keep[seq_len(model$nsdf)] <- TRUE
          }
          X[, !keep] <- 0
          estimate <- unname(drop(X %*% coef(model)))
          se <- unname(sqrt(rowSums((X %*% vcov(model)) * X)))
          expected_grid <- first[, vars, drop = FALSE]
          rownames(expected_grid) <- NULL
          expect_equal(as.data.frame(actual[, vars, drop = FALSE]), expected_grid)
          expect_equal(unname(actual$.diff), estimate)
          expect_equal(unname(actual$.se), se)
          expect_equal(unname(actual$.lower_ci), estimate - qnorm(0.9) * se)
          expect_equal(unname(actual$.upper_ci), estimate + qnorm(0.9) * se)
        }
      }
    }

    # Missing rows, different coordinates, duplicates on either side, and
    # an absent level must fail explicitly rather than recycling/mispairing.
    changed <- nd
    changed$x[5] <- changed$x[5] + 1e-8
    invalid <- list(nd[-5, ], changed, rbind(nd, nd[1, ]),
      rbind(nd, nd[5, ]), nd[nd$group != "b", ])
    # For two-dimensional smooths, both coordinates are part of the key.
    if (kind == "multivariate") {
      changed_z <- nd
      changed_z$z[5] <- 0.9
      invalid <- c(invalid, list(changed_z))
    }
    for (bad_data in invalid) {
      expect_error(difference_smooths(model, select = select, data = bad_data),
        "same prediction grid|unique covariate combinations")
    }
  }
})
