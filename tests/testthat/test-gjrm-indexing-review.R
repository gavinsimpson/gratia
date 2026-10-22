skip_if_not_installed("GJRM", minimum_version = "0.2-6")

gjrm_indexing_review_fixture <- local({
  models <- list()
  function(kind = c("ordinal_aux", "ordinal_pair_aux", "switching", "scalar")) {
    kind <- match.arg(kind)
    if (is.null(models[[kind]])) {
      s <- mgcv::s
      d <- withr::with_seed(3771, {
        n <- 160L
        d <- data.frame(x = runif(n), z = runif(n))
        d$y1 <- sin(4 * d$x) + rnorm(n)
        d$y2 <- .2 + cos(4 * d$z) + rnorm(n)
        d$y3 <- sin(4 * d$z) + rnorm(n)
        d
      })
      if (kind == "switching") {
        d$y1 <- as.numeric(d$y1 > 0)
        # Exercise the constructor because GJRM 0.2.6.9 cannot dispatch SWITCH
        # to it through gjrm(); this still produces its ordinary gjrm class.
        fit <- getFromNamespace("SemiParROY", "GJRM")
        args <- list(formula = list(y1 ~ s(x, k = 4), y2 ~ s(z, k = 4),
          y3 ~ s(x, k = 4), ~ 1, ~ 1, ~ s(x, k = 4), ~ s(z, k = 4)),
          data = d, weights = rep(1, nrow(d)), parscale = 1,
          margins = c("probit", "N", "N"))
      } else {
        fit <- GJRM::gjrm
        if (kind == "scalar") {
          d$y1 <- pnorm(d$y1)
          d$y2 <- pnorm(d$y2)
          args <- list(formula = list(y1 ~ 1, y2 ~ 1, ~ 1), data = d,
            fixed.margins = TRUE)
        } else {
          d$y1 <- as.integer(cut(d$y1, c(-Inf, -.5, .4, Inf)))
          if (kind == "ordinal_pair_aux") {
            d$y2 <- as.integer(cut(d$y2, c(-Inf, -.5, .2, .9, Inf)))
            args <- list(formula = list(y1 ~ s(x, k = 4), y2 ~ s(z, k = 4),
              ~ s(x, k = 4)), data = d, margins = c("ord.probit", "ord.logit"),
              model = "B")
          } else {
            args <- list(formula = list(y1 ~ s(x, k = 4), y2 ~ s(z, k = 4),
              ~ s(x, k = 4), ~ s(z, k = 4)), data = d,
              margins = c("ord.probit", "N"), model = "B")
          }
        }
      }
      models[[kind]] <<- withr::with_seed(3772, do.call(fit, args))
    }
    models[[kind]]
  }
})

test_that("ordinal auxiliary equations follow every cutpoint and slope block", {
  d <- data.frame(x = c(.2, .7, .4), z = c(.8, .1, .6))
  for (kind in c("ordinal_aux", "ordinal_pair_aux")) {
    model <- gjrm_indexing_review_fixture(kind)
    cutpoints <- model$VC$K1 - 1L
    if (kind == "ordinal_pair_aux") cutpoints <- cutpoints + model$VC$K2 - 1L
    offset <- cutpoints
    for (equation in seq_along(model$formula)) {
      g <- model[[paste0("gam", equation)]]
      width <- model[[paste0("X", equation, ".d2")]]
      ordinal <- equation == 1L || (equation == 2L && kind == "ordinal_pair_aux")
      sm <- g$smooth[[1L]]
      # Native ordinal prediction removes its synthetic intercept column.
      X <- predict(model, eq = equation, newdata = d, type = "lpmatrix")
      local <- sm$first.para:sm$last.para - as.integer(ordinal)
      columns <- offset + local
      for (frequentist in c(FALSE, TRUE)) {
        e <- smooth_estimates(model, select = equation, data = d,
          overall_uncertainty = FALSE, frequentist = frequentist)
        V <- if (frequentist) model$Ve else model$Vb
        expect_equal(e$.estimate,
          unname(drop(X[, local, drop = FALSE] %*% model$coefficients[columns])))
        expect_equal(e$.se, unname(sqrt(rowSums(
          (X[, local, drop = FALSE] %*% V[columns, columns, drop = FALSE]) *
            X[, local, drop = FALSE]))))
      }
      offset <- offset + width
    }
  }
})

test_that("switching smooths keep offsets across scalar scale equations", {
  model <- gjrm_indexing_review_fixture("switching")
  d <- data.frame(x = c(.2, .7), z = c(.8, .1))
  offset <- 0L
  selected <- 0L
  for (equation in 1:7) {
    g <- model[[paste0("gam", equation)]]
    width <- model[[paste0("X", equation, ".d2")]]
    for (sm in g$smooth) {
      selected <- selected + 1L
      X <- mgcv::PredictMat(sm, d)
      columns <- offset + sm$first.para:sm$last.para
      for (frequentist in c(FALSE, TRUE)) {
        e <- smooth_estimates(model, select = selected, data = d,
          overall_uncertainty = FALSE, frequentist = frequentist)
        V <- if (frequentist) model$Ve else model$Vb
        expect_equal(e$.estimate, unname(drop(X %*% model$coefficients[columns])))
        expect_equal(e$.se,
          unname(sqrt(rowSums((X %*% V[columns, columns, drop = FALSE]) * X))))
      }
    }
    offset <- offset + width
  }
})

test_that("scalar fixed-margin components retain matrix dimensions and model context", {
  model <- gjrm_indexing_review_fixture("scalar")
  component <- gjrm_gamlss_components(model)[[1L]]
  expect_identical(dim(component$Vp), c(1L, 1L))
  expect_identical(dim(component$Ve), c(1L, 1L))
  expect_equal(unname(component$coefficients), unname(model$coefficients))
  expect_equal(unname(component$Vp), unname(model$Vb))
  expect_equal(unname(component$Ve), unname(model$Ve))
  expect_identical(model_context(model)$component, "gam5")
  expect_identical(model_context(model, "gam5")$component, "gam5")
  for (component in list("gam1", c("gam1", "gam5"), NA_character_, 5L)) {
    expect_error(model_context(model, component), "component")
  }
})

test_that("missing intermediate GAMs do not shift later coefficient blocks", {
  model <- gjrm_joint_fixture()
  # The final coefficient block remains present when a working GAM is absent.
  model$gam3 <- NULL
  components <- gjrm_gamlss_components(model)
  expect_identical(vapply(components, attr, integer(1), "gratia.gjrm_equation"),
    c(1L, 2L, 4L, 5L))
  offset <- model$X1.d2 + model$X2.d2 + model$X3.d2
  columns <- offset + seq_len(model$X4.d2)
  expect_equal(unname(components[[3]]$coefficients),
    unname(model$coefficients[columns]))
  expect_equal(unname(components[[3]]$Vp), unname(model$Vb[columns, columns]))
})

test_that("joint adapters reject unavailable final uncertainty instead of working values", {
  model <- gjrm_joint_fixture()
  for (field in c("Vb", "Ve", "F")) {
    altered <- model
    altered[[field]] <- NULL
    expect_error(smooth_estimates(altered, n = 4), "GJRM final")
  }
  for (invalid in list(length(model$coefficients),
    matrix(1, length(model$coefficients), length(model$coefficients) + 1L))) {
    altered <- model
    altered$F <- invalid
    expect_error(smooth_estimates(altered, n = 4), "effective degrees of freedom")
  }
})

test_that("fixed-margin smooth plots retain their dependence equation label", {
  model <- gjrm_joint_fixture("fixed")
  estimates <- smooth_estimates(model, n = 8)
  expect_identical(unique(estimates$.model), "Dependence (theta)")
  expect_identical(assemble(model, n = 8)[[1]]$labels$subtitle, "Dependence (theta)")
  expect_identical(draw(model, n = 8, wrap = FALSE)[[1]]$labels$subtitle,
    "Dependence (theta)")
  expect_identical(draw(estimates)[[1]]$labels$subtitle, "Dependence (theta)")
  nested <- smooth_estimates(model, n = 8, unnest = FALSE)
  expect_identical(unique(nested$.model), "Dependence (theta)")
  expect_identical(draw(nested)[[1]]$labels$subtitle, "Dependence (theta)")
  expect_identical(draw(model, n = 8, subtitle = "Custom", wrap = FALSE)[[1]]$labels$subtitle,
    "Custom")
})
