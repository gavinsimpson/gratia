test_that("model constants use actual intercepts and selected predictor names", {
  for (type in c("gaulss", "gaulss_no_location", "gaulss_no_scale")) {
    m <- intercept_test_model(type)
    # Explicit coefficient names give an independent reference for each formula.
    expected <- c(if (type == "gaulss_no_location") 0 else coef(m)["(Intercept)"],
      if (type == "gaulss_no_scale") 0 else coef(m)["(Intercept).1"])
    for (lp in list(1:2, 2L, 2:1)) {
      ref <- unname(expected[lp])
      attr(ref, "par_names") <- c("location", "scale")[lp]
      expect_equal(model_constant(m, lp = lp), ref)
    }
  }
  for (m in list(m_gam, intercept_test_model("gam"),
      intercept_test_model("gam_linear"))) {
    ref <- if ("(Intercept)" %in% names(coef(m))) unname(coef(m)["(Intercept)"]) else 0
    attr(ref, "par_names") <- "location"
    expect_equal(model_constant(m), ref)
  }
  expect_equal(model_constant(m_glm), coef(m_glm)["(Intercept)"])
  expect_equal(model_constant(intercept_test_model("glm")), 0)
})

test_that("confidence intervals shift each smooth by its predictor's intercept", {
  for (type in c("gaulss", "gaulss_no_location", "gaulss_no_scale", "gam")) {
    m <- intercept_test_model(type)
    constants <- if (type == "gam") 0 else c(
      if (type == "gaulss_no_location") 0 else unname(coef(m)["(Intercept)"]),
      if (type == "gaulss_no_scale") 0 else unname(coef(m)["(Intercept).1"]))
    labels <- smooths(m)
    # Check both smooths together and the later predictor selected on its own.
    for (selection in list(seq_along(labels), length(labels))) {
      for (overall in c(FALSE, TRUE)) {
        ci <- confint(m, parm = labels[selection], n = 5,
          overall_uncertainty = overall)
        shifted <- confint(m, parm = labels[selection], n = 5, shift = TRUE,
          overall_uncertainty = overall)
        shift <- constants[match(ci$.smooth, labels)]
        for (column in c(".estimate", ".lower_ci", ".upper_ci")) {
          expect_equal(shifted[[column]], ci[[column]] + shift)
        }
        expect_equal(shifted$.se, ci$.se)
        expect_equal(shifted$.crit, ci$.crit)
        expect_equal(attr(shifted, "interval_info"), attr(ci, "interval_info"))
      }
    }
  }
})

test_that("added fitted terms retain native prediction constants", {
  for (m in list(m_gam, intercept_test_model("gam"),
      intercept_test_model("gam_linear"), intercept_test_model("gaulss"),
      intercept_test_model("gaulss_no_location"))) {
    d <- model.frame(m)[1:5, , drop = FALSE]
    types <- if (n_eta(m) > 1L) "terms" else c("terms", "iterms")
    for (type in types) {
      for (se in c(FALSE, TRUE)) {
        native <- predict(m, newdata = d, type = type, se.fit = se)
        if (se) native <- native$fit
        constant <- attr(native, "constant")
        if (is.null(constant)) constant <- 0
        added <- suppressMessages(add_fitted(d, m, type = type, se.fit = se))
        expect_equal(added$.constant, rep(unname(constant), nrow(d)))
        expect_equal(as.matrix(added[, ncol(d) + 1L + seq_len(ncol(native)), drop = FALSE]),
          native, ignore_attr = TRUE)
      }
    }
  }
})

test_that("shifting simultaneous intervals preserves their uncertainty", {
  m <- intercept_test_model()
  for (overall in c(FALSE, TRUE)) {
    ci <- confint(m, parm = smooths(m), n = 5, type = "simultaneous",
      nsim = 100, seed = 433, overall_uncertainty = overall)
    shifted <- confint(m, parm = smooths(m), n = 5, type = "simultaneous",
      nsim = 100, seed = 433, overall_uncertainty = overall, shift = TRUE)
    shift <- unname(coef(m)[c("(Intercept)", "(Intercept).1")])[
      match(ci$.smooth, smooths(m))]
    for (column in c(".estimate", ".lower_ci", ".upper_ci")) {
      expect_equal(shifted[[column]], ci[[column]] + shift)
    }
    expect_equal(shifted$.se, ci$.se)
    expect_equal(shifted$.crit, ci$.crit)
  }
})

test_that("shared GAM utilities retain BAM and gamm4 intercepts", {
  for (m in list(m_bam, m_gamm4)) {
    component <- if (inherits(m, "gamm4")) m$gam else m
    intercept <- unname(coef(component)["(Intercept)"])
    expect_equal(as.numeric(model_constant(m)), intercept)
    ci <- confint(m, parm = "s(x1)", n = 5)
    shifted <- confint(m, parm = "s(x1)", n = 5, shift = TRUE)
    for (column in c(".estimate", ".lower_ci", ".upper_ci")) {
      expect_equal(shifted[[column]], ci[[column]] + intercept)
    }
    for (type in c("terms", "iterms")) {
      native <- predict(component, newdata = tiny_eg1[1:5, ], type = type)
      added <- add_fitted(tiny_eg1[1:5, ], m, type = type)
      expect_equal(added$.constant,
        rep(unname(attr(native, "constant")), 5))
    }
  }
})
