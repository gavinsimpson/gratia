test_that("overview reports coefficient-level parametric degrees of freedom", {
  withr::local_seed(42)
  for (nlevels in 2:4) {
    d <- data.frame(
      f = factor(rep(seq_len(nlevels), each = 50)),
      x = runif(50 * nlevels), z = rnorm(50 * nlevels)
    )
    d$y <- as.integer(d$f) + sin(6 * d$x) + d$z + rnorm(nrow(d))
    for (intercept in c(TRUE, FALSE)) {
      form <- if (intercept) {
        y ~ f + z + s(x, k = 6)
      } else {
        y ~ 0 + f + z + s(x, k = 6)
      }
      m <- gam(form, data = d, method = "REML")
      got <- overview(m)
      para <- got[got$type == "parametric", ]
      ref <- summary(m)$p.table
      expect_equal(
        para$term,
        sub("^\\(Intercept\\)$", "Intercept", rownames(ref))
      )
      expect_equal(para$statistic, unname(ref[, 3]))
      expect_equal(
        para$p.value,
        format.pval(unname(ref[, 4]), eps = 0.001, digits = 3)
      )
      expect_equal(para$edf, rep(1, nrow(ref)))
      expect_equal(para$ref.edf, rep(1, nrow(ref)))
      smooth_only <- overview(m, parametric = FALSE)
      expect_equal(got[got$type != "parametric", ], smooth_only)
    }
  }
})

test_that("overview retains every coefficient in models without smooths", {
  models <- list(m_only_para, variance_intercept_model,
    intercept_test_model("gam_linear"))
  for (model in models) {
    ref <- summary(model)$p.table
    for (stars in c(FALSE, TRUE)) {
      got <- overview(model, stars = stars)
      expect_identical(class(got), c("overview", "tbl_df", "tbl", "data.frame"))
      expect_identical(nrow(got), nrow(ref))
      expect_identical(
        got$term,
        sub("^\\(Intercept\\)$", "Intercept", rownames(ref))
      )
      expect_identical(got$type, rep("parametric", nrow(ref)))
      expect_identical(got$k, rep(NA_real_, nrow(ref)))
      expect_equal(got$edf, rep(1, nrow(ref)))
      expect_equal(got$ref.edf, rep(1, nrow(ref)))
      expect_equal(got$statistic, unname(ref[, 3]))
      expect_identical(
        got$p.value,
        format.pval(unname(ref[, 4]), eps = 0.001, digits = 3)
      )
      if (stars) {
        ref_stars <- symnum(unname(ref[, 4]), corr = FALSE, na = FALSE,
          cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
          symbols = c("***", "**", "*", ".", " "))
        expect_identical(got$stars, ref_stars)
        expect_identical(attr(got, "legend"), attr(ref_stars, "legend"))
      } else {
        expect_null(attr(got, "legend"))
      }
      expect_output(print(got), "Generalized Additive Model")
    }
  }
})

test_that("overview without parametric terms can be empty", {
  models <- list(m_only_para, variance_intercept_model,
    intercept_test_model("gam_linear"))
  for (model in models) {
    for (stars in c(FALSE, TRUE)) {
      got <- overview(model, parametric = FALSE, stars = stars)
      expected_types <- c(term = "character", type = "character", k = "double",
        edf = "double", ref.edf = "double", statistic = "double",
        p.value = "character")
      if (stars) expected_types <- c(expected_types, stars = "character")
      expect_identical(nrow(got), 0L)
      expect_named(got, names(expected_types))
      expect_identical(vapply(got, typeof, character(1)), expected_types)
      expect_identical(class(got), c("overview", "tbl_df", "tbl", "data.frame"))
      expect_null(attr(got, "legend"))
      if (stars) expect_s3_class(got$stars, "noquote")
      expect_output(print(got), "Generalized Additive Model with 0 terms")
    }
  }
})
