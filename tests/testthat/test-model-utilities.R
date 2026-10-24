
test_that("model_constant returns the intercept estimate", {
  expect_silent(b <- model_constant(m_gam))
  expect_type(b, "double")
  ref <- unname(coef(m_gam)[1L])
  attr(ref, "par_names") <- "location"
  expect_identical(b, ref)
  expect_named(b, expected = NULL)
})

test_that("model_constant works for a GAMLSS", {
  expect_length(model_constant(m_gaulss), 2L)
  expect_length(model_constant(m_gam), 1L)
})

test_that("model vars works for various GAMs", {
  expect_silent(mvars <- model_vars(m_gam))
  expect_identical(mvars, paste0("x", 0:3))
  expect_silent(mvars <- model_vars(m_bam))
  expect_identical(mvars, paste0("x", 0:3))
  expect_silent(mvars <- model_vars(m_gamm))
  expect_identical(mvars, paste0("x", 0:3))
  expect_silent(mvars <- model_vars(m_gamm4))
  expect_identical(mvars, paste0("x", 0:3))
})

test_that("model_terms works", {
  expect_silent(tm <- model_terms(m_gam))
  expect_type(tm, "character")
  expect_identical(tm, c("(Intercept)", "s(x0)", "s(x1)", "s(x2)", "s(x3)"))
  expect_silent(tm <- model_terms(m_gamm))
  expect_type(tm, "character")
  expect_identical(tm, c("(Intercept)", "s(x0)", "s(x1)", "s(x2)", "s(x3)"))
  expect_silent(tm <- model_terms(su_m_factor_by))
  expect_type(tm, "character")
  expect_identical(
    tm,
    c(
      "(Intercept)", "fac2", "fac3", "s(x2):fac1", "s(x2):fac2", "s(x2):fac3",
      "s(x0)"
    )
  )
  expect_silent(tm <- model_terms(m_only_para))
  expect_type(tm, "character")
  expect_identical(
    tm,
    c(
      "(Intercept)", "fac2", "fac3", "ffB", "ffC", "ffD", "x0", "x1", "x2",
      "fac2:ffB", "fac3:ffB", "fac2:ffC", "fac3:ffC", "fac2:ffD", "fac3:ffD"
    )
  )
})

test_that("model_terms handles one coefficient and preserves coefficient order", {
  expect_identical(model_terms(variance_intercept_model), "(Intercept)")
  expect_identical(model_terms(intercept_test_model("gam_linear")), "x0")
  expect_identical(model_terms(m_only_para), names(coef(m_only_para)))
  expect_identical(model_terms(m_para_sm),
    c(names(summary(m_para_sm)$p.coeff), smooths(m_para_sm)))
  expect_identical(model_terms(intercept_test_model("gam")), c("x0", "s(x1)"))
  expect_identical(model_terms(m_bam), model_terms(m_gam))
  expect_identical(model_terms(m_gamm4), model_terms(m_gam))
})

test_that("match_smoother returns one logical value for every supplied name", {
  prefixes <- c("s(", "ti(", "te(", "t2(")
  smooth_names <- paste0(c(prefixes, paste0("mgcv::", prefixes)), "x)")
  other_names <- c("(Intercept)", "wt", "slope", "as(x)", "x:s(x)")
  for (name in smooth_names) {
    expect_identical(match_smoother(name), TRUE)
  }
  for (name in other_names) {
    expect_identical(match_smoother(name), FALSE)
  }
  expect_identical(match_smoother(character()), logical())
  expect_identical(match_smoother(c(other_names, smooth_names)),
    c(rep(FALSE, length(other_names)), rep(TRUE, length(smooth_names))))
  expect_identical(match_smoother(c(smooth = "s(x)", slope = "wt")),
    c(TRUE, FALSE))
})
