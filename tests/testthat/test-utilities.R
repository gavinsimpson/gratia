## Test Utilities

test_that("smooth_terms() methods work", {
  st <- smooth_terms(m_gam)
  expect_type(st, "list")
  expect_length(st, 4L)
  expect_identical(st, as.list(paste0("x", 0:3)))

  st <- smooth_terms(m_gamm)
  expect_type(st, "list")
  expect_length(st, 4L)
  expect_identical(st, as.list(paste0("x", 0:3)))

  st <- smooth_terms(m_gam[["smooth"]][[1]])
  expect_type(st, "character")
  expect_length(st, 1L)
  expect_identical(st, "x0")
})

test_that("smooth_dim() methods work", {
  d <- smooth_dim(m_gam)
  expect_type(d, "integer")
  expect_length(d, 4L)
  expect_identical(d, rep(1L, 4L))

  d <- smooth_dim(m_gamm)
  expect_type(d, "integer")
  expect_length(d, 4L)
  expect_identical(d, rep(1L, 4L))

  d <- smooth_dim(m_gam[["smooth"]][[1]])
  expect_type(d, "integer")
  expect_length(d, 1L)
  expect_identical(d, rep(1L, 1L))
})

test_that("select_terms() works", {
  st <- select_terms(m_gam)
  expect_type(st, "character")
  expect_length(st, 4L)
  expect_identical(st, paste0("x", 0:3))

  st <- select_terms(m_gam, "x1")
  expect_type(st, "character")
  expect_length(st, 1L)
  expect_identical(st, "x1")

  st <- select_terms(m_gam, c("x1", "x2"))
  expect_type(st, "character")
  expect_length(st, 2L)
  expect_identical(st, c("x1", "x2"))

  expect_message(select_terms(m_gam, "x4"), "x4 not found in `object`")
  expect_message(select_terms(
    m_gam,
    c("x1", "x4")
  ), "x4 not found in `object`")
})

test_that("select_smooth() works", {
  expect_error(select_smooth(m_gam), "'smooth' must be supplied")
  expect_message(
    select_smooth(m_gam, smooth = c("s(x1)", "s(x2)")),
    "Multiple smooths supplied. Using only first"
  )

  sm <- select_smooth(m_gam, smooth = "s(x1)")
  expect_identical(sm, "s(x1)")
})

data(columb) ## data frame
data(columb.polys) ## district shapes list
xt <- list(polys = columb.polys) ## neighbourhood structure info for MRF
## First a full rank MRF...
mrf_mod <- gam(crime ~ s(district, bs = "mrf", xt = xt),
  data = columb,
  method = "REML"
)

test_that("is_mrf_smooth returns true for an MRF smooth", {
  expect_true(is_mrf_smooth(get_smooth(mrf_mod, "s(district)")))
})

test_that("is_mrf_smooth returns false for an none MRF smooth", {
  expect_false(is_mrf_smooth(get_smooth(m_gam, "s(x0)")))
})

test_that("is_mgcv_smooth returns false for objects that aren't smooths", {
  expect_false(is_mgcv_smooth(1:10))
})

test_that("check_is_mgcv_smooth throws error for objects that aren't smooths", {
  expect_error(check_is_mgcv_smooth(1:10),
    "'smooth' is not an 'mgcv.smooth'",
    fixed = TRUE
  )
})

test_that("is.gam returns TRUE for a GAM", {
  expect_true(is.gam(mrf_mod))
  expect_true(is.gam(m_gam))
})

test_that("is.gam returns FALSE for a none GAM", {
  expect_false(is.gam(1:10))
  expect_false(is.gam(data.frame(x = 1:10)))
  expect_false(is.gam(m_gamm))
})

test_that("is.gamm returns TRUE for a GAMM", {
  expect_true(is.gamm(m_gamm))
})

test_that("is.gam returns FALSE for a none GAMM", {
  expect_false(is.gamm(1:10))
  expect_false(is.gamm(data.frame(x = 1:10)))
  expect_false(is.gamm(m_gam))
  expect_false(is.gamm(mrf_mod))
})

test_that("get_vcov with frequentist TRUE works", {
  V <- get_vcov(m_gam, frequentist = TRUE)
  expect_type(V, "double")
  expect_equal(V, m_gam[["Ve"]])
})

test_that("get_vcov with unconditional = TRUE throws warning if not available", {
  expect_warning(
    V <- get_vcov(m_gamgcv, unconditional = TRUE),
    "Covariance corrected for smoothness uncertainty not available."
  )
  expect_type(V, "double")
  expect_equal(V, m_gamgcv[["Vp"]])
})

test_that("get_vcov with unconditional = TRUE returns Vp", {
  V <- get_vcov(m_gam, unconditional = TRUE)
  expect_type(V, "double")
  expect_equal(V, m_gam[["Vc"]])
})

test_that("get_vcov with term specified works", {
  V <- get_vcov(m_gam, term = "s(x1)")
  expect_type(V, "double")
  smooth <- m_gam[["smooth"]][[2L]]
  ind <- smooth$first.para:smooth$last.para
  expect_equal(V, m_gam[["Vp"]][ind, ind, drop = FALSE])

  V <- get_vcov(m_gam, frequentist = TRUE, term = "s(x1)")
  expect_equal(V, m_gam[["Ve"]][ind, ind, drop = FALSE])

  V <- get_vcov(m_gam, unconditional = TRUE, term = "s(x1)")
  expect_equal(V, m_gam[["Vc"]][ind, ind, drop = FALSE])

  expect_message(
    get_vcov(m_gam, term = c("s(x1)", "s(x2)")),
    "Supplied more than 1 'term'; using only the first"
  )
})

test_that("get_smooth works for a GAM", {
  sm <- get_smooth(m_gam, "s(x1)")
  expect_s3_class(sm, "mgcv.smooth")
  expect_true(is_mgcv_smooth(sm))
})

test_that("get_smooth works for a GAMM", {
  sm <- get_smooth(m_gamm, "s(x1)")
  expect_s3_class(sm, "mgcv.smooth")
  expect_true(is_mgcv_smooth(sm))
})

test_that("get_smooths_by_id works for a GAM", {
  sm <- get_smooths_by_id(m_gam, 2L)
  expect_type(sm, "list")
  expect_true(is_mgcv_smooth(sm[[1L]]))
  expect_equal(sm[[1L]], get_smooth(m_gam, "s(x1)"))
})

test_that("get_smooths_by_id works for a GAMM", {
  sm <- get_smooths_by_id(m_gamm, 2L)
  expect_type(sm, "list")
  expect_true(is_mgcv_smooth(sm[[1L]]))
  expect_equal(sm[[1L]], get_smooth(m_gamm, "s(x1)"))
})

test_that("get_smooths_by_id works for gamm4", {
  sm <- get_smooths_by_id(m_gamm4, 2L)
  expect_type(sm, "list")
  expect_true(is_mgcv_smooth(sm[[1L]]))
  expect_equal(sm[[1L]], get_smooth(m_gamm4, "s(x1)"))
})

test_that("get_smooths_by_id works for scam", {
  sm <- get_smooths_by_id(m_scam, 1L)
  expect_type(sm, "list")
  expect_true(is_mgcv_smooth(sm[[1L]]))
  expect_equal(sm[[1L]], get_smooth(m_scam, "s(x1)"))
})

test_that("seq_min_max works as intended", {
  x <- rnorm(10)
  n <- 50L
  s1 <- seq_min_max(x, n = n)
  s2 <- seq(min(x), max(x), length.out = n)
  expect_equal(s1, s2)
  expect_identical(length(s1), length(s2))
  expect_identical(length(s1), n)
})

test_that("factor_var_names works", {
  expect_silent(result <- factor_var_names(su_eg4))
  expect_identical("fac", result)

  expect_null(factor_var_names(su_eg1[, 1:2]))
})

test_that("data_class works for a data frame", {
  expect_silent(result <- data_class(su_eg4))

  expect_named(result, names(su_eg4))

  actual <- c(rep("numeric", 4L), "factor", rep("numeric", 4L))
  names(actual) <- names(su_eg4)
  expect_identical(actual, result)
})

test_that("n_smooths works for gam models", {
  expect_silent(result <- n_smooths(m_gam))
  expect_identical(result, 4L)
})

test_that("n_smooths works for gamm models", {
  expect_silent(result <- n_smooths(m_gamm))
  expect_identical(result, 4L)
})

test_that("n_smooths works for bam models", {
  expect_silent(result <- n_smooths(m_bam))
  expect_identical(result, 4L)
})

test_that("n_smooths, works for objects with a smooth component", {
  expect_silent(result <- n_smooths(list(smooth = 1:10)))
  expect_identical(result, 10L)
})

test_that("n_smooths, fails for objects with no smooth component", {
  expect_error(result <- n_smooths(su_eg1),
    "Don't know how to identify smooths for <tbl_df>",
    fixed = TRUE
  )
})

test_that("which_smooths throws error if no smooths match the supplied term", {
  err_msg <- "None of the terms matched a smooth."
  expect_error(which_smooths(m_gam, "foo"), err_msg, fixed = TRUE)
  expect_error(which_smooths(m_gamm, "foo"), err_msg, fixed = TRUE)
  expect_error(which_smooths(m_bam, "foo"), err_msg, fixed = TRUE)

  expect_identical(2L, which_smooths(m_gam, "s(x1)"))
  expect_identical(2L, which_smooths(m_gamm, "s(x1)"))
  expect_identical(2L, which_smooths(m_bam, "s(x1)"))

  expect_identical(2L, which_smooth(m_gamm, "s(x1)"))
})

test_that("which_smooths throws error for objects It can't handle", {
  expect_error(which_smooths(su_eg1, terms = "foo"),
    "Don't know how to identify smooths for <tbl_df>",
    fixed = TRUE
  )
  expect_error(which_smooths(su_eg1),
    "Don't know how to identify smooths for <tbl_df>",
    fixed = TRUE
  )
})

test_that("fix_offset can replace and offset only if there is one", {
  ## df <- gamSim(1, n = 100, dist = "normal", verbose = FALSE)
  m <- gam(y ~ s(x0) + s(x1) + offset(x2), data = su_eg1, method = "REML")
  off_val <- 1L

  expect_silent(fixed <- fix_offset(m, model.frame(m),
    offset_val = off_val
  ))
  expect_identical(c("y", "x2", "x0", "x1"), names(fixed))
  expect_true(all(fixed[["x2"]] == off_val))

  # originally had this model
  # m <- gam(y ~ s(x0) + s(x1), data = df, method = "REML")
  expect_identical(
    model.frame(m_gam),
    fix_offset(m_gam, model.frame(m_gam),
      offset_val = off_val
    )
  )
})

## test coverage_ functions
test_that("coverage_normal works for given level", {
  expect_silent(coverage_normal(0.95))
})

test_that("coverage_normal fails level outside valid range", {
  expect_error(coverage_normal(2),
    "Invalid 'level': must be 0 < level < 1")
})

test_that("coverage_t works for given level", {
  expect_silent(coverage_t(0.95, df = 5))
})

test_that("coverage_t fails level outside valid range", {
  expect_error(coverage_t(2),
    "Invalid 'level': must be 0 < level < 1")
})

test_that("parametric_terms works for a gaulss GAM", {
  data(mcycle, package = "MASS")
  m1 <- gam(list(accel ~ s(times), ~ s(times)),
    data = mcycle, method = "REML",
    family = gaulss()
  )
  expect_equal(parametric_terms(m1), character(0))
})

test_that("parametric_terms works for a gaussian GAM", {
  data(mcycle, package = "MASS")
  m1 <- gam(accel ~ s(times),
    data = mcycle, method = "REML",
    family = gaussian()
  )
  expect_equal(parametric_terms(m1), character(0))
})

test_that("parametric_terms works for a gaussian GAM", {
  expect_error(parametric_terms(character(0)),
    "Don't know how to identify parametric terms from <character>",
    fixed = TRUE
  )
})

test_that("load_mgcv returns invisibly", {
  out <- expect_invisible(load_mgcv())
  expect_true(out)
})

test_that("is_gamm4 returns true for a gamm4 model", {
  expect_true(is_gamm4(m_gamm4))
})

test_that("is_gamm4 returns false for something that isn't a gamm4 model object", {
  expect_false(is_gamm4(m_gam))
  expect_false(is_gamm4(m_gamgcv))
  expect_false(is_gamm4(m_bam))
  expect_false(is_gamm4(m_gamm))
  expect_false(is_gamm4(list(gam = 1:3, mer = 1:4)))
})

test_that("term_names works with a gam", {
  expect_silent(tn <- term_names(m_gam))
})

test_that("term_names works with a mgcv smooth", {
  expect_silent(tn <- term_names(get_smooth(m_gam, term = "s(x0)")))
  expect_identical(tn, "x0")

  expect_silent(tn <- term_names(get_smooth(su_m_factor_by,
    term = "s(x2):fac2"
  )))
  expect_identical(tn, c("x2", "fac"))
})

test_that("term_names fails if not a gam", {
  skip_on_cran()
  expect_error(tn <- gratia:::term_names.gam(m_glm),
    "`object` does not contain `pred.formula`; is this is fitted GAM?",
    fixed = TRUE
  )
})

test_that("term_names works with a gamm", {
  expect_silent(tn <- term_names(m_gamm))
})

test_that("is_factor_term works", {
  expect_false(ft <- is_factor_term(m_para_sm, term = "x0"))
  expect_true(ft <- is_factor_term(m_para_sm, term = "ff"))
  expect_null(ft <- is_factor_term(m_gam, term = "s(x0)"))
})

test_that("is_factor_term works for a bam", {
  expect_null(ft <- is_factor_term(m_bam, term = "s(x0)"))
})

test_that("is_factor_term works for a gamm", {
  expect_null(ft <- is_factor_term(m_gamm, term = "s(x0)"))
})

test_that("is_factor_term works for a gamm4", {
  expect_null(ft <- is_factor_term(m_gamm4, term = "s(x0)"))
})

test_that("term_variables works for a gam", {
  expect_identical(
    term_variables(m_para_sm, term = "fac:ff"),
    c("fac", "ff")
  )
})

test_that("term_variables works for a terms", {
  expect_identical(
    term_variables(terms(m_para_sm), term = "fac:ff"),
    c("fac", "ff")
  )
})

test_that("transform_fun works for parametric_effects", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_message(
    pe <- parametric_effects(m_para_sm,
      data = df_2_fac,
      envir = teardown_env()
    ),
    "Interaction terms are not currently supported."
  )
  expect_silent(pe <- transform_fun(pe, fun = abs))
  expect_true(all(!pe$.partial < 0L))
})

test_that("transform_fun works for smooth_estimates", {
  expect_silent(sm <- smooth_estimates(m_gam, select = "s(x1)"))
  expect_silent(sm <- transform_fun(sm, fun = exp))
})

test_that("transform_fun works for tbl", {
  expect_silent(tbl <- transform_fun(su_eg1, fun = abs, column = "y"))
})

test_that("transform_fun works for smooth_estimates with constant", {
  expect_silent(sm <- smooth_estimates(m_gam, select = "s(x1)"))
  expect_silent(sm <- transform_fun(sm, fun = exp, constant = coef(m_gam)[1]))
})

test_that("transform_fun works for smooth_samples with constant", {
  expect_silent(sm <- smooth_samples(m_gam, select = "s(x1)", n = 5))
  expect_silent(sm <- transform_fun(sm, fun = exp, constant = coef(m_gam)[1]))
})

test_that("transform_fun works for tbl with constant", {
  expect_silent(tbl <- transform_fun(su_eg1, fun = abs, column = "y",
  constant = 5))
})

test_that("involves_ranef_smooth works", {
  sm <- smooths(su_m_trivar_t2)
  expect_false(involves_ranef_smooth(get_smooth(su_m_trivar_t2, sm[1])))
})

test_that("null_deviance works for a gam", {
  expect_silent(nd <- null_deviance(m_bam))
  expect_identical(null_deviance(m_gam), m_gam$null.deviance)
})

test_that("null_deviance works for a gam", {
  expect_silent(nd <- null_deviance(m_bam))
  expect_identical(null_deviance(m_gam), m_bam$null.deviance)
})

test_that("null_deviance fails for an object without a null deviance", {
  expect_error(null_deviance(m_lm),
  "The null deviance is not available for <m_lm>")
})

## smooth_label
test_that("smooth_label extracts the smooth label from a GAM", {
  expect_silent(lab <- smooth_label(m_gam$smooth[[1]]))
  expect_identical(lab, "s(x0)")

  labs <- vapply(m_gam$smooth, FUN = smooth_label, FUN.VALUE = character(1L))
  expect_identical(labs, c("s(x0)", "s(x1)", "s(x2)", "s(x3)"))
})

test_that("smooth_label works for a gam object", {
  expect_identical(smooth_label(m_gam, id = 1), "s(x0)")
  expect_identical(
    smooth_label(m_gam),
    c("s(x0)", "s(x1)", "s(x2)", "s(x3)")
  )
})

test_that("norm_minus_one_to_one works", {
  expect_silent(x <- norm_minus_one_to_one(0:10))
  expect_equal(seq(-1, 1, by = 0.2), x)
  expect_equal(min(x), -1.0)
  expect_equal(max(x), 1.0)
  expect_identical(length(x), length(0:10))
  expect_identical(range(x), c(-1, 1))

  expect_silent(x <- norm_minus_one_to_one(-10:10))
  expect_equal(seq(-1, 1, by = 0.1), x)
  expect_equal(min(x), -1.0)
  expect_equal(max(x), 1.0)
  expect_identical(length(x), length(-10:10))
  expect_identical(range(x), c(-1, 1))
})

test_that("norm_minus_one_to_one works with NA", {
  expect_silent(x <- norm_minus_one_to_one(c(0:10, NA)))
  expect_equal(c(seq(-1, 1, by = 0.2), NA), x)
  expect_equal(min(x, na.rm = TRUE), -1.0)
  expect_equal(max(x, na.rm = TRUE), 1.0)
  expect_identical(length(x), length(c(0:10, NA)))
  expect_identical(range(x, na.rm = TRUE), c(-1, 1))
})

test_that("model_constant returns the intercept estimate", {
  expect_silent(b <- model_constant(m_gam))
  expect_type(b, "double")
  ref <- unname(coef(m_gam)[1L])
  attr(ref, "par_names") <- "location"
  expect_identical(b, ref)
  expect_named(b, expected = NULL)
})

test_that("is bam identifies a BAM vs GAM or GAMMs", {
  expect_true(is.bam(m_bam))
  expect_false(is.bam(m_gam))
  expect_false(is.bam(m_gamm))
  expect_false(is.bam(m_gamm4))
})

test_that("get smooths by id works for gamm4", {
  expect_silent(sm <- get_smooths_by_id(m_gamm4, 1))
  expect_true(is_mgcv_smooth(sm[[1]]))
  expect_identical(sm[[1]], m_gamm4$gam$smooth[[1]])

  expect_error(get_smooths_by_id(list(a = 10), 1),
    "Not a gamm4 model fit.")
})

test_that("by smooth failure throws the right error", {
  msg <- "Hmm, something went wrong identifying the requested smooth. Found:\n s(x0), s(x1), s(x2), s(x3) \nNot all of these are 'by' variable smooths. Contact Maintainer."
  expect_identical(by_smooth_failure(m_gam$smooth), msg)
})

test_that("rep first factor value works", {
  expect_silent(f <- factor(letters[1:3]))
  expect_identical(rep_first_factor_value(factor(letters[1:3]), 2),
    factor(rep("a", 2), levels = letters[1:3]))
})

test_that("check user select smooths fails with error is some missing", {
  expect_error(check_user_select_smooths(smooths(m_gam),
    select = c("s(x1)", "s(x4)")),
  "Some smooths in 'select' were not found in model :\\n\\ts\\(x4\\)")
})

test_that("check user select smooths fails with error if invalid select", {
  expect_error(check_user_select_smooths(smooths(m_gam),
    select = list(1:10)),
  "'select' is not numeric, character, or logical.")
})

test_that("is factor term errors if term missing", {
  expect_error(is_factor_term(terms(m_gam)),
    "Argument 'term' must be provided.")
})

test_that("is factor term errors if generic list", {
  expect_error(is_factor_term(list(1:10), "x0"),
    "Don't know how to handle generic list objects.")
})

test_that("is factor term is FALSE with list of terms none factor", {
  expect_false(is_factor_term(list(terms(m_gam), terms(m_bam)), "x0"))
})

test_that("term variables returns variables for term in a bam", {
  expect_identical(term_variables(m_bam, "x0"), "x0")
})

test_that("is isotropic smooth works", {
  expect_true(is_isotropic_smooth(get_smooths_by_id(su_m_bivar)[[1]]))
  expect_true(is_isotropic_smooth(get_smooths_by_id(su_m_bivar_ds)[[1]]))
  expect_false(is_isotropic_smooth(get_smooths_by_id(m_gam)[[1]]))
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

test_that("dispersion works for a GAM", {
  expect_identical(dispersion(m_gam), m_gam$sig2)
})

test_that("dispersion works for a GLM", {
  expect_identical(dispersion(m_glm), summary(m_glm)$dispersion)
})

test_that("n_eta work", {
  expect_identical(n_eta(m_gam), 1L)
  expect_identical(n_eta(m_accel), 2L)
})

test_that("model_constant works for a GAMLSS", {
  expect_length(model_constant(m_accel), 2L)
  expect_length(model_constant(m_gam), 1L)
})

test_that("rtw works for twlss model", {
  skip_on_cran()
  skip_on_ci()
  fit <- fitted(m_twlss)
  tw_pars <- get_tw_bounds(m_twlss)
  expect_snapshot(
    with_seed(
      123,
      rtw(
        fit[, 1],
        theta_2_power(fit[, 2], a = tw_pars[1], b = tw_pars[2]),
        exp(fit[, 3]))
    )
  )
})

test_that("if a multivariate model is identified correctly", {
  expect_identical(is_multivariate_y(m_mvn), TRUE)
  expect_identical(is_multivariate_y(m_gam), FALSE)
})
