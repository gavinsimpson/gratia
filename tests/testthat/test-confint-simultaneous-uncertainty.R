test_that("confint simultaneous intervals honour overall uncertainty and covariance", {
  model <- smooth_band_model
  nd <- smooth_band_data[1:11, ]
  for (overall in c(FALSE, TRUE)) for (frequentist in c(FALSE, TRUE)) {
    for (unconditional in c(FALSE, TRUE)) {
      reference <- smooth_band_reference(model, "s(x)", nd, overall,
        frequentist, unconditional)
      actual <- confint(model, parm = "s(x)", data = nd, type = "simultaneous",
        level = 0.9, nsim = 200, seed = 314, ncores = 1,
        frequentist = frequentist, unconditional = unconditional,
        overall_uncertainty = overall)
      expect_smooth_band_reference(actual, reference)
    }
  }
})
