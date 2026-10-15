test_that("ordinary and scalable QQ and worm plots share labels", {
  labels <- c("x", "y", "title", "subtitle", "caption", "colour")
  for (model in list(m_tiny_eg1, b_pois, gfam_model)) {
    cases <- list(
      list(type = "deviance", method = "normal"),
      list(type = "deviance", method = "uniform"),
      list(type = "deviance", method = "simulate"),
      list(type = "pit"), list(type = "quantile"), list(type = "dharma")
    )
    for (case in cases) {
      args <- c(list(model = model, seed = 31, n_simulate = 3,
        n_uniform = 2, n_dharma = 20), case)
      for (worm in c(FALSE, TRUE)) {
        ordinary <- do.call(if (worm) worm_plot else qq_plot, args)
        scalable <- do.call(if (worm) scalable_worm_plot else scalable_qq_plot, args)
        for (label in labels) {
          expect_identical(scalable$labels[[label]], ordinary$labels[[label]])
        }
        expect_false(grepl("\n", ordinary$labels$subtitle, fixed = TRUE))
        expect_false(grepl("pointwise", ordinary$labels$caption %||% "", ignore.case = TRUE))
      }
    }
  }
})

test_that("diagnostic captions can be replaced or omitted", {
  model <- nlme::lme(distance ~ age, random = ~ 1 | Subject,
    data = nlme::Orthodont)
  for (conditioning in c("conditional", "marginal")) {
    for (fun in list(qq_plot, worm_plot, scalable_qq_plot, scalable_worm_plot)) {
      args <- list(model = model, conditioning = conditioning, seed = 31,
        type = "quantile", n_simulate = 3)
      p <- do.call(fun, args)
      expect_identical(p$labels$subtitle, "Method: std. normal")
      expect_identical(p$labels$caption, paste0("Conditioning: ", conditioning,
        if (conditioning == "marginal") "; joint simulation bands" else ""))
      for (caption in c("Custom caption", "")) {
        expect_identical(do.call(fun, c(args, list(caption = caption)))$labels$caption, caption)
      }
    }
    for (scalable in c(FALSE, TRUE)) {
      args <- list(model = model, conditioning = conditioning, n = 3,
        seed = 31, scalable = scalable)
      p <- do.call(posterior_predictive_plot, args)
      expect_null(p$labels$subtitle)
      expect_identical(p$labels$caption, paste("Conditioning:", conditioning))
      for (caption in c("Custom caption", "")) {
        expect_identical(do.call(posterior_predictive_plot,
          c(args, list(caption = caption)))$labels$caption, caption)
      }
    }
  }
})
