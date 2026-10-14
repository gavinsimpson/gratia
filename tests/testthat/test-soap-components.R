test_that("soap component classes have distinct supported types", {
  expect_s3_class(m_soap_sep$smooth[[1]], "sf.film")
  expect_s3_class(m_soap_sep$smooth[[2]], "sf.wiggly")
  expect_identical(vapply(m_soap_sep$smooth, smooth_type, character(1)),
    c("Soap film (boundary)", "Soap film (wiggly)"))
  expect_false(exists("smooth_type.sf", asNamespace("gratia"), inherits = FALSE))
  expect_false(exists("smooth_type.sw", asNamespace("gratia"), inherits = FALSE))
  for (sm in m_soap_sep$smooth) {
    expect_identical(boundary(sm), lapply(soap_fsb, `[`, c("v", "w")))
    expect_true(is_soap_film(sm))
    expect_identical(unique(penalty(sm)$.type), smooth_type(sm))
  }
  bf <- basis(m_soap_sep, data = soap_data[1:5, ])
  expect_setequal(unique(bf$.type),
    c("Soap film (boundary)", "Soap film (wiggly)"))
})

test_that("split component estimates and standard errors match mgcv by position", {
  for (model in list(m_soap_sep, m_soap_sep_known, m_soap_tensor)) {
    d <- model.frame(model)[1:8, ]
    reference <- predict(model, newdata = d, type = "terms", se.fit = TRUE)
    estimates <- smooth_estimates(model, data = d, unnest = FALSE,
      overall_uncertainty = FALSE)
    expect_identical(nrow(estimates), 2L)
    for (i in seq_along(model$smooth)) {
      values <- estimates$data[[i]]
      if (".bndry" %in% names(values)) values <- values[!values$.bndry, ]
      expect_equal(values$.estimate, unname(reference$fit[, i]), tolerance = 1e-7)
      expect_equal(values$.se, unname(reference$se.fit[, i]), tolerance = 1e-7)
      selected <- smooth_estimates(model, select = i, data = d,
        overall_uncertainty = FALSE)
      if (".bndry" %in% names(selected)) selected <- selected[!selected$.bndry, ]
      expect_equal(selected$.estimate, values$.estimate)
      expect_equal(selected$.se, values$.se)
    }
  }
})

test_that("component clipping preserves holes, boundary loops and offsets", {
  model <- m_soap_sep_known
  # gam.side() clears vn in models with repeated smooth covariates.
  expect_null(model$smooth[[1]]$vn)
  uncut <- smooth_estimates(model, n_2d = 10, clip = FALSE, unnest = FALSE)
  clipped <- smooth_estimates(model, n_2d = 10, clip = TRUE, unnest = FALSE)
  for (i in 1:2) {
    a <- uncut$data[[i]]; b <- clipped$data[[i]]
    expect_equal(nrow(a[!a$.bndry, ]), 100L)
    expect_lt(nrow(b), nrow(a))
    expect_true(all(mgcv::in.out(soap_component_boundary,
      as.matrix(b[!b$.bndry, c("v", "w")]))))
    expect_true(any(!is.finite(a$.estimate[!a$.bndry])))
    expect_setequal(b$.loop[b$.bndry], 1:2)
    expect_equal(sum(b$.bndry), sum(lengths(lapply(soap_component_boundary, `[[`, "v"))))
  }
  sm <- model$smooth[[1]]
  d <- model.frame(model)[1:8, ]
  X <- mgcv::PredictMat(sm, d)
  expect_true(any(abs(attr(X, "offset")) > 0))
  expected <- drop(X %*% coef(model)[sm$first.para:sm$last.para]) + attr(X, "offset")
  actual <- smooth_estimates(model, select = 1, data = d, overall_uncertainty = FALSE)
  expect_equal(actual$.estimate[!actual$.bndry], expected, tolerance = 1e-7)
})

test_that("split component plotting keeps separate panels and observations", {
  for (grouped in c(FALSE, TRUE)) {
    for (surface in c("raster", "tile")) {
      panels <- assemble(m_soap_sep, n_2d = 7, grouped_by = grouped,
        surface = surface, residuals = TRUE)
      expect_length(panels, 2L)
      expect_silent(lapply(panels, ggplot2::ggplot_build))
      sm <- smooth_estimates(m_soap_sep, n_2d = 7, clip = TRUE)
      saved <- draw(sm, grouped_by = grouped, surface = surface)
      expect_silent(ggplot2::ggplot_build(saved))
      expect_length(split_smooth_components(sm), 2L)
      nested <- smooth_estimates(m_soap_sep, n_2d = 7, clip = TRUE, unnest = FALSE)
      expect_silent(ggplot2::ggplot_build(draw(nested, grouped_by = grouped,
        surface = surface)))
    }
  }
  # Numeric selection must remain positional even though labels coincide.
  for (i in 1:2) {
    expect_length(assemble(m_soap_sep, select = i, n_2d = 7), 1L)
  }
  observations <- nested_component_observations(m_soap_sep, 1:2)
  expect_equal(lengths(observations$rug_data), c(2L, 2L))
  expect_equal(nrow(observations$rug_data[[1]]), nrow(soap_data))
  residual <- nested_component_observations(m_soap_sep, 1:2, partial = TRUE)
  ref <- predict(m_soap_sep, type = "terms") +
    residuals(m_soap_sep, "working") * sqrt(weights(m_soap_sep, "working"))
  for (i in 1:2) {
    expect_equal(residual$partial_residual[[i]]$partial_residual,
      unname(ref[, i]), tolerance = 1e-7)
  }
  expect_identical(smooths(m_soap_sep), c("s(v,w)", "s(v,w)"))
})

test_that("component 3D plots retain soap domain boundaries", {
  skip_if_not_installed("ggcube")
  skip_if_not_installed("sf")
  skip_if_not_installed("geometry")
  for (entry in c("model", "estimates")) {
    panels <- if (entry == "model") {
      assemble(m_soap_sep_known, surface = "3d", n_2d = 10, rug = FALSE)
    } else {
      draw(smooth_estimates(m_soap_sep_known, n_2d = 10), surface = "3d")
    }
    for (i in 1:2) {
      p <- panels[[i]]
      expect_s3_class(p$coordinates, "Coord3D")
      scene <- p$layers[[1]]$data
      edges <- scene[grepl("^boundary:", scene$.surface_face), ]
      expect_gt(nrow(edges), 0L)
      expect_true(all(edges$.surface_floor))
      expect_silent(ggplot2::ggplotGrob(p))
    }
  }
})

test_that("fixed soap component fill ranges ignore missing boundary values", {
  panels <- assemble(m_soap_sep, scales = "fixed", n_2d = 10, rug = FALSE)
  limits <- lapply(panels, function(p) {
    ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")$get_limits()
  })
  expect_length(limits, 2L)
  expect_true(all(is.finite(unlist(limits))))
  expect_equal(limits[[1]], limits[[2]])
  # Both components contain boundary metadata rows with missing effect values.
  estimates <- smooth_estimates(m_soap_sep, n_2d = 10,
    clip = TRUE, unnest = FALSE, interval = "confidence")
  expect_true(all(vapply(estimates$data, function(d) {
    any(d$.bndry & is.na(d$.estimate))
  }, logical(1))))
  expected <- range(unlist(lapply(estimates$data, function(d) {
    c(d$.estimate, d$.lower_ci, d$.upper_ci)
  })), na.rm = TRUE)
  expect_equal(limits[[1]], expected)
})

test_that("soap component fitting is unaffected by attaching sf", {
  skip_if_not_installed("sf")
  withr::local_package("sf")
  model <- update(m_soap_sep)
  for (i in 1:2) {
    expect_false(any(class(model$smooth[[i]]) %in% c("sf", "sw")))
  }
  expect_s3_class(model$smooth[[1]], "sf.film")
  expect_s3_class(model$smooth[[2]], "sf.wiggly")
  expect_silent(smooth_estimates(model, n_2d = 7, clip = TRUE))
  expect_silent(lapply(assemble(model, n_2d = 7), ggplot2::ggplot_build))
})

test_that("tensor components sharing both label and type retain fitted indices", {
  model <- m_soap_tensor
  sm <- smooth_estimates(model, n_2d = 7, n_3d = 2)
  expect_identical(unique(sm$.smooth_id), 1:2)
  expect_length(split_smooth_components(sm), 2L)
  nested <- smooth_estimates(model, n_2d = 7, n_3d = 2, unnest = FALSE)
  expect_identical(nested$.smooth_id, 1:2)
  expect_false(".smooth_id" %in% names(smooth_estimates(m_soap_sep, n_2d = 7)))
  for (grouped in c(FALSE, TRUE)) {
    panels <- assemble(model, n_2d = 7, n_3d = 2, grouped_by = grouped,
      residuals = TRUE)
    expect_length(panels, 2L)
    expect_silent(lapply(panels, ggplot2::ggplot_build))
    for (values in list(sm, nested)) {
      drawn <- draw(values, grouped_by = grouped)
      expect_silent(ggplot2::ggplot_build(drawn))
    }
  }
  # Bracket and dplyr subsets keep the explicit identity column intact.
  second <- sm[sm$.smooth_id == 2L, ]
  expect_identical(unique(second$.smooth_id), 2L)
  expect_length(split_smooth_components(second), 1L)
  expect_silent(ggplot2::ggplot_build(draw(second)))
  filtered <- dplyr::filter(sm, .smooth_id == 1L)
  expect_identical(unique(filtered$.smooth_id), 1L)
  expect_silent(ggplot2::ggplot_build(draw(filtered)))
  expect_identical(smooths(model), c("te(v,w,time)", "te(v,w,time)"))
})
