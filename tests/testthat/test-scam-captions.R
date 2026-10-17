skip_if_not_installed("scam", minimum_version = "1.2-22")

scam_caption_expectations <- c(
  mpi = "P spline (increasing)", mpd = "P spline (decreasing)",
  cx = "P spline (convex)", cv = "P spline (concave)",
  micx = "P spline (increasing, convex)",
  micv = "P spline (increasing, concave)",
  mdcx = "P spline (decreasing, convex)",
  mdcv = "P spline (decreasing, concave)",
  miso = "P spline (increasing, zero left)",
  mifo = "P spline (increasing, zero right)",
  po = "P spline (positive)", ipo = "P spline (increasing, positive)",
  dpo = "P spline (decreasing, positive)",
  cpopspline = "Cyclic P spline (positive)",
  lmpi = "P spline (increasing for age <= 0.5)",
  lipl = "P spline (increasing for age <= 0.5; then flat)",
  tedmi = "Tensor product; age: increasing; dose: increasing",
  tedmd = "Tensor product; age: decreasing; dose: decreasing",
  tedi = "Tensor product; age: decreasing; dose: increasing",
  teid = "Tensor product; age: increasing; dose: decreasing",
  temicx = "Tensor product; age: increasing; dose: convex",
  temicv = "Tensor product; age: increasing; dose: concave",
  tedecx = "Tensor product; age: decreasing; dose: convex",
  tedecv = "Tensor product; age: decreasing; dose: concave",
  tecxcx = "Tensor product; age: convex; dose: convex",
  tecvcv = "Tensor product; age: concave; dose: concave",
  tecxcv = "Tensor product; age: convex; dose: concave",
  tesmi1 = "Tensor product; age: increasing; dose: free",
  tesmi2 = "Tensor product; age: free; dose: increasing",
  tesmd1 = "Tensor product; age: decreasing; dose: free",
  tesmd2 = "Tensor product; age: free; dose: decreasing",
  tescx = "Tensor product; age: free; dose: convex",
  tescv = "Tensor product; age: free; dose: concave",
  tismi = "Tensor product int.; age: increasing; dose: free",
  tismd = "Tensor product int.; age: decreasing; dose: free")
by_bases <- c("mpi", "mpd", "cx", "cv", "micx", "micv", "mdcx", "mdcv")
scam_caption_expectations[paste0(by_bases, "By")] <- scam_caption_expectations[by_bases]

scam_caption_smooth <- function(basis, cyclic = FALSE) {
  tensor <- startsWith(basis, "te") || startsWith(basis, "ti")
  structure(list(label = if (tensor) "s(age,dose)" else "s(age)",
    term = if (tensor) c("age", "dose") else "age",
    dim = if (tensor) 2L else 1L, xt = list(xc = 0.5),
    margin.bs = if (cyclic) "cc" else "ps"),
    class = c(paste0(basis, ".smooth"), "mgcv.smooth"))
}

scam_caption_text <- function(caption) paste(deparse(caption), collapse = " ")

test_that("SCAM captions describe every constructor without changing smooth types", {
  bases <- names(scam_caption_expectations)
  smooths <- lapply(bases, scam_caption_smooth)
  original <- smooths
  types <- vapply(smooths, smooth_type, character(1))
  ids <- seq_along(smooths) + 10L
  model <- structure(list(smooth = smooths), class = "scam")
  info <- scam_smooth_info(model, ids = ids)
  expect_setequal(paste0(bases, ".smooth"), scam_smooth_classes())
  expect_length(info, length(bases))
  expect_identical(names(info), as.character(ids))
  actual <- vapply(info, scam_shape_caption, character(1), plotmath = FALSE)
  expect_identical(unname(actual), unname(scam_caption_expectations))
  expect_identical(smooths, original)
  expect_identical(vapply(smooths, smooth_type, character(1)), types)
  expect_true(all(vapply(info, function(x) is.expression(scam_shape_caption(x)), logical(1))))
  expect_identical(scam_smooth_info(structure(model, class = "gam")), list())
})

test_that("SCAM captions retain surrounding text, arrows and cyclic margin labels", {
  smooths <- lapply(c("mpi", "mpd", "tesmi1", "tesmi2", "lipl"), scam_caption_smooth)
  smooths[[3L]]$margin.bs <- smooths[[4L]]$margin.bs <- "cc"
  smooths[[5L]]$term <- "temperature"
  smooths[[5L]]$xt$xc <- 0.37
  model <- structure(list(smooth = smooths), class = "scam")
  info <- scam_smooth_info(model)
  expect_identical(scam_shape_caption(info[[3L]], FALSE),
    "Tensor product; age: increasing; dose: free (cyclic)")
  expect_identical(scam_shape_caption(info[[4L]], FALSE),
    "Tensor product; age: free (cyclic); dose: increasing")
  expect_identical(scam_shape_caption(info[[5L]], FALSE),
    "P spline (increasing for temperature <= 0.37; then flat)")
  expect_match(scam_caption_text(scam_shape_caption(info[[1L]])), "%up%", fixed = TRUE)
  expect_match(scam_caption_text(scam_shape_caption(info[[2L]])), "%down%", fixed = TRUE)
  local <- scam_caption_text(scam_shape_caption(info[[5L]]))
  expect_match(local, "<=", fixed = TRUE)
  expect_match(local, "temperature", fixed = TRUE)
  expect_match(local, "0.37", fixed = TRUE)
  object <- data.frame(.smooth = "s(age)", .smooth_id = 1L,
    .type = smooth_type(smooths[[1L]]), .by = NA_character_)
  attr(object, "scam_smooth_info") <- info
  default <- paste0("Facets: dose; Basis: ", object$.type, "; other plot text")
  expect_identical(scam_shape_plot_caption(object, default, FALSE),
    "Facets: dose; Basis: P spline (increasing); other plot text")
  expect_true(is.expression(scam_shape_plot_caption(object, default)))
  expect_identical(scam_shape_plot_caption(object, "custom", FALSE),
    "Basis: P spline (increasing)")
  attr(object, "scam_smooth_info") <- NULL
  expect_identical(scam_shape_plot_caption(object, default), default)
})

scam_caption_models <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    s <- mgcv::s
    d <- withr::with_seed(653, {
      d <- data.frame(age = runif(120, 0.05, 0.95), dose = runif(120, 0.05, 0.95),
        group = factor(rep(c("a", "b"), 60)))
      d$y <- exp(d$age) + sqrt(d$dose) + rnorm(120, sd = 0.08)
      d
    })
    models <- list(
      additive = scam::scam(y ~ s(age, bs = "mpi", k = 6) +
        s(dose, bs = "cv", k = 6), data = d, sp = c(0.1, 0.1)),
      by = scam::scam(y ~ group + s(age, by = group, bs = "mpi", k = 6),
        data = d, sp = c(0.1, 0.1)),
      local = scam::scam(y ~ s(age, bs = "lipl", k = 12, xt = list(xc = 0.37)),
        data = d, sp = 0.1),
      cyclic = scam::scam(y ~ s(age, dose, bs = "tesmi1", k = c(5, 5), xt = "cc"),
        data = d, sp = c(0.1, 0.1)))
    cache <<- models
    cache
  }
})

test_that("fitted SCAM captions survive saved estimates and smooth selection", {
  models <- scam_caption_models()
  for (name in c("additive", "local", "cyclic")) {
    model <- models[[name]]
    estimates <- smooth_estimates(model, n = 20, n_2d = 10, dist = 0,
      overall_uncertainty = FALSE)
    info <- attr(estimates, "scam_smooth_info")
    expect_length(info, length(model$smooth))
    expect_length(attr(estimates, "shape_constraints"), 0L)
    direct <- draw(model, n = 20, n_2d = 10, dist = 0,
      parametric = FALSE, rug = FALSE, wrap = FALSE)
    saved <- draw(estimates)
    for (i in seq_along(direct)) {
      expect_identical(saved[[i]]$labels$caption, direct[[i]]$labels$caption)
      expect_true(is.expression(saved[[i]]$labels$caption))
    }
    expect_null(draw(model, caption = FALSE, n = 20, n_2d = 10, dist = 0,
      parametric = FALSE, rug = FALSE, wrap = FALSE)[[1L]]$labels$caption)
    expect_null(draw(estimates, caption = FALSE)[[1L]]$labels$caption)
    expect_silent(ggplot2::ggplotGrob(direct[[1L]]))
  }
  model <- models$additive
  estimates <- smooth_estimates(model, n = 20, overall_uncertainty = FALSE)
  selected <- estimates[estimates$.smooth == "s(dose)", ]
  expect_identical(names(attr(selected, "scam_smooth_info")), "2")
  expect_identical(draw(selected)[[1L]]$labels$caption,
    draw(model, select = "s(dose)", n = 20, rug = FALSE, wrap = FALSE)[[1L]]$labels$caption)
  nested <- smooth_estimates(models$local, n = 20, unnest = FALSE,
    overall_uncertainty = FALSE)
  expect_identical(draw(nested)[[1L]]$labels$caption,
    draw(models$local, n = 20, rug = FALSE, wrap = FALSE)[[1L]]$labels$caption)
  local_info <- attr(nested, "scam_smooth_info")[[1L]]
  expect_identical(scam_shape_caption(local_info, FALSE),
    "P spline (increasing for age <= 0.37; then flat)")
  cyclic <- scam_smooth_info(models$cyclic)[[1L]]
  expect_identical(scam_shape_caption(cyclic, FALSE),
    "Tensor product; age: increasing; dose: free (cyclic)")
})

test_that("grouped factor-by SCAM captions describe their common basis once", {
  model <- scam_caption_models()$by
  estimates <- smooth_estimates(model, n = 20, overall_uncertainty = FALSE)
  direct <- draw(model, n = 20, grouped_by = TRUE, parametric = FALSE,
    rug = FALSE, wrap = FALSE)
  saved <- draw(estimates, grouped_by = TRUE)
  expect_length(direct, 1L)
  caption <- direct[[1L]]$labels$caption
  expect_identical(saved[[1L]]$labels$caption, caption)
  text <- scam_caption_text(caption)
  expect_identical(lengths(regmatches(text, gregexpr("%up%", text, fixed = TRUE))), 1L)
  expect_length(attr(estimates, "scam_smooth_info"), 2L)
  expect_length(attr(estimates, "shape_constraints"), 0L)
  expect_silent(ggplot2::ggplotGrob(direct[[1L]]))
})

test_that("SCAM captions preserve literal coordinates and select duplicate labels by ID", {
  smooths <- lapply(c("lmpi", "lipl", "tesmi1"), scam_caption_smooth)
  smooths[[1L]]$label <- smooths[[2L]]$label <- "s(log(age))"
  smooths[[1L]]$term <- smooths[[2L]]$term <- "log(age)"
  smooths[[1L]]$xt$xc <- -0.5
  smooths[[2L]]$xt$xc <- -0.25
  smooths[[3L]]$term <- c("water temperature", "log(dose)")
  model <- structure(list(smooth = smooths), class = "scam")
  info <- scam_smooth_info(model)
  expect_identical(scam_shape_caption(info[[3L]], FALSE),
    "Tensor product; water temperature: increasing; log(dose): free")
  expression_caption <- scam_caption_text(scam_shape_caption(info[[3L]]))
  expect_match(expression_caption, "water temperature", fixed = TRUE)
  expect_match(expression_caption, "log(dose)", fixed = TRUE)
  object <- data.frame(.smooth = "s(log(age))", .smooth_id = 2L,
    .type = smooth_type(smooths[[2L]]), .by = NA_character_)
  attr(object, "scam_smooth_info") <- info
  caption <- scam_shape_plot_caption(object, paste0("Basis: ", object$.type), FALSE)
  expect_identical(caption,
    "Basis: P spline (increasing for log(age) <= -0.25; then flat)")
  expression_caption <- scam_caption_text(scam_shape_plot_caption(object,
    paste0("Basis: ", object$.type)))
  expect_match(expression_caption, "log(age)", fixed = TRUE)
  expect_match(expression_caption, "-0.25", fixed = TRUE)
  expect_false(grepl("-0.5", expression_caption, fixed = TRUE))
})

test_that("local SCAM captions retain model coordinates on original-scale plots", {
  s <- mgcv::s
  d <- withr::with_seed(873, {
    d <- data.frame(age = exp(seq(-2, 0, length.out = 100)))
    d$y <- pmin(log(d$age), -0.75) + rnorm(100, sd = 0.06)
    d
  })
  model <- scam::scam(y ~ s(log(age), bs = "lipl", k = 12,
    xt = list(xc = -0.75)), data = d, sp = 0.1)
  estimates <- smooth_estimates(model, n = 20, back_transform = TRUE,
    overall_uncertainty = FALSE)
  info <- attr(estimates, "scam_smooth_info")[[1L]]
  expect_identical(scam_shape_caption(info, FALSE),
    "P spline (increasing for log(age) <= -0.75; then flat)")
  direct <- draw(model, n = 20, back_transform = TRUE, rug = FALSE,
    parametric = FALSE, wrap = FALSE)[[1L]]
  saved <- draw(estimates, back_transform = TRUE)[[1L]]
  expect_identical(saved$labels$caption, direct$labels$caption)
  text <- scam_caption_text(direct$labels$caption)
  expect_match(text, "log(age)", fixed = TRUE)
  expect_match(text, "-0.75", fixed = TRUE)
  expect_identical(direct$labels$x, "age")
  expect_silent(ggplot2::ggplotGrob(direct))
})
