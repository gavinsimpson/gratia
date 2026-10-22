# Run after regenerating the example report, with pkgload::load_all() and
# testthat::test_file() from the repository.
# The example files are excluded from the source package, so these checks run
# separately from the installed-package tests.
skip_if_not_installed("GJRM", minimum_version = "0.2-6")
example_dir <- testthat::test_path("..", "..", "notes", "gjrm-support")
checker <- new.env(parent = environment())
sys.source(file.path(example_dir, "check-smooths.R"), envir = checker)
models <- readRDS(file.path(example_dir, "gjrm-example-models.rds"))

test_that("all example smooths have successful nonempty numerical comparisons", {
  expected <- c(gaussian = 5L, binary_lognormal = 3L,
    count_gaussian = 3L, factor_by = 4L, tensor = 3L,
    ordinal = 3L, fixed_margins = 1L)
  expect_silent(results <- lapply(models, checker$check_example_smooths))
  expect_identical(vapply(results, nrow, integer(1)), expected)
  expect_equal(sum(vapply(results, nrow, integer(1))), 22)
  comparisons <- do.call(rbind, results)
  expect_equal(sum(comparisons$evaluation_rows), 7320)
  expect_true(all(comparisons$evaluation_rows > 0L))
  expect_true(all(is.finite(comparisons$estimate_error)))
  expect_true(all(is.finite(comparisons$standard_error_error)))
  expect_true(all(comparisons$estimate_error < 1e-10))
  expect_true(all(comparisons$standard_error_error < 1e-10))
  expect_equal(sum(endsWith(results$factor_by$smooth, ":fb")), 2)
  expect_identical(results$factor_by$evaluation_rows, rep(180L, 4))
})

test_that("factor-by checks select both levels even when the fitting rows are grouped", {
  fit <- models$factor_by
  expect_identical(unique(as.character(fit$gam1$model$f[1:20])), "a")
  supplied <- list()
  record_estimate <- function(object, select, data, ...) {
    supplied[[select]] <<- data
    gratia::smooth_estimates(object, select = select, data = data, ...)
  }
  expect_silent(checker$check_example_smooths(fit, estimate = record_estimate))
  expect_identical(vapply(supplied, function(d) unique(as.character(d$f)),
    character(1)), c("a", "b", "a", "b"))
  expect_identical(vapply(supplied, nrow, integer(1)), rep(180L, 4))
})

test_that("random-effect checks include every fitted group level", {
  for (name in c("gaussian", "tensor")) {
    groups <- NULL
    record_estimate <- function(object, select, data, ...) {
      if (select == 2L) groups <<- unique(as.character(data$group))
      gratia::smooth_estimates(object, select = select, data = data, ...)
    }
    expect_silent(checker$check_example_smooths(models[[name]],
      estimate = record_estimate))
    expect_setequal(groups, levels(models[[name]]$gam1$model$group))
    expect_length(groups, 12L)
  }
})

test_that("numerical comparisons reject missing, empty or misaligned results", {
  good <- data.frame(.estimate = c(1, 2), .se = c(.1, .2))
  expect_equal(unname(checker$check_example_values(good, c(1, 2), c(.1, .2), 2)),
    c(0, 0))
  expect_error(checker$check_example_values(good[".estimate"], c(1, 2), c(.1, .2), 2),
    "requires .estimate and .se columns", fixed = TRUE)
  expect_error(checker$check_example_values(good[FALSE, ], numeric(), numeric(), 0),
    "nonempty results with matching row counts")
  expect_error(checker$check_example_values(good[1, ], c(1, 2), c(.1, .2), 2),
    "nonempty results with matching row counts")
  expect_error(checker$check_example_values(good, 1, c(.1, .2), 2),
    "nonempty results with matching row counts")
  for (bad in c(NA_real_, NaN, Inf, -Inf)) {
    values <- good
    values$.estimate[1] <- bad
    expect_error(checker$check_example_values(values, c(1, 2), c(.1, .2), 2),
      "finite numeric")
    expect_error(checker$check_example_values(good, c(bad, 2), c(.1, .2), 2),
      "finite numeric")
    expect_error(checker$check_example_values(good, c(1, 2), c(bad, .2), 2),
      "finite numeric")
  }
  expect_error(checker$check_example_values(
    data.frame(.estimate = 1e308, .se = .1), -1e308, .1, 1),
    "non-finite comparison error")
})

test_that("the example checker fails on warnings, absent levels or incorrect estimates", {
  fit <- models$factor_by
  warn <- function(...) warning("unexpected evaluation warning")
  expect_error(checker$check_example_smooths(fit, estimate = warn),
    "Numerical smooth check raised a warning: unexpected evaluation warning",
    fixed = TRUE)
  empty <- function(...) data.frame(.estimate = double(), .se = double())
  expect_error(checker$check_example_smooths(fit, estimate = empty),
    "nonempty results with matching row counts")
  incorrect <- function(...) {
    out <- gratia::smooth_estimates(...)
    out$.estimate <- out$.estimate + .1
    out
  }
  expect_error(checker$check_example_smooths(fit, estimate = incorrect),
    "estimates or standard errors disagree")
  fit$gam1$model <- fit$gam1$model[fit$gam1$model$f == "a", , drop = FALSE]
  expect_error(checker$check_example_smooths(fit), "No evaluation rows.*:fb")
})



test_that("the rendered report records all checks without warnings", {
  report <- xml2::read_html(file.path(example_dir, "gjrm-examples.html"))
  section <- xml2::xml_find_first(report,
    "//*[@id='numerical-checks-of-the-example-smooths']")
  expect_false(inherits(section, "xml_missing"))
  expect_length(xml2::xml_find_all(section, ".//table"), 1L)
  text <- gsub("[[:space:]]+", " ", xml2::xml_text(section))
  expect_false(grepl("## Warning|## Error", text))
  expect_match(text, "All 22 smooths")
  expect_match(text, "7,320 evaluation rows")
  summary <- read.csv(file.path(example_dir, "coefficient-checks.csv"),
    row.names = 1L)
  details <- read.csv(file.path(example_dir, "coefficient-check-details.csv"))
  expect_equal(nrow(details), 22L)
  expect_equal(sum(summary$smooths), 22)
  expect_equal(sum(summary$evaluation_rows), 7320)
  expect_equal(sum(details$evaluation_rows), 7320)
  expect_true(all(details$evaluation_rows > 0))
  expect_true(all(is.finite(details$estimate_error)))
  expect_true(all(is.finite(details$standard_error_error)))
})
