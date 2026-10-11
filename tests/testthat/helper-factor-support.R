# Capture diagnostics without allowing an unexpected warning to obscure its
# assertions. All conditions remain available for explicit checks below.
capture_factor_support <- function(expr) {
  messages <- warnings <- list()
  value <- withCallingHandlers(expr,
    message = function(cnd) {
      messages[[length(messages) + 1L]] <<- cnd
      invokeRestart("muffleMessage")
    },
    warning = function(cnd) {
      warnings[[length(warnings) + 1L]] <<- cnd
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, messages = messages, warnings = warnings)
}

factor_support_newdata <- function(data) {
  # Rows 2 and 4 have known levels but an unobserved pair, including duplicates.
  d <- data[c(1, 2, 3, 2, 4), c("a", "b", "x")]
  rownames(d) <- NULL
  d
}

expect_factor_support_message <- function(result, rows = "2, 4") {
  expect_length(result$warnings, 0L)
  expect_length(result$messages, 1L)
  expect_s3_class(result$messages[[1L]], "gratia_unobserved_factor_combinations")
  expect_match(conditionMessage(result$messages[[1L]]),
    "combinations of categorical predictor levels absent")
  expect_match(conditionMessage(result$messages[[1L]]), paste0("Input rows: ", rows), fixed = TRUE)
  expect_false(grepl("Predictions are unchanged",
    conditionMessage(result$messages[[1L]]), fixed = TRUE))
  invisible(result$value)
}
