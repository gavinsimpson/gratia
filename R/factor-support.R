# Compare categorical predictor support, not estimability. In particular, an
# additive model can predict an unobserved combination of known factor levels.
# The stored frame is authoritative after fitting subset and NA exclusions;
# original data or raw variables underlying transformed factors are not needed.
inform_unobserved_factor_combinations <- function(model, layout) {
  if (layout$training || !any(!is.na(layout$input_rows))) {
    return(invisible(NULL))
  }
  training <- delete_response(model, data = stats::model.frame(model))
  categorical <- vapply(training, function(x) is.factor(x) || is.character(x), logical(1L))
  factors <- names(training)[categorical]
  if (length(factors) < 2L) {
    return(invisible(NULL))
  }
  prediction <- delete_response(model, data = layout$data)
  known <- rep(TRUE, nrow(prediction))
  for (nm in factors) {
    # Character prediction columns are accepted for fitted factor predictors.
    # Unknown levels are left to the native predictor's diagnostics; they must
    # not be described as unseen combinations of otherwise known levels.
    known_levels <- if (is.factor(training[[nm]])) levels(training[[nm]]) else
      unique(training[[nm]])
    known <- known & !is.na(prediction[[nm]]) &
      as.character(prediction[[nm]]) %in% known_levels
    prediction[[nm]] <- as.character(prediction[[nm]])
    training[[nm]] <- as.character(training[[nm]])
  }
  matched <- vctrs::vec_match(prediction[factors], unique(training[factors]))
  rows <- layout$input_rows[known & is.na(matched)]
  if (!length(rows)) {
    return(invisible(NULL))
  }
  shown <- paste(utils::head(rows, 10L), collapse = ", ")
  if (length(rows) > 10L) shown <- paste0(shown, ", ...")
  cli::cli_inform(c(
    "Prediction data contain {length(rows)} row{?s} with combinations of categorical predictor levels absent from the fitting data.",
    "i" = "Factors: {.var {factors}}. Input rows: {shown}."
  ), class = "gratia_unobserved_factor_combinations")
  invisible(rows)
}
