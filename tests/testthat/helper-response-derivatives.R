# Native predictions with a five-point difference provide a reference separate
# from the sampled fitted values and two-point differences used by gratia.
native_response_slope <- function(model, data, focal, type = "response") {
  h <- 1e-4
  at <- function(shift) {
    nd <- data
    nd[[focal]] <- nd[[focal]] + shift
    predict(model, newdata = nd, type = type)
  }
  unname((at(-2 * h) - 8 * at(-h) + 8 * at(h) - at(2 * h)) / (12 * h))
}
