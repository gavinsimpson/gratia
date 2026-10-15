# Internal audit of stored fitting information. Never evaluate the model call:
# its method can be an unavailable symbol, and mgcv can change it during fitting.
sampling_smoothing_info <- function(model) {
  method <- model$method
  bam <- inherits(model, "bam")
  discrete <- bam && !is.null(model$dinfo)
  fam <- stats::family(model)
  nth <- as.integer(fam$n.theta %||% 0L)
  q <- length(model$sp)
  likelihood <- method %in% c("ML", "REML", "fREML")
  # Extended-family bam updates theta separately from the smoothing optimiser.
  # Its scalar parameter ordering has not been validated for this research code.
  known_layout <- likelihood && !(bam && inherits(fam, "extended.family"))
  layout <- function(V, fast = FALSE) {
    if (!known_layout || !is.matrix(V)) return(NULL)
    scale <- isTRUE(model$scale.estimated) && !fast
    expected <- nth + q + as.integer(scale)
    if (!identical(dim(V), c(expected, expected))) return(NULL)
    list(family = seq_len(nth), log_sp = nth + seq_len(q),
      log_scale = if (scale) expected else integer(),
      family_scale = "family internal parameterisation")
  }
  theta <- if (is.function(fam$getTheta)) fam$getTheta() else NULL
  theta_response <- if (is.function(fam$getTheta))
    tryCatch(fam$getTheta(TRUE), error = function(e) NULL) else NULL
  V <- model$V.sp
  sp_vcov <- mgcv::sp.vcov(model, edge.correct = FALSE)
  list(sp = model$sp, full.sp = model$full.sp,
    V.sp = V, spind = attr(V, "spind"), sp.vcov = sp_vcov,
    V_sp_layout = layout(V, fast = bam && identical(method, "fREML")),
    sp_vcov_layout = layout(sp_vcov), db.drho = model$db.drho,
    Vc = model$Vc, method = method,
    family_parameters = list(estimated = nth, internal = theta,
      response = theta_response, sampled = FALSE),
    fitting = list(requested_method = model$call$method %||%
        if (bam) "fREML" else "GCV.Cp",
      actual_method = method, stored_score_label = names(model$gcv.ubre),
      stored_optimizer = model$optimizer, discrete = discrete))
}
