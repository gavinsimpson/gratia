# Keep SCAM's original fitting coordinates intact. Native summary and sampling
# use coefficients/Vp/Ve, whereas prediction uses coefficients.t/Vp.t/Ve.t.
# Shared gratia code extracts the latter through coef() and get_vcov().
scam_gam_adapter <- function(object) {
  if (!inherits(object, "scam")) {
    stop("A fitted scam model is required.", call. = FALSE)
  }
  if (is.null(object[["pred.formula"]])) {
    object$pred.formula <- stats::formula(stats::delete.response(stats::terms(object)))
    environment(object$pred.formula) <- environment(stats::formula(object))
  }
  class(object) <- unique(append(class(object), "gam", after = 1L))
  object
}

scam_smooth_classes <- function() {
  c("mpi.smooth", "mpd.smooth", "cx.smooth", "cv.smooth",
    "micx.smooth", "micv.smooth", "mdcx.smooth", "mdcv.smooth",
    "miso.smooth", "mifo.smooth", "po.smooth", "ipo.smooth", "dpo.smooth",
    "cpopspline.smooth", "lmpi.smooth", "lipl.smooth",
    "mpiBy.smooth", "mpdBy.smooth", "cxBy.smooth", "cvBy.smooth",
    "micxBy.smooth", "micvBy.smooth", "mdcxBy.smooth", "mdcvBy.smooth",
    "tedmi.smooth", "tedmd.smooth", "tedi.smooth", "teid.smooth",
    "temicx.smooth", "temicv.smooth",
    "tedecx.smooth", "tedecv.smooth", "tecxcx.smooth", "tecvcv.smooth",
    "tecxcv.smooth", "tesmi1.smooth", "tesmi2.smooth", "tesmd1.smooth",
    "tesmd2.smooth", "tescx.smooth", "tescv.smooth", "tismi.smooth", "tismd.smooth")
}

is_scam_smooth <- function(smooth) {
  inherits(smooth, scam_smooth_classes())
}

# Return the basis in the order of coefficients.t. These are linear maps;
# positivity has already been imposed on the coefficients themselves.
scam_smooth_matrix <- function(smooth, X) {
  offset <- attr(X, "offset")
  q <- if (!is.null(smooth[["first.para"]]) && !is.null(smooth[["last.para"]])) {
    length(smooth_coef_indices(smooth))
  } else if (is.matrix(smooth[["X"]])) ncol(smooth[["X"]]) else smooth[["df"]]
  if (isTRUE(attr(X, "gratia.fitted_columns"))) {
    if (ncol(X) != q) stop("Cannot align the SCAM smooth basis with its fitted coefficients.",
      call. = FALSE)
    attr(X, "gratia.fitted_columns") <- NULL
    return(X)
  }
  if (inherits(smooth, c("po.smooth", "ipo.smooth", "cpopspline.smooth",
      "tedmi.smooth", "tedmd.smooth", "tedi.smooth", "teid.smooth",
      "temicx.smooth", "temicv.smooth",
      "tedecx.smooth", "tedecv.smooth", "tecxcx.smooth", "tecvcv.smooth",
      "tecxcv.smooth"))) {
    X <- X[, -1L, drop = FALSE]
  } else if (inherits(smooth, "dpo.smooth")) {
    X <- X[, -ncol(X), drop = FALSE]
  } else if (inherits(smooth, c("miso.smooth", "mifo.smooth"))) {
    X <- X[, -smooth[["n.zero"]], drop = FALSE]
  } else if (inherits(smooth, "lipl.smooth")) {
    if (ncol(X) != q + 1L + smooth[["n.zero.col"]]) {
      stop("Cannot align the locally plateauing SCAM basis with its fitted coefficients.",
        call. = FALSE)
    }
    X <- X[, seq_len(q) + 1L, drop = FALSE]
  } else if (inherits(smooth, c("tesmi1.smooth", "tesmi2.smooth",
      "tesmd1.smooth", "tesmd2.smooth", "tescx.smooth", "tescv.smooth",
      "tismi.smooth", "tismd.smooth"))) {
    Z <- smooth[["Zc"]]
    if (!is.matrix(Z) || ncol(X) != nrow(Z)) {
      stop("Cannot align the SCAM basis with its fitted identifiability constraints.",
        call. = FALSE)
    }
    X <- X %*% Z
  }
  if (ncol(X) != q) {
    stop("Cannot align the SCAM smooth basis with its fitted coefficients.",
      call. = FALSE)
  }
  if (!is.null(offset)) attr(X, "offset") <- offset
  X
}

# Obtain parametric columns through native prediction so contrasts, formula
# expressions and offset conventions remain those used by the fitted model.
scam_parametric_model <- function(object) {
  p <- seq_len(object$nsdf)
  object$smooth <- list()
  object$coefficients <- object$coefficients[p]
  object$coefficients.t <- object$coefficients.t[p]
  object$edf <- object$edf[p]
  object$Vp.t <- object$Vp.t[p, p, drop = FALSE]
  object$Ve.t <- object$Ve.t[p, p, drop = FALSE]
  object
}

scam_model_matrix <- function(object, data, training = FALSE, ..., envir = NULL) {
  object <- with_model_envir(object, envir)
  dots <- list(...)
  dots[c("type", "se.fit", "terms", "exclude")] <- NULL
  param <- scam_parametric_model(object)
  native <- function(type) {
    a <- c(list(object = param, type = type, se.fit = FALSE), dots)
    if (!training) a$newdata <- data
    do.call(predict_model.default, a)
  }
  q <- length(stats::coef(object))
  if (is.null(data)) data <- object$model
  if (object$nsdf > 0L) {
    xp <- native("lpmatrix")
    n <- nrow(xp)
  } else {
    n <- nrow(data)
    xp <- matrix(numeric(), n, 0L)
  }
  if (nrow(data) != n) {
    retained <- match(rownames(xp), rownames(data))
    if (anyNA(retained)) stop("Cannot align SCAM prediction rows.", call. = FALSE)
    data <- data[retained, , drop = FALSE]
  }
  X <- matrix(0, n, q, dimnames = list(rownames(data), names(stats::coef(object))))
  if (object$nsdf > 0L) X[, seq_len(object$nsdf)] <- xp
  extra <- attributes(xp)[setdiff(names(attributes(xp)), c("dim", "dimnames"))]
  for (name in names(extra)) attr(X, name) <- extra[[name]]
  # The fitted offset includes both formula offsets and an offset argument.
  # New data use formula offsets only, evaluated with the same expression rules
  # as native prediction, including previously evaluated model-frame columns.
  param_offset <- if (training) {
    object$offset %||% rep(0, n)
  } else {
    stats::model.offset(evaluated_model_frame(object, data)) %||% rep(0, n)
  }
  if (length(param_offset) == 1L) param_offset <- rep(param_offset, n)
  if (length(param_offset) != n) stop(
    "SCAM offsets must have one value per prediction row.", call. = FALSE)
  attr(X, "model.offset") <- param_offset
  off <- matrix(0, n, length(object$smooth))
  for (i in seq_along(object$smooth)) {
    sm <- object$smooth[[i]]
    nd <- prepare_smooth_data(object, sm, data)
    raw <- predict_mat_rows(sm, nd)
    X[, smooth_coef_indices(sm)] <- scam_smooth_matrix(sm, raw)
    if (!is.null(attr(raw, "offset"))) off[, i] <- attr(raw, "offset")
  }
  if (any(off != 0, na.rm = TRUE)) attr(X, "offset") <- off
  list(X = X, offsets = off, data = data, parametric = param,
    parametric_X = xp, parametric_offset = param_offset, native = native)
}

scam_predict_terms <- function(object, context, type, terms, exclude, se.fit) {
  X <- context$X
  labels <- c(attr(object$pterms, "term.labels"), smooths(object))
  indices <- lapply(seq_along(attr(object$pterms, "term.labels")), function(i) {
    which(object$assign == i)
  })
  indices <- c(indices, lapply(object$smooth, smooth_coef_indices))
  fit <- se <- matrix(0, nrow(X), length(labels), dimnames = list(rownames(X), labels))
  V <- object$Vp.t
  beta <- stats::coef(object)
  intercept <- which(names(beta) == "(Intercept)")
  nparam <- length(attr(object$pterms, "term.labels"))
  for (i in seq_along(labels)) {
    idx <- indices[[i]]
    fit[, i] <- drop(X[, idx, drop = FALSE] %*% beta[idx])
    if (i > nparam) fit[, i] <- fit[, i] + context$offsets[, i - nparam]
    if (se.fit) {
      G <- X[, idx, drop = FALSE]
      if (type == "iterms" && i > nparam && length(intercept)) {
        idx <- c(intercept, idx)
        G <- cbind(1, G)
      }
      se[, i] <- sqrt(pmax(0, rowSums((G %*% V[idx, idx, drop = FALSE]) * G)))
    }
  }
  keep <- if (is.null(terms)) seq_along(labels) else which(labels %in% terms)
  keep <- keep[!labels[keep] %in% exclude]
  fit <- fit[, keep, drop = FALSE]
  se <- se[, keep, drop = FALSE]
  attr(fit, "constant") <- if (length(intercept)) unname(beta[intercept]) else 0
  if (se.fit) list(fit = fit, se.fit = se) else fit
}

# Internal term uncertainty refers to the returned fitted contribution. Native
# plot/predict sometimes centre only the standard errors of constrained terms.
# The direct fitted matrix keeps values and uncertainty on the same quantity.
scam_predict_model <- function(object, data, training = FALSE, ..., envir = NULL) {
  dots <- list(...)
  type <- dots$type %||% "link"
  if (!type %in% c("lpmatrix", "terms", "iterms", "link", "response")) {
    stop("Unsupported SCAM prediction type.", call. = FALSE)
  }
  context <- scam_model_matrix(object, data, training, ..., envir = envir)
  X <- context$X
  beta <- stats::coef(object)
  terms <- dots$terms
  exclude <- dots$exclude
  se.fit <- isTRUE(dots$se.fit)
  if (type %in% c("terms", "iterms")) {
    return(scam_predict_terms(object, context, type, terms, exclude, se.fit))
  }
  roles <- rep("(Intercept)", ncol(X))
  pterms <- attr(object$pterms, "term.labels")
  for (i in seq_along(pterms)) roles[which(object$assign == i)] <- pterms[i]
  for (sm in object$smooth) roles[smooth_coef_indices(sm)] <- sm$label
  selected <- if (is.null(terms)) rep(TRUE, ncol(X)) else roles %in% terms
  selected <- selected & !roles %in% exclude
  X[, !selected] <- 0
  if (type == "lpmatrix") return(X)
  param_offset <- context$parametric_offset
  keep_smooth <- vapply(object$smooth, function(sm) {
    (is.null(terms) || sm$label %in% terms) && !sm$label %in% exclude
  }, logical(1))
  offset <- param_offset + rowSums(context$offsets[, keep_smooth, drop = FALSE])
  fit <- drop(X %*% beta) + offset
  se <- if (se.fit) sqrt(pmax(0, rowSums((X %*% object$Vp.t) * X))) else NULL
  if (type == "response") {
    f <- stats::family(object)
    if (se.fit) se <- abs(f$mu.eta(fit)) * se
    fit <- f$linkinv(fit)
  }
  if (se.fit) list(fit = fit, se.fit = se) else fit
}

# Preserve the distinction between training predictions and explicit new data.
# In particular, argument offsets are retained only for training predictions.
predict_model <- function(object, newdata, ..., envir = NULL) {
  if (inherits(object, "scam")) {
    if (!missing(newdata)) return(scam_predict_model(object, data = newdata,
      training = FALSE, ..., envir = envir))
    map <- model_row_map(object)
    used <- model_used_rows(object)
    result <- scam_predict_model(used, data = NULL, training = TRUE,
      ..., envir = envir)
    if (!identical(map, seq_len(nrow(used$model)))) {
      result <- scam_restore_prediction(result, map)
    }
    return(result)
  }
  if (missing(newdata)) predict_model.default(object, ..., envir = envir) else
    predict_model.default(object, newdata = newdata, ..., envir = envir)
}

# Restore training exclusions after every component has been evaluated on the
# compact model frame. Offset attributes describe rows too, so restore them
# alongside fitted values and standard errors.
scam_restore_prediction <- function(x, map) {
  if (is.list(x)) return(lapply(x, scam_restore_prediction, map = map))
  extra <- lapply(c("model.offset", "offset"), function(name) attr(x, name, exact = TRUE))
  out <- slice_observations(x, map)
  for (i in seq_along(extra)) {
    value <- extra[[i]]
    if (is.null(value)) next
    restored <- if (is.list(value)) lapply(value, slice_observations, i = map) else
      slice_observations(value, map)
    attr(out, c("model.offset", "offset")[i]) <- restored
  }
  out
}
