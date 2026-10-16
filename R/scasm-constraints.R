# Keep shape information separate from smooth classes: sc smooths use the
# ordinary B-spline prediction methods after construction.
scasm_pc_replaces_named_constraints <- function(smooth) {
  pc <- smooth$point.con
  length(pc) > 1L && is.list(pc[[1L]]) && length(pc[[2L]]) > 0L
}

smooth_shape_constraints <- function(model, smooths = model$smooth,
  ids = seq_along(smooths), warn = TRUE) {
  A <- model$Ain
  fitted_constraints <- is.matrix(A) && is.numeric(A) &&
    ncol(A) == length(stats::coef(model)) && all(is.finite(A))
  info <- lapply(seq_along(smooths), function(i) {
    sm <- smooths[[i]]
    tensor <- length(sm[["margin"]]) > 0L
    margins <- if (tensor) sm[["margin"]] else list(sm)
    indices <- seq.int(sm$first.para, sm$last.para)
    retained <- fitted_constraints && nrow(A) > 0L &&
      any(A[, indices, drop = FALSE] != 0) &&
      is.matrix(sm$Ain) && nrow(sm$Ain) > 0L
    fitted_margins <- margins
    replaced <- scasm_pc_replaces_named_constraints(sm)
    margins <- lapply(margins, function(margin) {
      codes <- character()
      if (inherits(margin, "Bspline.smooth")) {
        xt <- margin$xt
        if (is.list(xt)) xt <- if (length(xt)) xt[[1L]] else NULL
        if (is.character(xt)) {
          codes <- intersect(c("m+", "m-", "c+", "c-", "+"), xt)
        }
      }
      linear <- is.matrix(margin$Ain) && nrow(margin$Ain) > 0L
      list(variables = margin$term, codes = codes,
        requested = length(codes) > 0L || linear,
        retained = retained && linear && !replaced &&
          !scasm_pc_replaces_named_constraints(margin))
    })
    named_shapes <- any(vapply(margins, function(x) x$requested, logical(1)))
    linear_constraints <- (!named_shapes || replaced ||
      any(vapply(fitted_margins, scasm_pc_replaces_named_constraints, logical(1)))) &&
      is.matrix(sm$Ain) && nrow(sm$Ain) > 0L
    if (!named_shapes && !linear_constraints) return(NULL)
    margins <- lapply(seq_along(margins), function(j) {
      # Multivariate tensor margins can omit the mgcv.smooth superclass.
      # Add it only to the local copy used to describe the marginal basis.
      margin <- fitted_margins[[j]]
      class(margin) <- unique(c(class(margin), "mgcv.smooth"))
      margins[[j]]$basis <- smooth_type(margin)
      margins[[j]]
    })
    list(smooth_id = ids[i], label = sm$label, tensor = tensor,
      margins = margins, linear_constraints = linear_constraints,
      retained = retained)
  })
  names(info) <- as.character(ids)
  info <- Filter(Negate(is.null), info)
  dropped <- vapply(info, function(x) {
    (x$linear_constraints && !x$retained) ||
      any(vapply(x$margins, function(m) m$requested && !m$retained, logical(1)))
  }, logical(1))
  if (isTRUE(warn) && any(dropped)) {
    warning("Requested shape constraints were not retained in the fitted model for: ",
      paste(vapply(info[dropped], `[[`, character(1), "label"), collapse = ", "),
      ". The estimates for these terms do not enforce the requested shapes.",
      call. = FALSE)
  }
  if (length(info)) info else list()
}

# Select metadata by fitted index when possible, retaining duplicate labels in
# models with several linear predictors.
scasm_shape_subset <- function(info, object) {
  if (is.null(info) || !length(info)) return(list())
  if (".smooth_id" %in% names(object)) {
    selected <- info[intersect(names(info), as.character(unique(object$.smooth_id)))]
    return(if (length(selected)) selected else list())
  }
  labels <- unique(as.character(object$.smooth))
  Filter(function(x) x$label %in% labels, info)
}

# Plotmath uses the device's mathematical symbol font for arrows. The text
# version is suitable for devices and downstream consumers without plotmath.
scasm_shape_caption <- function(info, plotmath = TRUE) {
  tokens <- list()
  add <- function(text) tokens[[length(tokens) + 1L]] <<- text
  if (isTRUE(info$tensor)) {
    add("Tensor product")
    if (isTRUE(info$linear_constraints)) {
      add(if (info$retained) " (linear constraints)" else
        " (linear constraints; requested, not retained)")
    }
    add("; ")
  }
  for (i in seq_along(info$margins)) {
    margin <- info$margins[[i]]
    if (i > 1L) add("; ")
    if (isTRUE(info$tensor)) add(paste0(paste(margin$variables, collapse = ", "), ": "))
    add(margin$basis)
    if (!margin$requested) next
    add(" (")
    codes <- margin$codes
    if (!length(codes)) add("linear constraints")
    for (j in seq_along(codes)) {
      if (j > 1L) add(", ")
      code <- codes[j]
      text <- switch(code, `m+` = "increasing", `m-` = "decreasing",
        `c+` = "convex", `c-` = "concave", `+` = "positive")
      if (isTRUE(plotmath) && code %in% c("m+", "m-")) {
        text <- call(if (code == "m+") "%up%" else "%down%", "", "")
      }
      add(text)
    }
    if (!margin$retained) add("; requested, not retained")
    add(")")
  }
  if (!isTRUE(info$tensor) && isTRUE(info$linear_constraints) &&
      any(vapply(info$margins, function(m) length(m$codes) > 0L, logical(1)))) {
    add(if (info$retained) "; general linear constraints retained" else
      "; general linear constraints requested, not retained")
  }
  if (!isTRUE(plotmath)) return(paste0(unlist(tokens), collapse = ""))
  as.expression(list(Reduce(function(x, y) call("*", x, y), tokens)))
}

scasm_caption_append <- function(caption, text, sep = "; ") {
  expression_part <- function(x) if (is.expression(x)) x[[1L]] else x
  if (is.expression(caption) || is.expression(text)) {
    return(as.expression(list(call("*", call("*", expression_part(caption), sep),
      expression_part(text)))))
  }
  paste(caption, text, sep = sep)
}

scasm_shape_plot_caption <- function(object, default_caption, plotmath = TRUE) {
  info <- scasm_shape_subset(attr(object, "shape_constraints"), object)
  if (!length(info)) return(default_caption)
  # Factor-by terms can share a caption; preserve only distinct descriptions.
  plain <- vapply(info, scasm_shape_caption, character(1), plotmath = FALSE)
  info <- info[!duplicated(plain)]
  captions <- lapply(info, scasm_shape_caption, plotmath = plotmath)
  caption <- Reduce(scasm_caption_append, captions)
  default_caption <- unique(default_caption)[1L]
  at <- regexpr("Basis: ", default_caption, fixed = TRUE)[1L]
  prefix <- if (at > 0L) substr(default_caption, 1L, at - 1L) else ""
  suffix <- ""
  if (at > 0L) {
    basis <- unique(object$.type)[1L]
    start <- at + nchar("Basis: ")
    if (identical(substr(default_caption, start, start + nchar(basis) - 1L), basis)) {
      suffix <- substr(default_caption, start + nchar(basis), nchar(default_caption))
    }
  }
  caption <- scasm_caption_append(paste0(prefix, "Basis: "), caption, sep = "")
  scasm_caption_append(caption, suffix, sep = "")
}
