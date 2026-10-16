# Captions describe fitted basis constraints independently of smooth_type().
# This information is for display; it does not enable shape interval methods.
scam_smooth_info <- function(model, smooths = model$smooth,
                             ids = seq_along(smooths)) {
  if (!inherits(model, "scam")) return(list())
  uni <- list(mpi = "m+", mpd = "m-", cx = "c+", cv = "c-",
    micx = c("m+", "c+"), micv = c("m+", "c-"),
    mdcx = c("m-", "c+"), mdcv = c("m-", "c-"),
    miso = c("m+", "zero left"), mifo = c("m+", "zero right"),
    po = "+", ipo = c("m+", "+"), dpo = c("m-", "+"),
    cpopspline = "+", lmpi = "m+", lipl = "m+")
  tensor <- list(tedmi = c("m+", "m+"), tedmd = c("m-", "m-"),
    temicx = c("m+", "c+"), temicv = c("m+", "c-"),
    tedecx = c("m-", "c+"), tedecv = c("m-", "c-"),
    tecxcx = c("c+", "c+"), tecvcv = c("c-", "c-"),
    tecxcv = c("c+", "c-"), tesmi1 = c("m+", "free"),
    tesmi2 = c("free", "m+"), tesmd1 = c("m-", "free"),
    tesmd2 = c("free", "m-"), tescx = c("free", "c+"),
    tescv = c("free", "c-"), tismi = c("m+", "free"),
    tismd = c("m-", "free"))
  info <- lapply(seq_along(smooths), function(i) {
    sm <- smooths[[i]]
    cls <- intersect(class(sm), scam_smooth_classes())
    if (!length(cls)) return(NULL)
    type <- sub("[.]smooth$", "", cls[1L])
    type <- sub("By$", "", type)
    is_tensor <- type %in% names(tensor)
    list(smooth_id = ids[i], label = sm$label,
      variables = sm$term, tensor = is_tensor,
      interaction = type %in% c("tismi", "tismd"),
      basis = if (type == "cpopspline") "Cyclic P spline" else "P spline",
      codes = if (is_tensor) tensor[[type]] else uni[[type]],
      local = type %in% c("lmpi", "lipl"), flat = type == "lipl",
      change_point = if (type %in% c("lmpi", "lipl")) sm$xt$xc else NULL,
      cyclic = identical(sm[["margin.bs"]], "cc"))
  })
  names(info) <- as.character(ids)
  Filter(Negate(is.null), info)
}

scam_shape_caption <- function(info, plotmath = TRUE) {
  tokens <- list()
  add <- function(text) tokens[[length(tokens) + 1L]] <<- text
  constraint <- function(code) {
    if (isTRUE(plotmath) && code %in% c("m+", "m-")) {
      return(call(if (code == "m+") "%up%" else "%down%", "", ""))
    }
    switch(code, `m+` = "increasing", `m-` = "decreasing",
      `c+` = "convex", `c-` = "concave", `+` = "positive", code)
  }
  if (info$tensor) {
    add(if (info$interaction) "Tensor product int." else "Tensor product")
    for (i in seq_along(info$variables)) {
      add(paste0("; ", info$variables[i], ": "))
      add(constraint(info$codes[i]))
      if (info$cyclic && info$codes[i] == "free") add(" (cyclic)")
    }
  } else {
    add(paste0(info$basis, " ("))
    for (i in seq_along(info$codes)) {
      if (i > 1L) add(", ")
      add(constraint(info$codes[i]))
    }
    if (info$local) {
      add(" for ")
      point <- format(info$change_point, digits = 7L, trim = TRUE)
      if (isTRUE(plotmath)) {
        add(call("<=", info$variables[1L], point))
      } else add(paste0(info$variables[1L], " <= ", point))
      if (info$flat) add("; then flat")
    }
    add(")")
  }
  if (!isTRUE(plotmath)) return(paste0(unlist(tokens), collapse = ""))
  as.expression(list(Reduce(function(x, y) call("*", x, y), tokens)))
}

scam_shape_plot_caption <- function(object, default_caption, plotmath = TRUE) {
  info <- scasm_shape_subset(attr(object, "scam_smooth_info"), object)
  if (!length(info)) return(default_caption)
  plain <- vapply(info, scam_shape_caption, character(1), plotmath = FALSE)
  info <- info[!duplicated(plain)]
  caption <- Reduce(scasm_caption_append, lapply(info, scam_shape_caption,
    plotmath = plotmath))
  replace_smooth_basis_caption(object, default_caption, caption)
}
