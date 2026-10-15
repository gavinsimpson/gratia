# Validate counts before starting workers or evaluating a model matrix.
sampling_count <- function(x, name, zero = FALSE) {
  if (length(x) != 1L || !is.numeric(x) || !is.finite(x) ||
      x != floor(x) || x < (if (zero) 0 else 1) || x > .Machine$integer.max) {
    stop("'", name, "' must be a ", if (zero) "non-negative" else "positive",
      " integer.", call. = FALSE)
  }
  as.integer(x)
}

sampling_layout <- function(n, chains) {
  n <- sampling_count(n, "n_draws")
  chains <- sampling_count(chains, "chains")
  if (n %% chains != 0L) stop("'n_draws' must be divisible by 'chains'; ",
    "it is the total number of retained draws.", call. = FALSE)
  data.frame(.draw = seq_len(n), .chain = rep(seq_len(chains), each = n / chains),
    .iteration = rep(seq_len(n / chains), chains))
}

# The caller owns the RNG scope. Changing the RNG kind here cannot escape it.
sampling_streams <- function(chains) {
  RNGkind("L'Ecuyer-CMRG")
  streams <- vector("list", chains)
  streams[[1L]] <- get(".Random.seed", .GlobalEnv)
  if (chains > 1L) for (i in 2:chains) {
    streams[[i]] <- parallel::nextRNGStream(streams[[i - 1L]])
  }
  streams
}

sampling_finish <- function(betas, ids, info = NULL, diagnostics = FALSE,
                            coefficient_names = NULL) {
  if (!isTRUE(diagnostics) && length(unique(ids$.chain)) == 1L) {
    return(betas)
  }
  if (!is.null(coefficient_names)) colnames(betas) <- coefficient_names
  meta <- list(draw_ids = ids, chains = info, coefficients = NULL)
  attr(betas, "sampling") <- meta
  if (isTRUE(diagnostics)) {
    sampling_require("posterior")
    named <- betas
    colnames(named) <- coefficient_names %||% colnames(betas) %||%
      paste0("b", seq_len(ncol(betas)))
    meta$coefficients <- posterior::summarise_draws(
      as_mcmc_draws(named), "mean", "sd", "rhat", "ess_bulk", "ess_tail",
      "mcse_mean", "mcse_sd")
    # Retain coefficient draws for native plots after response or smooth sampling.
    # Strip metadata so the saved matrix cannot contain another copy of itself.
    attributes(named) <- list(dim = dim(named), dimnames = dimnames(named))
    meta$coefficient_draws <- named
    attr(betas, "sampling") <- meta
  }
  betas
}

attach_sampling <- function(x, source, draw_index = FALSE) {
  meta <- if (is.list(source) && "draw_ids" %in% names(source)) source else
    attr(source, "sampling")
  if (is.null(meta)) return(x)
  if (draw_index && is.data.frame(x) && ".draw" %in% names(x)) {
    i <- x$.draw
    if (anyNA(i) || any(i < 1 | i > nrow(meta$draw_ids) | i != floor(i))) stop(
      "Draw positions do not match the coefficient matrix.", call. = FALSE)
    x$.draw <- meta$draw_ids$.draw[i]
  }
  if (is.data.frame(x) && ".draw" %in% names(x) &&
      length(unique(meta$draw_ids$.chain)) > 1L) {
    i <- match(x$.draw, meta$draw_ids$.draw)
    if (anyNA(i)) stop("Draw identifiers no longer match the sampled chains.", call. = FALSE)
    x$.chain <- meta$draw_ids$.chain[i]
    x$.iteration <- meta$draw_ids$.iteration[i]
  }
  attr(x, "sampling") <- meta
  x
}

sampling_require <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) stop(
    "Package '", package, "' is required for this sampling conversion or diagnostic.",
    call. = FALSE)
}

#' Convert posterior samples for MCMC diagnostics
#'
#' Coefficient matrices retain their chain mapping in an attribute. Sampled
#' quantities use their `.chain`, `.iteration`, and `.draw` columns. A plain
#' matrix without a mapping is treated as one chain; chain boundaries are never
#' inferred from its number of rows. Filtering must leave the same consecutive
#' iterations in every chain. Missing observations remain missing variables.
#'
#' @param x A coefficient matrix from [post_draws()] or a table from
#'   [fitted_samples()], [posterior_samples()], [smooth_samples()], or
#'   [derivative_samples()].
#' @param format One of `"draws_array"`, `"draws_df"`, `"mcmc.list"`, or
#'   `"array"`. The first two require posterior; `"mcmc.list"` requires coda.
#' @param value Column containing sampled values. By default the appropriate
#'   column is selected from `.fitted`, `.response`, `.value`, or `.derivative`.
#' @return An iteration by chain by variable array, a posterior draws object,
#'   or a coda list of chains. Variable names identify sampled quantities and
#'   their observation rows, parameters, or smooths.
#' @export
as_mcmc_draws <- function(x,
    format = c("draws_array", "draws_df", "mcmc.list", "array"), value = NULL) {
  format <- match.arg(format)
  if (is.matrix(x)) {
    ids <- attr(x, "sampling")$draw_ids %||% sampling_layout(nrow(x), 1L)
    values <- x
    if (nrow(ids) != nrow(values)) stop("The coefficient matrix has lost rows ",
      "from its chain mapping.", call. = FALSE)
    variables <- colnames(x) %||% paste0("b", seq_len(ncol(x)))
  } else if (is.data.frame(x)) {
    if ("data" %in% names(x) && is.list(x$data)) x <- tidyr::unnest(x, "data")
    if (!".draw" %in% names(x)) stop("Samples must contain '.draw'.", call. = FALSE)
    if (is.null(value)) {
      value <- intersect(c(".fitted", ".response", ".value", ".derivative"), names(x))
      if (length(value) != 1L) stop("Specify one sampled 'value' column.", call. = FALSE)
    }
    if (length(value) != 1L || !value %in% names(x) || !is.numeric(x[[value]])) {
      stop("'value' must name one numeric sampled column.", call. = FALSE)
    }
    if (xor(".chain" %in% names(x), ".iteration" %in% names(x))) stop(
      "Both '.chain' and '.iteration' are required.", call. = FALSE)
    ids <- if (".chain" %in% names(x)) {
      unique(as.data.frame(x[c(".draw", ".chain", ".iteration")]))
    } else {
      meta <- attr(x, "sampling")$draw_ids
      if (is.null(meta)) {
        draws <- sort(unique(x$.draw))
        data.frame(.draw = draws, .chain = 1L, .iteration = seq_along(draws))
      } else meta[meta$.draw %in% x$.draw, , drop = FALSE]
    }
    keys <- intersect(c(".smooth", ".term", ".focal", ".parameter", ".category", ".y", ".row"), names(x))
    if (!length(keys)) stop("Sampled quantities need observation or parameter identifiers.", call. = FALSE)
    labels <- do.call(paste, c(lapply(keys, function(k) paste0(k, "=", x[[k]])), sep = ","))
    labels <- paste0(sub("^\\.", "", value), "[", labels, "]")
    variables <- unique(labels)
    ii <- match(x$.draw, ids$.draw)
    jj <- match(labels, variables)
    key <- paste(ii, jj, sep = ":")
    if (anyDuplicated(key)) stop("More than one value for a draw and sampled quantity.", call. = FALSE)
    values <- matrix(NA_real_, nrow(ids), length(variables))
    values[cbind(ii, jj)] <- x[[value]]
    if (length(key) != length(values)) stop("Every sampled quantity must have ",
      "the same draws and chains.", call. = FALSE)
  } else stop("Supply a coefficient matrix or a table of sampled quantities.", call. = FALSE)
  if (nrow(ids) == 0L) stop("No retained draws to convert.", call. = FALSE)
  if (any(vapply(ids, function(v) !is.numeric(v) ||
      any(!is.finite(v) | v < 1 | v != floor(v)), logical(1L)))) stop(
    "Chain, iteration, and draw identifiers must be positive integers.", call. = FALSE)
  if (anyNA(ids) || anyDuplicated(ids$.draw) ||
      anyDuplicated(ids[c(".chain", ".iteration")]) ||
      any(ids$.chain < 1 | ids$.chain != floor(ids$.chain)) ||
      any(ids$.iteration < 1 | ids$.iteration != floor(ids$.iteration))) {
    stop("Invalid chain, iteration, or draw identifiers.", call. = FALSE)
  }
  chain_ids <- sort(unique(ids$.chain))
  iterations <- sort(unique(ids$.iteration))
  if (length(iterations) > 1L && any(diff(iterations) != 1L) ||
      nrow(ids) != length(chain_ids) * length(iterations)) stop(
    "Conversion requires balanced chains with consecutive iterations.", call. = FALSE)
  out <- array(NA_real_, c(length(iterations), length(chain_ids), ncol(values)),
    dimnames = list(iteration = as.character(iterations), chain = as.character(chain_ids),
      variable = variables))
  for (j in seq_along(chain_ids)) {
    take <- which(ids$.chain == chain_ids[j])
    out[match(ids$.iteration[take], iterations), j, ] <- values[take, , drop = FALSE]
  }
  if (format == "array") return(out)
  if (format == "mcmc.list") {
    sampling_require("coda")
    return(coda::mcmc.list(lapply(seq_along(chain_ids), function(j) {
      coda::mcmc(matrix(out[, j, ], nrow = length(iterations),
        dimnames = list(NULL, variables)), start = min(iterations))
    })))
  }
  sampling_require("posterior")
  out <- posterior::as_draws_array(out)
  if (format == "draws_df") posterior::as_draws_df(out) else out
}

#' Extract coefficient-sampler diagnostics
#'
#' Diagnostics attached to fitted values or response samples describe the
#' coefficient chains before transformation or response simulation. Response
#' noise can conceal poor mixing of coefficient chains. Use [as_mcmc_draws()]
#' to examine particular derived quantities as well.
#' @param x Output from a gratia sampling function; for the print method, a
#'   `sampling_diagnostics` object.
#'
#' @details
#' Coefficient draws are retained when sampling with `diagnostics = TRUE`,
#' allowing [draw.sampling_diagnostics()] to plot the original coefficient
#' chains even when `x` contains response samples, fitted values, smooths or
#' derivatives. These plots do not describe the transformed quantities.
#' Numerical summaries require posterior, but plotting does not.
#'
#' @return A `sampling_diagnostics` list with `chains` (acceptance rates and
#'   elapsed seconds), `coefficients` (posterior summaries, or `NULL` if not
#'   requested), and `draws` (a coefficient matrix with chain identities, or
#'   `NULL` if unavailable). Coefficient matrices provide their own draws;
#'   other sampling outputs need `diagnostics = TRUE` to retain them.
#' @seealso [draw.sampling_diagnostics()], [as_mcmc_draws()]
#' @export
sampling_diagnostics <- function(x) {
  meta <- attr(x, "sampling")
  if (is.null(meta)) stop("No sampling diagnostics are attached; request ",
    "'diagnostics = TRUE' when sampling.", call. = FALSE)
  draws <- if (is.matrix(x)) x else meta$coefficient_draws
  if (!is.null(draws)) {
    if (is.null(colnames(draws))) {
      # Native single-chain matrices can be unnamed even when diagnostics
      # retain model coefficient names. Recover labels only for a full match.
      labels <- colnames(meta$coefficient_draws) %||% meta$coefficients$variable
      if (length(labels) == ncol(draws)) colnames(draws) <- labels
    }
    # Retain only the layout needed for conversion, not nested diagnostics.
    attributes(draws) <- list(dim = dim(draws), dimnames = dimnames(draws),
      sampling = list(draw_ids = meta$draw_ids))
  }
  structure(list(chains = meta$chains, coefficients = meta$coefficients,
    draws = draws), class = c("sampling_diagnostics", "list"))
}

#' @export
#' @rdname sampling_diagnostics
#' @param ... Further arguments passed to the print methods for the summaries.
print.sampling_diagnostics <- function(x, ...) {
  cat("Coefficient sampling diagnostics\n")
  if (!is.null(x$chains)) print(x$chains, ...)
  if (!is.null(x$coefficients)) print(x$coefficients, ...)
  if (!is.null(x$draws)) {
    ids <- attr(x$draws, "sampling")$draw_ids
    cat(nrow(x$draws), "retained draws in", length(unique(ids$.chain)),
      "chain(s);", ncol(x$draws), "coefficient(s).\n")
  }
  invisible(x)
}

# Reuse the active mirai pool without changing it. A private compute profile
# owns only workers created by this call. Batches cap concurrency even when
# the caller has configured a larger pool.
sampling_parallel <- function(tasks, args, workers) {
  sampling_require("mirai")
  profile <- NULL
  if (!isTRUE(mirai::daemons_set())) {
    profile <- paste0("gratia-sampling-", Sys.getpid(), "-", basename(tempfile()))
    mirai::daemons(workers, .compute = profile)
    on.exit(mirai::daemons(0L, .compute = profile), add = TRUE)
  }
  pending <- list()
  on.exit(lapply(pending, function(m) {
    if (mirai::unresolved(m)) mirai::stop_mirai(m)
  }), add = TRUE)
  result <- vector("list", length(tasks))
  batches <- split(seq_along(tasks), ceiling(seq_along(tasks) / workers))
  for (batch in batches) {
    pending <- lapply(batch, function(i) {
      task <- tasks[[i]]
      libs <- .libPaths()
      mirai::mirai({
        .libPaths(libs)
        do.call(get("sampling_mh_worker", asNamespace("gratia")),
          c(list(task = task), args))
      }, task = task, args = args, libs = libs, .compute = profile)
    })
    for (j in seq_along(pending)) {
      value <- pending[[j]][]
      if (mirai::is_error_value(value)) stop("Sampling chain ", batch[j],
        " failed: ", as.character(value), call. = FALSE)
      result[[batch[j]]] <- value
    }
  }
  result
}
