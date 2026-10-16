# Reject obsolete names before argument matching can give them a new meaning.
sampling_check_names <- function(args, derivative = FALSE, gaussian = FALSE) {
  args <- as.list(args)[-1L]
  nm <- names(args)
  if (!length(args)) {
    return(invisible(NULL))
  }
  if (is.null(nm) || any(!nzchar(nm))) {
    stop(
      "Sampling controls must be named. Use n_draws = ... for the draw count ",
      "and data = ... for evaluation data.",
      call. = FALSE
    )
  }
  replacements <- c(
    n = if (derivative) {
      "n_grid (evaluation points); use n_draws for draws"
    } else {
      "n_draws"
    },
    n_sim = "n_draws",
    n_vals = "n_grid",
    freq = "frequentist",
    newdata = "data",
    ncores = "n_cores for MH chains or n_threads for Gaussian draws",
    object = "model (or data for add_* helpers)",
    term = "select"
  )
  if (gaussian) replacements <- c(replacements, n_cores = "n_threads for Gaussian sampling")
  old <- intersect(nm, names(replacements))
  if (length(old)) {
    stop(
      "Argument '",
      old[1L],
      "' has been removed. Use ",
      replacements[[old[1L]]],
      ". See vignette('sampling-migration').",
      call. = FALSE
    )
  }
  ignored <- intersect(nm, c("mu", "sigma", "level", "rng_per_smooth"))
  if (length(ignored)) {
    stop(
      "Argument '",
      ignored[1L],
      "' is no longer supported by sampling functions. ",
      switch(ignored[1L],
        level = "Use response_derivatives(level = ...) for interval summaries.",
        rng_per_smooth = "Smooths now transform the same joint coefficient draws.",
        "These arguments were ignored; use method = 'user' with a coefficient matrix in draws."),
      call. = FALSE
    )
  }
  invisible(NULL)
}

sampling_method_controls <- function(method) {
  switch(
    method,
    gaussian = c(
      "n_threads",
      "mvn_method",
      "frequentist",
      "unconditional",
      "parametrized"
    ),
    mh = c(
      "n_cores",
      "burnin",
      "thin",
      "t_df",
      "rw_scale",
      "mh_backend",
      "block_size",
      "dense_limit"
    ),
    user = "draws",
    character()
  )
}

sampling_check_options <- function(
  method,
  supplied,
  mh_backend = "mgcv",
  mvn_method = "mvnfast"
) {
  if (
    method == "gaussian" && mvn_method == "mgcv" && "n_threads" %in% supplied
  ) {
    stop(
      "Argument 'n_threads' requires mvn_method = 'mvnfast'.",
      call. = FALSE
    )
  }
  controls <- unique(c(
    sampling_method_controls("gaussian"),
    sampling_method_controls("mh"),
    "draws"
  ))
  bad <- setdiff(
    intersect(supplied, controls),
    sampling_method_controls(method)
  )
  if (length(bad)) {
    stop(
      "Argument '",
      bad[1L],
      "' is not used by method = '",
      method,
      "'.",
      if (bad[1L] == "n_cores" && method == "gaussian") {
        " Use n_threads for Gaussian sampling threads."
      } else {
        ""
      },
      call. = FALSE
    )
  }
  if (
    method == "mh" &&
      mh_backend != "gratia" &&
      any(c("block_size", "dense_limit") %in% supplied)
  ) {
    stop(
      "Arguments 'block_size' and 'dense_limit' require mh_backend = 'gratia'.",
      call. = FALSE
    )
  }
}

sampling_resolve <- function(
  n_draws,
  chains = NULL,
  draws = NULL,
  method = "gaussian"
) {
  if (identical(method, "user")) {
    if (!is.matrix(draws)) {
      stop("Supplied 'draws' is not a matrix of coefficients.", call. = FALSE)
    }
    ids <- attr(draws, "sampling")$draw_ids
    inferred <- if (is.null(ids)) {
      1L
    } else {
      required <- c(".draw", ".chain", ".iteration")
      if (
        !is.data.frame(ids) ||
          !all(required %in% names(ids)) ||
          nrow(ids) != nrow(draws) ||
          any(vapply(
            ids[required],
            function(x) {
              !is.numeric(x) || any(!is.finite(x) | x < 1 | x != floor(x))
            },
            logical(1L)
          )) ||
          anyDuplicated(ids$.draw) ||
          anyDuplicated(ids[c(".chain", ".iteration")])
      ) {
        stop(
          "Invalid chain mapping in 'draws'; every coefficient row needs a unique draw and chain iteration.",
          call. = FALSE
        )
      }
      iterations <- sort(unique(ids$.iteration))
      if (
        length(iterations) * length(unique(ids$.chain)) != nrow(ids) ||
          (length(iterations) > 1L && any(diff(iterations) != 1L))
      ) {
        stop(
          "Supplied draws require balanced chains with consecutive iterations.",
          call. = FALSE
        )
      }
      length(unique(ids$.chain))
    }
    if (
      !is.null(n_draws) && sampling_count(n_draws, "n_draws") != nrow(draws)
    ) {
      stop(
        "'n_draws' must agree with the number of rows in 'draws'.",
        call. = FALSE
      )
    }
    if (!is.null(chains) && sampling_count(chains, "chains") != inferred) {
      stop(
        "'chains' must agree with the chain mapping in 'draws'.",
        call. = FALSE
      )
    }
    n_draws <- nrow(draws)
    chains <- inferred
  } else if (is.null(n_draws)) {
    stop(
      "Supply 'n_draws': the total retained draws across all chains. ",
      "For example, n_draws = 1000, chains = 4 retains 250 per chain.",
      call. = FALSE
    )
  }
  chains <- chains %||% 1L
  sampling_layout(n_draws, chains)
  list(
    n_draws = sampling_count(n_draws, "n_draws"),
    chains = sampling_count(chains, "chains")
  )
}

# Internal callers forward only controls that the selected method can use.
# Public callers receive errors for explicitly supplied inactive controls.
sampling_call <- function(fun, args) {
  default <- if (identical(fun, mh_draws)) {
    "mh"
  } else if (identical(fun, user_draws)) {
    "user"
  } else {
    "gaussian"
  }
  method <- match.arg(
    args$method %||% default,
    c("gaussian", "mh", "inla", "user")
  )
  controls <- unique(c(
    sampling_method_controls("gaussian"),
    sampling_method_controls("mh"),
    "draws"
  ))
  args <- args[
    !names(args) %in% setdiff(controls, sampling_method_controls(method))
  ]
  if (method == "mh" && (args$mh_backend %||% "mgcv")[[1L]] != "gratia") {
    args$block_size <- args$dense_limit <- NULL
  }
  if (method == "gaussian" && identical(args$mvn_method, "mgcv")) {
    args$n_threads <- NULL
  }
  if (!is.null(args$parametrized) && !inherits(args$model, "scam")) {
    args$parametrized <- NULL
  }
  do.call(fun, args)
}

# Response draws at fitted parameters have no coefficient chains to diagnose.
sampling_check_prediction <- function(args) {
  nm <- names(as.list(args)[-1L])
  bad <- intersect(
    nm,
    c(
      "chains",
      "diagnostics",
      "method",
      "draws",
      sampling_method_controls("gaussian"),
      sampling_method_controls("mh")
    )
  )
  if (length(bad)) {
    stop(
      "Argument '",
      bad[1L],
      "' is not supported by predicted_samples(), which samples responses at fitted parameters. ",
      "Use posterior_samples() to include coefficient uncertainty.",
      call. = FALSE
    )
  }
}

sampling_check_extra <- function(args, prediction = FALSE) {
  args <- as.list(args)[-1L]
  allowed <- if (prediction) {
    c(
      "terms",
      "exclude",
      "na.action",
      "block.size",
      "newdata.guaranteed",
      "discrete",
      "cluster",
      "n.threads",
      "envir"
    )
  } else {
    character()
  }
  bad <- setdiff(names(args), allowed)
  if (length(bad)) {
    stop(
      "Argument '",
      bad[1L],
      "' is not supported here. Use the full argument name and check the model's sampling capabilities.",
      call. = FALSE
    )
  }
}
