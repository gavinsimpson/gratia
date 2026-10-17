skip_if_not_installed("mirai")
skip_if_not_installed("carrier")

parallel_ale_worker_counts <- function() {
  if (full_test_run()) c(1L, 2L, 4L) else 1L
}

parallel_ale_fixture <- local({
  fixture <- NULL
  function() {
    if (!is.null(fixture)) return(fixture)
    fixture <<- withr::with_seed(8401, {
      d <- data.frame(x = runif(127, .2, 1.8), z = runif(127, -.5, .5),
        g = factor(rep(letters[1:3], length.out = 127)))
      d$off <- .1 * log(d$x)
      d$y <- 2 + sin(2 * d$x) + .5 * d$z + rnorm(127, sd = .3)
      smooth <- mgcv::gam(y ~ g + s(x, by = g, k = 5) +
        te(x, z, k = c(4, 4)) + offset(log(x)), data = d, method = "REML")
      poisson_data <- d
      poisson_data$y <- rpois(nrow(d), exp(.2 + sin(d$x) + d$z + d$off))
      count <- mgcv::bam(y ~ g + s(x, by = g, k = 5) + s(z, k = 5) +
        offset(log(x)), data = poisson_data, family = poisson(),
        method = "fREML", discrete = TRUE, nthreads = 1)
      distributional <- mgcv::gam(list(y ~ s(x, k = 5) + offset(off),
        ~ s(z, k = 5)), data = d, family = mgcv::gaulss(), method = "REML")
      qualified <- mgcv::gam(y ~ s(stats::plogis(x), k = 5) + z,
        data = d, method = "REML")
      omitted_data <- d
      omitted_data$y[7L] <- NA
      omitted <- mgcv::gam(y ~ s(log(x), k = 5) + I(x * z) +
        offset(log(x)), data = omitted_data, subset = seq_len(nrow(d)) > 4L,
        na.action = na.exclude, method = "REML")
      recovery_env <- list2env(list(omitted_data = omitted_data, d = d),
        parent = globalenv())
      list(data = d, smooth = smooth, count = count,
        poisson_data = poisson_data, distributional = distributional,
        qualified = qualified, omitted = omitted, recovery_env = recovery_env)
    })
    fixture
  }
})

# Initialise counters before ordinary tasks so mirai's cleanup retains them.
parallel_ale_workers <- function(n) {
  mirai::daemons(0)
  gratia_test_workers(n)
  setup <- mirai::everywhere({
    .GlobalEnv$.gratia_ale_block_calls <- list()
    trace("ale_block_summary", where = asNamespace("gratia"), print = FALSE,
      tracer = quote({
        .GlobalEnv$.gratia_ale_block_calls <- append(
          .GlobalEnv$.gratia_ale_block_calls,
          list(list(pid = Sys.getpid(), rows = nrow(reference))))
      }))
    Sys.getpid()
  })
  unlist(mirai::collect_mirai(setup, options = ".stop"), use.names = FALSE)
}

parallel_ale_worker_calls <- function(reset = FALSE) {
  query <- mirai::everywhere({
    out <- list(pid = Sys.getpid(), calls = .GlobalEnv$.gratia_ale_block_calls)
    if (reset) .GlobalEnv$.gratia_ale_block_calls <- list()
    out
  }, reset = reset)
  mirai::collect_mirai(query, options = ".stop")
}

parallel_ale_called_pids <- function(calls) {
  unlist(lapply(calls, function(worker) {
    lapply(worker$calls, `[[`, "pid")
  }), use.names = FALSE)
}

parallel_ale_called_rows <- function(calls) {
  unlist(lapply(calls, function(worker) {
    lapply(worker$calls, `[[`, "rows")
  }), use.names = FALSE)
}

test_that("ALE workers preserve block order, row counts and all returned attributes", {
  withr::local_seed(8402)
  mirai::daemons(0)
  withr::defer(mirai::daemons(0))
  f <- parallel_ale_fixture()
  d <- f$data
  cases <- list(
    factor_link = list(object = f$smooth, focal = "x", data = d),
    factor_response = list(object = f$smooth, focal = "x", data = d,
      scale = "response", center = FALSE),
    excluded = list(object = f$smooth, focal = "x", data = d,
      exclude = "te(x,z)"),
    bam_link = list(object = f$count, focal = "x", data = f$poisson_data,
      n.threads = 1L),
    bam_response = list(object = f$count, focal = "x", data = f$poisson_data,
      scale = "response", n.threads = 1L),
    distributional_location = list(object = f$distributional, focal = "x",
      data = d, parameter = "location", scale = "response"),
    distributional_scale = list(object = f$distributional, focal = "z",
      data = d, parameter = "scale", scale = "response"),
    qualified_transform = list(object = f$qualified, focal = "x", data = d),
    omitted_training = list(object = f$omitted, focal = "x", envir = f$recovery_env),
    missing_response = list(object = f$omitted, focal = "x",
      data = transform(d, y = NA_real_)),
    explicit_breaks = list(object = f$smooth, focal = "x", data = d,
      breaks = c(min(d$x), .6, 1, max(d$x))),
    repeated_reference_rows = list(object = f$smooth, focal = "x",
      data = d[c(127, 1, 127, 1:100), ])
  )
  cases <- lapply(cases, function(args) c(args, list(n_bins = 8L, block_size = 17L)))
  tied <- d
  tied$x <- rep(c(.2, .7, 1.8), length.out = nrow(tied))
  for (size in c(1L, 19L)) {
    cases[[paste0("tied_bins_", size)]] <- list(object = f$smooth,
      focal = "x", data = tied, n_bins = 40, block_size = size)
  }
  expected <- lapply(cases, function(args) do.call(ale, args))
  expect_identical(sum(expected$factor_link$.n), nrow(d))
  expect_identical(attr(expected$omitted_training, "ale_info")$n_reference, 122L)
  for (n in parallel_ale_worker_counts()) {
    pids <- parallel_ale_workers(n)
    expect_length(unique(pids), n)
    expect_false(Sys.getpid() %in% pids)
    before <- .Random.seed
    used <- integer()
    for (name in names(cases)) {
      actual <- do.call(ale, cases[[name]])
      expect_identical(actual, expected[[name]], info = name)
      expect_identical(.Random.seed, before)
      expect_true(mirai::daemons_set())
      calls <- parallel_ale_worker_calls(reset = TRUE)
      called <- parallel_ale_called_pids(calls)
      if (name == "distributional_scale") {
        expect_length(called, 0L)
      } else {
        expect_true(length(called) > 0L, info = name)
        expect_true(all(called %in% pids))
        count <- attr(actual, "ale_info")$n_reference
        size <- cases[[name]]$block_size
        starts <- seq.int(1L, count, by = size)
        lengths <- as.integer(pmin(size, count - starts + 1L))
        expect_identical(sort(parallel_ale_called_rows(calls)), sort(lengths), info = name)
      }
      used <- c(used, called)
    }
    expect_setequal(used, pids)
    invisible(ale(f$smooth, "x", data = d, block_size = nrow(d)))
    expect_length(parallel_ale_called_pids(parallel_ale_worker_calls()), 0L)
    expect_true(mirai::daemons_set())
    # A failed worker calculation must surface; retrying in the caller could
    # conceal a dependency problem or change a custom method's behaviour.
    setup <- mirai::everywhere({
      trace("ale_block_summary", where = asNamespace("gratia"), print = FALSE,
        tracer = quote(stop("deliberate ALE worker failure")))
      invisible(NULL)
    })
    invisible(mirai::collect_mirai(setup, options = ".stop"))
    expect_error(ale(f$smooth, "x", data = d, block_size = 17L),
      "deliberate ALE worker failure")
    expect_true(mirai::daemons_set())
    after <- mirai::everywhere(Sys.getpid())
    expect_setequal(unlist(mirai::collect_mirai(after, options = ".stop")), pids)
  }
})

test_that("ALE workers retain one caller draw matrix and the caller RNG state", {
  mirai::daemons(0)
  withr::defer(mirai::daemons(0))
  withr::local_preserve_seed()
  old_kind <- RNGkind()
  withr::defer(do.call(RNGkind, as.list(old_kind)))
  f <- parallel_ale_fixture()
  specs <- list()
  for (kind in c("Mersenne-Twister", "L'Ecuyer-CMRG")) {
    for (seed in list(53L, NULL)) {
      RNGkind(kind)
      set.seed(8403)
      before <- .Random.seed
      args <- list(object = f$smooth, focal = "x", data = f$data,
        block_size = 17L, n_bins = 8L, interval = "simultaneous", n_sim = 120L,
        n_cores = 1L, seed = seed)
      expected <- do.call(ale, args)
      specs[[length(specs) + 1L]] <- list(kind = RNGkind(), args = args,
        expected = expected, before = before, after = .Random.seed)
    }
  }
  rlang::local_bindings(.env = .GlobalEnv,
    .gratia_ale_draw_records = list(), .gratia_ale_interval_inputs = list())
  ns <- asNamespace("gratia")
  suppressMessages(trace("simultaneous_coefficient_deviations", where = ns,
    print = FALSE, exit = quote({
      .GlobalEnv$.gratia_ale_draw_records <- append(
        .GlobalEnv$.gratia_ale_draw_records,
        list(list(pid = Sys.getpid(), draws = returnValue())))
    })))
  withr::defer(suppressMessages(untrace("simultaneous_coefficient_deviations",
    where = ns)))
  suppressMessages(trace("simultaneous_intervals", where = ns, print = FALSE,
    tracer = quote({
      .GlobalEnv$.gratia_ale_interval_inputs <- append(
        .GlobalEnv$.gratia_ale_interval_inputs,
        list(list(X = X, V = V, se = se, level = level)))
    })))
  withr::defer(suppressMessages(untrace("simultaneous_intervals", where = ns)))
  for (n in parallel_ale_worker_counts()) {
    pids <- parallel_ale_workers(n)
    setup <- mirai::everywhere({
      trace("simultaneous_coefficient_deviations", where = asNamespace("gratia"),
        print = FALSE, tracer = quote(stop("ALE coefficient draws reached a worker")))
      invisible(NULL)
    })
    invisible(mirai::collect_mirai(setup, options = ".stop"))
    for (spec in specs) {
      do.call(RNGkind, as.list(spec$kind))
      assign(".Random.seed", spec$before, envir = .GlobalEnv)
      .GlobalEnv$.gratia_ale_draw_records <- list()
      .GlobalEnv$.gratia_ale_interval_inputs <- list()
      actual <- do.call(ale, spec$args)
      expect_identical(actual, spec$expected)
      expect_identical(.Random.seed, spec$after)
      expect_identical(RNGkind(), spec$kind)
      if (!is.null(spec$args$seed)) expect_identical(.Random.seed, spec$before)
      records <- .GlobalEnv$.gratia_ale_draw_records
      inputs <- .GlobalEnv$.gratia_ale_interval_inputs
      expect_length(records, 1L)
      expect_length(inputs, 1L)
      expect_identical(records[[1L]]$pid, Sys.getpid())
      expect_identical(dim(records[[1L]]$draws), c(120L, length(coef(f$smooth))))
      # Recalculate the maximum over every returned boundary from this single
      # joint coefficient draw matrix, independently of the interval helper.
      positive <- which(inputs[[1L]]$se > 0)
      errors <- tcrossprod(inputs[[1L]]$X[positive, , drop = FALSE], records[[1L]]$draws)
      maxima <- apply(abs(sweep(errors, 1L, inputs[[1L]]$se[positive], "/")), 2L, max)
      critical <- as.numeric(stats::quantile(maxima, inputs[[1L]]$level, type = 8))
      expect_identical(unname(unique(actual$.crit)), critical)
    }
    expect_setequal(parallel_ale_called_pids(parallel_ale_worker_calls()), pids)
    .GlobalEnv$.gratia_ale_draw_records <- list()
    before <- .Random.seed
    invisible(ale(f$smooth, "x", data = f$data, block_size = 17L))
    expect_identical(.Random.seed, before)
    expect_length(.GlobalEnv$.gratia_ale_draw_records, 0L)
    if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
    invisible(ale(f$smooth, "x", data = f$data, block_size = 17L,
      interval = "simultaneous", n_sim = 80L, seed = 43L))
    expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
    set.seed(8404)
    linear <- mgcv::gam(y ~ x + z, data = f$data)
    before <- .Random.seed
    .GlobalEnv$.gratia_ale_draw_records <- list()
    zero <- ale(linear, "x", data = f$data, exclude = "x", block_size = 17L,
      interval = "simultaneous", n_sim = 80L)
    expect_true(all(zero$.ale == 0))
    expect_true(all(zero$.se == 0))
    expect_identical(.Random.seed, before)
    expect_length(.GlobalEnv$.gratia_ale_draw_records, 0L)
    expect_true(mirai::daemons_set())
  }
})

test_that("fresh ALE workers retain caller prediction methods and formula dependencies", {
  withr::local_seed(8405)
  mirai::daemons(0)
  withr::defer(mirai::daemons(0))
  f <- parallel_ale_fixture()
  d <- f$data
  rlang::local_bindings(.env = .GlobalEnv,
    gratia_ale_prediction_helper = identity,
    predict.gratia_ale_worker = function(object, ...) {
      gratia_ale_prediction_helper(NextMethod())
    },
    Predict.matrix.gratia_ale_worker = function(object, data) {
      gratia_ale_prediction_helper(NextMethod())
    },
    gratia_ale_base_alias = base::log1p,
    gratia_ale_stats_alias = stats::plogis,
    gratia_ale_constant = .3,
    gratia_ale_transform = function(x) x + .01 * x^2)
  custom <- f$smooth
  class(custom) <- c("gratia_ale_worker", class(custom))
  custom_smooth <- f$smooth
  class(custom_smooth$smooth[[1L]]) <- c("gratia_ale_worker",
    class(custom_smooth$smooth[[1L]]))
  namespace_method <- function(object, ...) mgcv::predict.gam(object, ...)
  environment(namespace_method) <- asNamespace("mgcv")
  rlang::local_bindings(.env = .GlobalEnv,
    predict.gratia_ale_namespace = namespace_method)
  namespace_model <- f$smooth
  class(namespace_model) <- c("gratia_ale_namespace", class(namespace_model))
  local_helper <- local({
    shifted <- function(x) x + .1
    list(model = mgcv::gam(y ~ s(shifted(x), k = 5) + z, data = d),
      envir = environment())
  })
  custom_transform <- f$smooth
  custom_transform$family$linkinv <- function(x) gratia_ale_transform(x)
  custom_transform$family$mu.eta <- function(x) 1 + .02 * x
  stochastic_transform <- f$smooth
  stochastic_transform$family$linkinv <- function(x) {
    x + stats::rnorm(length(x), sd = .001)
  }
  shadow <- function(x) base::log1p(x) + .02 * x^2
  environment(shadow) <- baseenv()
  rlang::local_bindings(.env = .GlobalEnv, log1p = shadow)
  formulas <- list(
    base_alias = y ~ s(gratia_ale_base_alias(x), k = 5) + z,
    stats_alias = y ~ s(gratia_ale_stats_alias(x), k = 5) + z,
    shadowed_base = y ~ s(log1p(x), k = 5) + z,
    global_constant = y ~ s(I(x + get("gratia_ale_constant")), k = 5) + z,
    qualified_lookup = y ~ s(I(x + base::get("gratia_ale_constant")), k = 5) + z,
    rnorm = y ~ s(I(x + rnorm(length(x), sd = .001)), k = 5) + z,
    qualified_rnorm = y ~ s(I(x + stats::rnorm(length(x), sd = .001)), k = 5) + z,
    jitter = y ~ s(jitter(x, amount = .001), k = 5) + z,
    sample = y ~ s(I(x + sample(c(-.001, .001), length(x), replace = TRUE)),
      k = 5) + z
  )
  cases <- list(custom_model = list(object = custom),
    custom_smooth = list(object = custom_smooth),
    namespace_method = list(object = namespace_model),
    local_helper = list(object = local_helper$model, envir = local_helper$envir),
    custom_transform = list(object = custom_transform, scale = "response"))
  for (name in names(formulas)) {
    cases[[name]] <- list(object = mgcv::gam(formulas[[name]], data = d))
  }
  cases$seeded_stochastic_formula <- list(object = cases$rnorm$object,
    interval = "simultaneous", n_sim = 80L, seed = 43L)
  cases$seeded_stochastic_transform <- list(object = stochastic_transform,
    scale = "response", interval = "simultaneous", n_sim = 80L, seed = 43L)
  cases <- lapply(cases, function(args) c(args,
    list(focal = "x", data = d, n_bins = 8L, block_size = 17L)))
  expected <- lapply(cases, function(args) {
    set.seed(8406)
    before <- .Random.seed
    value <- do.call(ale, args)
    list(value = value, state = .Random.seed, before = before)
  })
  callback <- f$smooth
  callback$family$predict <- function(...) stop("caller ALE family callback")
  callback_expected <- ale(callback, "x", data = d, block_size = 17L)
  for (n in parallel_ale_worker_counts()) {
    pids <- parallel_ale_workers(n)
    absent <- mirai::everywhere(c(
      exists("predict.gratia_ale_worker", .GlobalEnv, inherits = FALSE),
      exists("Predict.matrix.gratia_ale_worker", .GlobalEnv, inherits = FALSE),
      exists("gratia_ale_base_alias", .GlobalEnv, inherits = FALSE),
      exists("gratia_ale_constant", .GlobalEnv, inherits = FALSE)))
    expect_false(any(unlist(mirai::collect_mirai(absent, options = ".stop"))))
    for (name in names(cases)) {
      set.seed(8406)
      actual <- do.call(ale, cases[[name]])
      expect_identical(actual, expected[[name]]$value, info = name)
      expect_identical(.Random.seed, expected[[name]]$state, info = name)
      if (name %in% c("seeded_stochastic_formula", "seeded_stochastic_transform")) {
        # The explicit seed covers band simulation. Prediction still consumes
        # the caller's RNG, just as it does without workers.
        expect_false(identical(.Random.seed, expected[[name]]$before), info = name)
      }
      expect_identical(length(parallel_ale_called_pids(
        parallel_ale_worker_calls(reset = TRUE))), 0L, info = name)
      expect_true(mirai::daemons_set())
    }
    expect_true(ale_predict_in_caller(callback, d))
    expect_identical(ale(callback, "x", data = d, block_size = 17L), callback_expected)
    expect_length(parallel_ale_called_pids(parallel_ale_worker_calls()), 0L)
    # Invalid reference inputs are rejected before any prediction task runs.
    expect_error(ale(f$smooth, "x", data = transform(d, z = Inf), block_size = 17L),
      "non-finite")
    expect_error(ale(f$smooth, "x", data = transform(d, x = NA_real_), block_size = 17L),
      "non-finite")
    expect_error(ale(f$smooth, "x", data = d[FALSE, ], block_size = 17L),
      "at least one")
    grouped <- f$smooth
    grouped$family <- mgcv::gfam(list(gaussian(), poisson()))
    expect_error(ale(grouped, "x", data = d, block_size = 17L),
      "distribution parameters")
    expect_length(parallel_ale_called_pids(parallel_ale_worker_calls()), 0L)
    after <- mirai::everywhere(Sys.getpid())
    expect_setequal(unlist(mirai::collect_mirai(after, options = ".stop")), pids)
  }
})

test_that("ALE retains S3 transformations in the caller with fresh workers", {
  withr::local_seed(8410)
  mirai::daemons(0)
  withr::defer(mirai::daemons(0))
  d <- parallel_ale_fixture()$data
  d$z <- d$z + 1
  rlang::local_bindings(.env = .GlobalEnv,
    gratia_ale_math_helper = function(x) x * 2,
    Math.AsIs = function(x, ...) gratia_ale_math_helper(NextMethod()))
  nested <- mgcv::gam(y ~ s(x, k = 5) + x:log(I(z)), data = d)
  classed <- d
  classed$z <- I(classed$z)
  direct <- mgcv::gam(y ~ s(x, k = 5) + x:log(z), data = classed)
  cases <- list(nested = list(object = nested, data = d),
    classed = list(object = direct, data = classed))
  # A stochastic method must consume the same caller draws during prediction,
  # even when interval simulation has its own explicit seed.
  .GlobalEnv$Math.AsIs <- function(x, ...) {
    gratia_ale_math_helper(NextMethod()) + rnorm(length(x), sd = .001)
  }
  for (name in names(cases)) {
    cases[[name]] <- c(cases[[name]], list(focal = "x", block_size = 17L,
      n_bins = 8L, interval = "simultaneous", n_sim = 80L, seed = 43L))
  }
  expected <- lapply(cases, function(args) {
    set.seed(8411)
    before <- .Random.seed
    value <- do.call(ale, args)
    expect_false(identical(.Random.seed, before))
    list(value = value, seed = .Random.seed)
  })
  check_route <- function(route) {
    if (route == "registered") {
      method <- .GlobalEnv$Math.AsIs
      rlang::local_bindings(.env = .GlobalEnv, Math.AsIs = NULL)
      rm("Math.AsIs", envir = .GlobalEnv)
      table <- get(".__S3MethodsTable__.", envir = baseenv())
      rlang::local_bindings(.env = table, Math.AsIs = method)
    }
    for (name in names(cases)) {
      set.seed(8411)
      expect_identical(do.call(ale, cases[[name]]), expected[[name]]$value,
        info = paste(route, name))
      expect_identical(.Random.seed, expected[[name]]$seed)
      expect_length(parallel_ale_called_pids(parallel_ale_worker_calls(reset = TRUE)), 0L)
    }
  }
  for (n in parallel_ale_worker_counts()) {
    parallel_ale_workers(n)
    absent <- mirai::everywhere(c(
      exists("Math.AsIs", .GlobalEnv, inherits = FALSE),
      exists("gratia_ale_math_helper", .GlobalEnv, inherits = FALSE)))
    expect_false(any(unlist(mirai::collect_mirai(absent, options = ".stop"))))
    check_route("visible")
    check_route("registered")
  }
})

test_that("ALE does not force active or lazy prediction method bindings", {
  withr::local_seed(8412)
  mirai::daemons(0)
  withr::defer(mirai::daemons(0))
  f <- parallel_ale_fixture()
  cases <- list(confidence = list(),
    seeded = list(interval = "simultaneous", n_sim = 80L, seed = 43L),
    unseeded = list(interval = "simultaneous", n_sim = 80L))
  cases <- lapply(cases, function(args) c(list(object = f$smooth, focal = "x",
    data = f$data, block_size = 17L, n_bins = 8L), args))
  expected <- lapply(cases, function(args) {
    set.seed(8413)
    value <- do.call(ale, args)
    list(value = value, seed = .Random.seed)
  })
  check_binding <- function(name, method, kind) {
    rlang::local_bindings(.env = .GlobalEnv, !!!setNames(list(NULL), name))
    rm(list = name, envir = .GlobalEnv)
    counter <- new.env(parent = emptyenv())
    counter$reads <- 0L
    if (kind == "active") {
      makeActiveBinding(name, function(value) {
        counter$reads <- counter$reads + 1L
        runif(1)
        method
      }, .GlobalEnv)
    } else {
      delayedAssign(name, {
        counter$reads <- counter$reads + 1L
        runif(1)
        method
      }, assign.env = .GlobalEnv)
    }
    for (case in names(cases)) {
      set.seed(8413)
      expect_identical(do.call(ale, cases[[case]]), expected[[case]]$value,
        info = paste(name, kind, case))
      expect_identical(.Random.seed, expected[[case]]$seed)
      expect_identical(counter$reads, 0L)
      expect_length(parallel_ale_called_pids(parallel_ale_worker_calls(reset = TRUE)), 0L)
    }
    # Discard the binding before deferred cleanup restores its original value.
    rm(list = name, envir = .GlobalEnv)
  }
  for (n in parallel_ale_worker_counts()) {
    parallel_ale_workers(n)
    for (kind in c("active", "lazy")) {
      check_binding("predict.gam", mgcv::predict.gam, kind)
      check_binding("Predict.matrix.tprs.smooth", mgcv::Predict.matrix.tprs.smooth, kind)
    }
  }
})
