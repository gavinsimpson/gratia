## Test family and link utilities

val <- 1
l <- list(mer = 1:3, gam = 1:3)

test_that("link() works with a glm() model", {
  f <- link(m_glm)
  expect_type(f, "closure")
  expect_identical(f, gaussian()$linkfun)
})

test_that("link() works with a gam() model", {
  f <- link(m_gam)
  expect_type(f, "closure")
  expect_identical(f, gaussian()$linkfun)
})

test_that("link() works with a gamm() model", {
  f <- link(m_gamm)
  expect_type(f, "closure")
  expect_identical(f, gaussian()$linkfun)
})

test_that("link() works with a gamm4() model", {
  f <- link(m_gamm4)
  expect_type(f, "closure")
  expect_identical(f, gaussian()$linkfun)
})

test_that("link.list() fails with a list that isn't a gamm4", {
  expect_error(link(l),
    regexp = "`object` does not appear to a `gamm4` model object",
    fixed = TRUE
  )
})

test_that("link() works with a bam() model", {
  f <- link(m_bam)
  expect_type(f, "closure")
  expect_identical(f, gaussian()$linkfun)
})

test_that("link() works with a gam() gaulss model", {
  f <- link(m_gaulss)
  expect_type(f, "closure")
  expect_identical(f, gaussian()$linkfun)
})

test_that("inv_link() works with a gam() model", {
  f <- inv_link(m_gam)
  expect_type(f, "closure")
  expect_identical(f, gaussian()$linkinv)
})

test_that("inv_link() works with a glm() model", {
  f <- inv_link(m_glm)
  expect_type(f, "closure")
  expect_identical(f, gaussian()$linkinv)
})

test_that("inv_link() works with a gamm() model", {
  f <- inv_link(m_gamm)
  expect_type(f, "closure")
  expect_identical(f, gaussian()$linkinv)
})

test_that("inv_link() works with a gamm4() model", {
  f <- inv_link(m_gamm4)
  expect_type(f, "closure")
  expect_identical(f, gaussian()$linkinv)
})

test_that("inv_link() works with a bam() model", {
  f <- inv_link(m_bam)
  expect_type(f, "closure")
  expect_identical(f, gaussian()$linkinv)
})

test_that("inv_link.list() fails with a list that isn't a gamm4", {
  expect_error(inv_link(l),
    regexp = "`object` does not appear to a `gamm4` model object",
    fixed = TRUE
  )
})

test_that("inv_link() works with a gam() gaulss model", {
  f <- inv_link(m_gaulss)
  expect_type(f, "closure")
  expect_identical(f, gaussian()$linkinv)
})

## Every previously supported single-link family uses its supplied functions.
single_link_families <- list(
  gaussian = function() gaussian(),
  poisson = function() poisson(),
  binomial = function() binomial(),
  Gamma = function() Gamma(),
  inverse.gaussian = function() inverse.gaussian(),
  quasi = function() quasi(),
  quasibinomial = function() quasibinomial(),
  quasipoisson = function() quasipoisson(),
  negbin = function() negbin(theta = 0.8),
  nb = function() nb(),
  Tweedie = function() Tweedie(p = 1.2),
  tw = function() tw(),
  scat = function() scat(),
  betar = function() betar(),
  ocat = function() ocat(theta = c(1, 2, 3)),
  ziP = function() ziP(),
  cox.ph = function() cox.ph(),
  cnorm = function() cnorm(),
  clog = function() clog(),
  bcg = function() bcg(),
  cpois = function() cpois()
)

for (family_id in names(single_link_families)) {
  test_that(paste("link extraction works for", family_id), {
    fam <- single_link_families[[family_id]]()
    for (parameter in list(NULL, "location", "mu")) {
      expect_identical(link(fam, parameter = parameter), fam$linkfun)
      expect_identical(inv_link(fam, parameter = parameter), fam$linkinv)
      for (inverse in c(FALSE, TRUE)) {
        expected <- if (inverse) fam$linkinv else fam$linkfun
        fun <- extract_link(fam, parameter = parameter, inverse = inverse)
        expect_identical(fun, expected)
        expect_identical(fun(val), expected(val))
      }
    }
    if (inherits(fam, "general.family")) {
      expect_identical(extract_link(fam, "location", TRUE), fam$linkinv)
    } else {
      expect_identical(extract_link(fam, TRUE), fam$linkinv)
    }
    expect_error(link(fam, parameter = "scale"), "'arg' should be")
    expect_error(inv_link(fam, parameter = "scale"), "'arg' should be")
    expect_error(extract_link(fam, parameter = "scale"), "'arg' should be")
  })
}

for (family_id in c("clognorm", "elf")) {
  test_that(paste("link extraction works for optional family", family_id), {
    if (family_id == "clognorm") {
      skip_if_not_installed("mgcvUtils")
      fam <- mgcvUtils::clognorm()
    } else {
      skip_if_not_installed("qgam")
      fam <- qgam::elf(qu = 0.5, co = 0.1)
    }
    for (parameter in list(NULL, "location", "mu")) {
      expect_identical(link(fam, parameter = parameter), fam$linkfun)
      expect_identical(inv_link(fam, parameter = parameter), fam$linkinv)
      expect_identical(extract_link(fam, parameter = parameter), fam$linkfun)
      expect_identical(extract_link(fam, parameter = parameter, inverse = TRUE),
        fam$linkinv)
    }
    expect_error(extract_link(fam, parameter = "power"), "'arg' should be")
  })
}

test_that("link extraction honours alternative supplied links", {
  for (fam in list(gaussian("log"), poisson("identity"),
      binomial("cloglog"), Gamma("log"), nb(link = "identity"))) {
    expect_identical(link(fam), fam$linkfun)
    expect_identical(inv_link(fam), fam$linkinv)
    expect_identical(extract_link(fam), fam$linkfun)
    expect_identical(extract_link(fam, inverse = TRUE), fam$linkinv)
  }
})

## Expected aliases and predictor positions, including the twlss scale fix.
multi_link_families <- list(
  gaulss = list(make = gaulss,
    parameters = c(location = 1L, mu = 1L, scale = 2L, sigma = 2L)),
  gammals = list(make = gammals,
    parameters = c(location = 1L, mu = 1L, scale = 2L, theta = 2L, phi = 2L)),
  gumbls = list(make = gumbls,
    parameters = c(location = 1L, mu = 1L, scale = 2L)),
  twlss = list(make = twlss,
    parameters = c(location = 1L, mu = 1L, power = 2L,
      scale = 3L, sigma = 3L, phi = 3L)),
  gevlss = list(make = gevlss,
    parameters = c(location = 1L, mu = 1L, scale = 2L, sigma = 2L,
      shape = 3L, xi = 3L)),
  ziplss = list(make = ziplss,
    parameters = c(location = 1L, mu = 1L, scale = 2L, pi = 2L)),
  shash = list(make = shash,
    parameters = c(location = 1L, mu = 1L, scale = 2L, sigma = 2L,
      skewness = 3L, epsilon = 3L, kurtosis = 4L, delta = 4L))
)

for (family_id in names(multi_link_families)) {
  test_that(paste("link extraction resolves all parameters for", family_id), {
    spec <- multi_link_families[[family_id]]
    fam <- spec$make()
    expect_identical(link(fam), fam$linfo[[1L]]$linkfun)
    expect_identical(inv_link(fam), fam$linfo[[1L]]$linkinv)
    expect_identical(extract_link(fam), fam$linfo[[1L]]$linkfun)
    expect_identical(extract_link(fam, inverse = TRUE), fam$linfo[[1L]]$linkinv)
    for (parameter in names(spec$parameters)) {
      lobj <- fam$linfo[[spec$parameters[[parameter]]]]
      expect_identical(link(fam, parameter = parameter), lobj$linkfun)
      expect_identical(inv_link(fam, parameter = parameter), lobj$linkinv)
      expect_identical(extract_link(fam, parameter = parameter), lobj$linkfun)
      expect_identical(extract_link(fam, parameter = parameter, inverse = TRUE),
        lobj$linkinv)
    }
    ## Several real predictors share the same link. Distinct marker functions
    ## also verify the predictor positions for every parameter and alias.
    for (index in seq_along(fam$linfo)) {
      fam$linfo[[index]]$linkfun <- local({
        i <- index
        function(x) x + i
      })
      fam$linfo[[index]]$linkinv <- local({
        i <- index
        function(x) x - i
      })
    }
    for (parameter in names(spec$parameters)) {
      index <- spec$parameters[[parameter]]
      expect_equal(link(fam, parameter = parameter)(0), index)
      expect_equal(inv_link(fam, parameter = parameter)(0), -index)
      expect_equal(extract_link(fam, parameter = parameter)(0), index)
      expect_equal(extract_link(fam, parameter = parameter, inverse = TRUE)(0),
        -index)
    }
    unsupported <- setdiff(c("power", "shape", "pi", "skewness", "phi", "bogus"),
      names(spec$parameters))
    for (parameter in unsupported) {
      expect_error(link(fam, parameter = parameter), "'arg' should be")
      expect_error(inv_link(fam, parameter = parameter), "'arg' should be")
      expect_error(extract_link(fam, parameter = parameter), "'arg' should be")
      expect_error(extract_link(fam, parameter = parameter, inverse = TRUE),
        "'arg' should be")
    }
  })
}

for (family_id in c("mvn", "multinom")) {
  test_that(paste("link extraction respects predictor selection for", family_id), {
    fam <- if (family_id == "mvn") mvn(d = 2) else multinom(K = 2)
    ## Distinct functions ensure selecting eta1 by accident cannot pass.
    fam$linfo[[2L]]$linkfun <- function(x) x + 10
    fam$linfo[[2L]]$linkinv <- function(x) x - 10
    for (parameter in list(NULL, "location", "mu")) {
      expect_identical(link(fam, parameter = parameter), fam$linfo[[1L]]$linkfun)
      expect_identical(inv_link(fam, parameter = parameter), fam$linfo[[1L]]$linkinv)
      expect_identical(extract_link(fam, parameter = parameter),
        fam$linfo[[1L]]$linkfun)
      expect_identical(extract_link(fam, parameter = parameter, inverse = TRUE),
        fam$linfo[[1L]]$linkinv)
      for (which_eta in 1:2) {
        lobj <- fam$linfo[[which_eta]]
        expect_identical(link(fam, parameter = parameter, which_eta = which_eta),
          lobj$linkfun)
        expect_identical(inv_link(fam, parameter = parameter, which_eta = which_eta),
          lobj$linkinv)
        expect_identical(extract_link(fam, parameter = parameter,
          which_eta = which_eta), lobj$linkfun)
        expect_identical(extract_link(fam, parameter = parameter,
          which_eta = which_eta, inverse = TRUE), lobj$linkinv)
      }
    }
    for (extractor in list(link, inv_link, extract_link)) {
      expect_error(extractor(fam, parameter = "power"), "'arg' should be")
      for (which_eta in list(0, 3, -1, 1.5, NA_real_, NaN, Inf, "2",
          TRUE, 1 + 1i, numeric(), list(2), list(2, 1))) {
        expect_error(extractor(fam, which_eta = which_eta), "Invalid 'which_eta'")
      }
      expect_warning(fun <- extractor(fam, which_eta = c(2, 1)),
        "using only the first")
      component <- if (identical(extractor, inv_link)) "linkinv" else "linkfun"
      expect_identical(fun, fam$linfo[[2L]][[component]])
    }
    expect_warning(fun <- extract_link(fam, which_eta = c(2, 1), inverse = TRUE),
      "using only the first")
    expect_identical(fun, fam$linfo[[2L]]$linkinv)
    model <- structure(list(family = fam), class = c("gam", "glm", "lm"))
    expect_identical(link(model, which_eta = 2), fam$linfo[[2L]]$linkfun)
    expect_identical(inv_link(model, which_eta = 2), fam$linfo[[2L]]$linkinv)
  })
}

test_that("model wrappers forward parameter selection", {
  for (model in list(m_gaulss, structure(list(gam = m_gaulss), class = "gamm"),
      list(gam = m_gaulss, mer = m_gamm4$mer))) {
    fam <- family(m_gaulss)
    expect_identical(link(model, parameter = "scale"), fam$linfo[[2L]]$linkfun)
    expect_identical(inv_link(model, parameter = "scale"), fam$linfo[[2L]]$linkinv)
  }
})

test_that("single-link extraction supports custom family names", {
  for (fam in list(gaussian(), nb(), cox.ph())) {
    fam$family <- "custom family"
    expect_identical(link(fam), fam$linkfun)
    expect_identical(inv_link(fam), fam$linkinv)
    expect_identical(extract_link(fam), fam$linkfun)
    expect_identical(extract_link(fam, inverse = TRUE), fam$linkinv)
    expect_error(extract_link(fam, parameter = "power"), "'arg' should be")
  }
})

test_that("extraction reports missing functions and parameter mappings", {
  expect_error(extract_link.family(1), "'family' is not a family object")
  for (fam in list(gaussian(), gaulss())) {
    if (is.null(fam$linfo)) {
      fam$linkfun <- NULL
      fam$linkinv <- 1
    } else {
      fam$linfo[[1L]]$linkfun <- NULL
      fam$linfo[[1L]]$linkinv <- 1
    }
    expect_error(link(fam), "does not supply a 'linkfun' function")
    expect_error(inv_link(fam), "does not supply a 'linkinv' function")
    expect_error(extract_link(fam), "does not supply a 'linkfun' function")
    expect_error(extract_link(fam, inverse = TRUE),
      "does not supply a 'linkinv' function")
  }
  fam <- twlss()
  fam$linfo <- fam$linfo[1:2]
  expect_error(extract_link(fam, parameter = "scale"),
    "No link information for parameter 'scale'")
  fam <- gaulss()
  fam$family <- "custom multi-link family"
  for (extractor in list(link, inv_link, extract_link)) {
    expect_error(extractor(fam), "Unknown parameter mapping")
  }
})

test_that("link extraction handles fitted family names", {
  for (model in list(m_scat, m_ziP, m_censor)) {
    fam <- family(model)
    expect_identical(link(model), fam$linkfun)
    expect_identical(inv_link(model), fam$linkinv)
    expect_identical(extract_link(fam), fam$linkfun)
    expect_identical(extract_link(fam, inverse = TRUE), fam$linkinv)
  }
})

## test other gamm4 family utils
test_that("family.gamm4 works for a gamm4 object", {
  fam <- family(m_gamm4)
  expect_s3_class(fam, class = "family")
  expect_equal(fam, gaussian(), ignore_function_env = TRUE)
})

test_that("family.gamm4 throws an error when passed a non-gamm4 object", {
  expect_error(family(l),
    regexp = "`object` does not appear to a `gamm4` model object",
    fixed = TRUE
  )
})

## test gamm family
test_that("family.gamm works for a gamm object", {
  fam <- family(m_gamm)
  expect_s3_class(fam, class = "family")
  expect_equal(fam, gaussian(), ignore_function_env = TRUE)
})

## test family name
test_that("family_name() works with a gam() model", {
  f <- family_name(m_gam)
  expect_type(f, "character")
  expect_identical(f, "gaussian")
})

test_that("family_name() works with a glm() model", {
  f <- family_name(m_glm)
  expect_type(f, "character")
  expect_identical(f, "gaussian")
})

test_that("family_name() works with a gamm() model", {
  f <- family_name(m_gamm)
  expect_type(f, "character")
  expect_identical(f, "gaussian")
})

test_that("family_name() works with a gamm4() model", {
  f <- family_name(m_gamm4)
  expect_type(f, "character")
  expect_identical(f, "gaussian")
})

test_that("family_name() works with a bam() model", {
  f <- family_name(m_bam)
  expect_type(f, "character")
  expect_identical(f, "gaussian")
})

test_that("family_name.list() fails with a list that isn't a gamm4", {
  expect_error(family_name(l),
    regexp = "`object` does not appear to a `gamm4` model object",
    fixed = TRUE
  )
})

test_that("family_name() works with a gam() gaulss model", {
  f <- family_name(m_gaulss)
  expect_type(f, "character")
  expect_identical(f, "gaulss")
})

test_that("family_name() works with a family() object", {
  f <- family_name(gaussian())
  expect_type(f, "character")
  expect_identical(f, "gaussian")
})

# special cnorm tests
test_that("family utils work on cnorm() family objects from a model", {
  ## link
  f <- extract_link(family(m_censor))
  expect_type(f, "closure")
  expect_identical(f(val), val)
  expect_identical(f, cnorm()$linkfun)
  ## inverse
  f <- extract_link(family(m_censor), inverse = TRUE)
  expect_type(f, "closure")
  expect_identical(f(val), val)
  expect_identical(f, cnorm()$linkinv)

  f <- family_name(m_censor)
  expect_type(f, "character")
  expect_match(f, "^cnorm\\(\\d+\\.?\\d+\\)$")
})
