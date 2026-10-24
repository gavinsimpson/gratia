# Test model fit inventory

This inventory reads every R file under tests without executing it. Entries are constructor call sites, not runtime fit counts: a call in a loop or helper can fit several models, while a cached call may not run at all.

The audit decisions and changes are described in README.md. Dynamic calls and update calls are listed so they are not missed; some update calls change formulas rather than refitting models.

## tests/integration/test-gjrm-example-checks.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/integration/test-inlaws-diagnostics.R

Keep in the separate optional integration workflow; these native families are not setup.R models.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 121 | `mgcv::gam` | fitted inlaws diagnostics share probabilities and means across plot sizes | `list(y ~ x, ~1)` | `inlaws::nbls()` |
| 138 | `mgcv::gam` | fitted inlaws diagnostics share probabilities and means across plot sizes | `list(y ~ x, ~1)` | `inlaws::nbls()` |
| 164 | `mgcv::gam` | fitted inlaws mixtures, ordinal and censored families produce the intended panels | `form` | `z$f` |
| 198 | `mgcv::gam` | inlaws factories survive grouped fitting and fitted nuisance-parameter reconstruction | `cbind(y, index) ~ s(x, k = 4)` | `mgcv::gfam(list(poisson(), f))` |
| 245 | `mgcv::gam` | native factories survive construction, cloning, nuisance updates and fitted reconstruction | `y ~ x` | `inlaws::pig()` |
| 257 | `mgcv::gam` | beta-binomial diagnostics respect retained trial totals and fitting order | `list(cbind(y, trials - y) ~ x, ~1)` | `inlaws::betabinomial()` |
| 297 | `mgcv::gam` | multivariate native diagnostics retain columns, moments and joint response dependence | `list(y ~ x, ~1, ~1)` | `f` |
| 357 | `mgcv::gam` | fitted censoring rules remain aligned after subsetting and missing responses | `form` | `f` |
| 387 | `mgcv::gam` | ordinary native count and continuous families survive grouped parameter reconstruction | `cbind(y, index) ~ x` | `mgcv::gfam(list(poisson(), f))` |

## tests/test-all.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/fixtures/secondary-models.R

Already shared on demand behind skip_on_cran(). Keep outside the CRAN workload.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 4 | `gam` | File setup or helper | `y ~ s(x0, x1, x2, x3)` | `` |
| 6 | `gam` | File setup or helper | `y ~ te(x0, x1, x2, x3, k = c(3, 3, 3, 3))` | `` |
| 10 | `gam` | File setup or helper | `y ~ t2(x0, x1, x2, x3, k = c(3, 3, 3, 3))` | `` |
| 14 | `gam` | File setup or helper | `y ~ fac + s(x, z, k = 40, by = fac)` | `` |
| 33 | `gam` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `bcg()` |
| 40 | `gam` | File setup or helper | `uptake ~ Plant + s(conc, k = 5) + s(conc, by = Plant, k = 5)` | `` |
| 44 | `gam` | File setup or helper | `y ~ s(x0) + ti(x0, fac, bs = c("tp", "re"), k = 5) + s(x1, k = 20)` | `` |
| 50 | `gam` | File setup or helper | `y ~ s(x0) + ti(x0, x1, fac, bs = c("tp", "tp", "re"), k = 5) + s(x1, k = 20)` | `` |
| 70 | `gam` | File setup or helper | `y ~ te(f_lag, lag)` | `` |
| 102 | `bam` | File setup or helper | `y ~ s(x, k = 20)` | `` |
| 107 | `bam` | File setup or helper | `y ~ series + s(x, k = 20, by = series)` | `` |
| 118 | `gam` | File setup or helper | `y ~ s(v, w, bs = "so", xt = list(bnd = soap_fsb2, nmax = 100))` | `` |
| 170 | `gam` | File setup or helper | `z ~ s(x, y, k = c(30, 15), bs = "so", xt = list(bnd = sf_nested_bndry, nmax = 60))` | `` |

## tests/testthat/helper-builder-diagnostics.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/helper-conditional-differences.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/helper-cran.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/helper-factor-support.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/helper-gamm4.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/helper-gfam-samples.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/helper-gjrm-joint.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/helper-lss-models.R

Already cached by family. Keep the offsets, weights and parameter definitions needed by the independent references.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 34 | `mgcv::gam` | File setup or helper | `formulas` | `family` |

## tests/testthat/helper-parallel.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/helper-secondary-models.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/helper-smooth-bands.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/helper-spherical.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/helper-transformed-data.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/helper-variance-components.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/helper-vdiffr.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/setup.R

Shared setup or a shared builder. New specialized fits are requested on demand.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 34 | `gam` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `` |
| 39 | `gam` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `` |
| 44 | `gam` | File setup or helper | `y ~ s(x0, bs = "ts") + s(x1, bs = "cs") + s(x2, bs = "ts") + s(x3, bs = "ts")` | `` |
| 51 | `gam` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `` |
| 56 | `gam` | File setup or helper | `y ~ s(x0, bs = "cr") + s(x1, bs = "bs") + s(x2, k = 15) + s(x3, bs = "ps")` | `` |
| 63 | `gam` | File setup or helper | `y ~ s(x, z, k = 40)` | `` |
| 65 | `gam` | File setup or helper | `y ~ s(x, z, k = 20, bs = "ds")` | `` |
| 70 | `gam` | File setup or helper | `y ~ s(x0, x1, x2)` | `` |
| 72 | `gam` | File setup or helper | `y ~ te(x, z, k = c(5, 5))` | `` |
| 74 | `gamm` | File setup or helper | `y ~ te(x, z, k = c(5, 5))` | `` |
| 78 | `gam` | File setup or helper | `y ~ s(x, k = 5) + s(z, k = 5) + ti(x, z, k = c(5, 5))` | `` |
| 82 | `gam` | File setup or helper | `y ~ t2(x, z, k = c(5, 5))` | `` |
| 84 | `gam` | File setup or helper | `y ~ te(x0, x1, x2, k = c(3, 3, 3))` | `` |
| 88 | `gam` | File setup or helper | `y ~ t2(x0, x1, x2, k = c(3, 3, 3))` | `` |
| 92 | `gam` | File setup or helper | `y ~ s(x2, by = x1)` | `` |
| 94 | `gam` | File setup or helper | `y ~ fac + s(x2, by = fac) + s(x0)` | `` |
| 99 | `gam` | File setup or helper | `y ~ s(fac, bs = "re") + s(x2) + s(x2, by = fac, m = 1) + s(x0)` | `` |
| 104 | `gamm` | File setup or helper | `y ~ fac + s(x2, by = fac) + s(x0)` | `` |
| 108 | `gamm4` | File setup or helper | `y ~ fac + s(x2, by = fac) + s(x0)` | `` |
| 112 | `bam` | File setup or helper | `y ~ fac + s(x2, by = fac) + s(x0)` | `` |
| 114 | `gam` | File setup or helper | `y ~ fac + s(x2, by = fac)` | `` |
| 118 | `gam` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2, by = fac)` | `` |
| 123 | `gam` | File setup or helper | `y ~ s(x2) + s(fac, x2, bs = "sz") + s(x0)` | `` |
| 163 | `bam` | File setup or helper | `y ~ s(x) + s(f1, x, bs = "sz") + s(f2, x, bs = "sz") + s(f1, f2, x, bs = "sz", id = 1)` | `` |
| 176 | `gamm` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `` |
| 181 | `gam` | File setup or helper | `y ~ s(x0)` | `` |
| 183 | `gam` | File setup or helper | `y ~ s(x0) + offset(log(off))` | `` |
| 191 | `bam` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `` |
| 196 | `gam` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `` |
| 201 | `gamm4` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `` |
| 208 | `gam` | File setup or helper | `list(y ~ s(x0) + s(x1) + s(x2) + s(x3), ~1)` | `gaulss` |
| 226 | `gam` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `betar(link = "logit")` |
| 258 | `gam` | File setup or helper | `list(y ~ s(x0) + s(x2), ~s(x1) + s(x3))` | `gammals()` |
| 289 | `gam` | File setup or helper | `list(y ~ s(x0) + s(x1), ~s(x2) + s(x3))` | `gumbls()` |
| 329 | `gam` | File setup or helper | `list(y ~ s(x2), ~s(x0), ~s(x1))` | `gevlss()` |
| 342 | `gam` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `scat()` |
| 348 | `gam` | File setup or helper | `y ~ te(x2)` | `` |
| 349 | `gam` | File setup or helper | `y ~ ti(x2)` | `` |
| 350 | `gam` | File setup or helper | `y ~ t2(x2)` | `` |
| 352 | `lm` | File setup or helper | `y ~ x0 + x1 + x2 + x3` | `` |
| 354 | `glm` | File setup or helper | `y ~ x0 + x1 + x2 + x3` | `` |
| 361 | `bam` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `poisson()` |
| 365 | `gam` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `nb()` |
| 369 | `gam` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `negbin(25)` |
| 373 | `gam` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `tw()` |
| 403 | `gam` | File setup or helper | `y ~ s(x0) + s(x1, fac, bs = "fs", k = 5) + s(x2, k = 20)` | `` |
| 407 | `gamm` | File setup or helper | `y ~ s(x0) + s(x1, fac, bs = "fs", k = 5) + s(x2, k = 20)` | `` |
| 412 | `gamm4::gamm4` | File setup or helper | `y ~ s(x0) + s(x1, fac, bs = "fs", k = 5) + s(x2, k = 20)` | `` |
| 425 | `gam` | File setup or helper | `y ~ s(fac, bs = "re") + s(x0) + s(x1) + s(x2) + s(x3)` | `` |
| 436 | `gam` | File setup or helper | `y ~ fac + s(ranef, bs = "re", by = fac) + s(x0) + s(x1) + s(x2)` | `` |
| 449 | `gam` | File setup or helper | `y ~ fac * ff + s(x0) + s(x1) + s(x2)` | `` |
| 453 | `gam` | File setup or helper | `y ~ fac * ff + x0 + s(x1) + s(x2)` | `` |
| 457 | `gam` | File setup or helper | `y ~ fac * ff + x0 + x1 + x2` | `` |
| 462 | `gam` | File setup or helper | `y ~ fac + ff + log(x0) + x1 + poly(x2, 2, raw = TRUE)` | `` |
| 470 | `scam` | File setup or helper | `Date ~ s(Depth, k = 5, bs = "mpd")` | `` |
| 474 | `scam` | File setup or helper | `Date ~ s(Depth, k = 5, bs = "mdcx")` | `` |
| 478 | `scam` | File setup or helper | `Date ~ s(Depth, k = 5, bs = "mdcv")` | `` |
| 507 | `scam` | File setup or helper | `y ~ s(x1, bs = "cr") + s(x2, bs = "mpi")` | `` |
| 508 | `scam` | File setup or helper | `y ~ s(x1, bs = "cr") + s(x2, bs = "micx")` | `` |
| 509 | `scam` | File setup or helper | `y ~ s(x1, bs = "cr") + s(x2, bs = "micv")` | `` |
| 517 | `gam` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `ocat(R = n_categories)` |
| 552 | `gam` | File setup or helper | `y ~ s(latitude, longitude, bs = "sos", k = 60)` | `` |
| 555 | `gam` | File setup or helper | `y ~ s(north, east, bs = "sos", k = 20)` | `` |
| 558 | `gam` | File setup or helper | `y ~ region + s(north, east, bs = "sos", by = region, k = 15)` | `` |
| 571 | `gam` | File setup or helper | `y_cens ~ s(x0) + s(x1) + s(x2) + s(x3)` | `cnorm()` |
| 582 | `gam` | File setup or helper | `y ~ x0 + x1 + I(x1^2) + s(x2, bs = "cr", k = 12) + fac + x3:fac + I(x1 * x2) + logi` | `` |
| 617 | `gam` | File setup or helper | `list(y ~ s(x2) + x3, ~s(x0) + x1)` | `ziplss()` |
| 632 | `gam` | File setup or helper | `list(y ~ s(x0) + s(x1) + s(x2) + s(x3), ~1, ~1)` | `twlss()` |
| 661 | `gam` | File setup or helper | `y ~ s(v, w, k = 30, bs = "so", xt = list(bnd = soap_fsb, nmax = 100))` | `` |
| 667 | `gam` | File setup or helper | `y ~ s(v, w, k = 10, bs = "sf", xt = list(bnd = soap_fsb, nmax = 40)) + s(v, w, k = 10, bs = "sw", xt = list(bnd = soap_fsb,      nmax = 40))` | `` |
| 691 | `gam` | File setup or helper | `y ~ s(v, w, k = 8, bs = "sf", xt = list(bnd = soap_component_boundary, nmax = 40)) + s(v, w, k = 8, bs = "sw", xt = list(bnd = soap_component_boundary,      nmax = 40))` | `` |
| 702 | `gam` | File setup or helper | `y ~ te(v, w, time, d = c(2, 1), k = c(10, 4), bs = c("sf", "cr"), xt = list(list(bnd = soap_fsb, nmax = 40), NULL)) + te(v,      w, time, d = c(2, 1), k = c(10, 4), bs = c("sw", "cr"), xt = list(list(bnd = soap_fsb, nmax = 40), NULL))` | `` |
| 725 | `gam` | File setup or helper | `y ~ month + var + s(x0) + s(x1) + s(x2) + s(x3)` | `` |
| 761 | `gam` | File setup or helper | `list(y0 ~ s(x0) + s(x1), y1 ~ s(x2) + s(x3))` | `mvn(d = 2)` |
| 797 | `gam` | File setup or helper | `list(y ~ s(x1) + s(x2), ~s(x1) + s(x2))` | `multinom(K = 2)` |
| 830 | `gam` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `ziP()` |
| 851 | `gamm4::gamm4` | File setup or helper | `y ~ fac + z + s(x, k = 5)` | `` |
| 853 | `gamm4::gamm4` | File setup or helper | `binary ~ fac + z + s(x, k = 5)` | `binomial()` |
| 855 | `gamm4::gamm4` | File setup or helper | `count ~ fac + z + s(x, k = 5) + offset(off)` | `poisson()` |
| 857 | `gamm4::gamm4` | File setup or helper | `y ~ fac + t2(x, z, k = c(4, 4))` | `` |
| 859 | `gamm4::gamm4` | File setup or helper | `y ~ fac + s(x, by = fac, k = 5)` | `` |
| 863 | `gamm4::gamm4` | File setup or helper | `y ~ fac + z + s(x, k = 5)` | `` |
| 887 | `gam` | File setup or helper | `cbind(y, family_id) ~ group + s(x, k = 5) + offset(off)` | `gfam(list(binomial(), gaussian(), poisson()))` |
| 890 | `bam` | File setup or helper | `cbind(y, family_id) ~ group + s(x, k = 5) + offset(off)` | `gfam(list(binomial(), gaussian(), poisson()))` |
| 895 | `gam` | File setup or helper | `response ~ group + s(x, k = 5) + offset(off)` | `gfam(list(binomial(), gaussian(), poisson()))` |
| 900 | `gam` | File setup or helper | `cbind(y, family_id) ~ group + s(x, k = 5) + offset(off)` | `gfam(list(binomial(), gaussian(), poisson()))` |
| 907 | `gam` | File setup or helper | `cbind(y, family_id) ~ s(x, k = 5)` | `gfam(list(gaussian(), gaussian()))` |
| 925 | `gam` | File setup or helper | `cbind(y, family_id) ~ group + s(x, k = 5)` | `gfam(list(nb(), gaussian(), nb(), tw()))` |
| 943 | `gam` | File setup or helper | `cbind(y, family_id) ~ group + s(x, k = 4)` | `gfam(list(ocat(R = 4), poisson()))` |
| 969 | `gam` | File setup or helper | `factor_support_formula` | `` |
| 970 | `gam` | File setup or helper | `factor_support_formula` | `` |
| 971 | `gam` | File setup or helper | `y ~ a * b + s(x, k = 5)` | `` |
| 972 | `bam` | File setup or helper | `factor_support_formula` | `` |
| 973 | `bam` | File setup or helper | `factor_support_formula` | `` |
| 974 | `gamm` | File setup or helper | `factor_support_formula` | `` |
| 975 | `gamm4` | File setup or helper | `factor_support_formula` | `` |
| 977 | `gam` | File setup or helper | `factor_support_formula` | `` |
| 981 | `gam` | File setup or helper | `factor_support_formula` | `` |
| 984 | `gam` | File setup or helper | `y ~ factor(j%%2) + b + s(x, k = 5)` | `` |
| 988 | `gam` | File setup or helper | `y ~ foo(j) + b + s(x, k = 5)` | `` |
| 1001 | `gam` | File setup or helper | `y ~ a + b + c + s(x, k = 5)` | `` |
| 1005 | `gam` | File setup or helper | `category ~ a + b + s(x, k = 5)` | `ocat(R = 3)` |
| 1007 | `gam` | File setup or helper | `binary ~ a + s(x, k = 5)` | `binomial()` |
| 1020 | `gam` | File setup or helper | `as.formula(paste0("y ~ s(u, k = 4) + ", constructor, "(f, x, z, bs = c('re', 'tp', 'tp'), k = c(4, 3, 3))"))` | `` |
| 1025 | `gam` | File setup or helper | `y ~ ti(x, z, f, g, bs = c("tp", "tp", "re", "re"), k = c(3, 3, 4, 3))` | `` |
| 1028 | `gam` | File setup or helper | `y ~ t2(f, x, z, u, bs = c("re", "tp", "tp", "tp"), k = c(4, 3, 3, 3))` | `` |
| 1031 | `gam` | File setup or helper | `y ~ te(f, x, g, bs = c("re", "tp", "re"), k = c(4, 3, 3))` | `` |
| 1034 | `gam` | File setup or helper | `y ~ ti(x, z, u, bs = c("tp", "tp", "re"), k = c(3, 3, 3))` | `` |
| 1037 | `gam` | File setup or helper | `y ~ ti(f, log(x), z, bs = c("re", "tp", "tp"), k = c(4, 3, 3))` | `` |
| 1047 | `gam` | File setup or helper | `y ~ te(f, g, h, bs = "re", k = c(3, 3, 3))` | `` |
| 1057 | `gam` | File setup or helper | `y ~ te(x, f, bs = c("tp", "re"), k = c(3, 17))` | `` |
| 1062 | `gam` | File setup or helper | `y ~ te(u, x, z, bs = c("re", "tp"), d = c(1, 2), k = c(3, 6))` | `` |
| 1090 | `gam` | File setup or helper | `y ~ s(area, bs = "mrf", xt = xt) + s(x, k = 5)` | `` |
| 1092 | `gam` | File setup or helper | `y ~ s(area, bs = "mrf", k = 5, xt = xt)` | `` |
| 1094 | `gam` | File setup or helper | `y ~ s(area, bs = "mrf", xt = list(penalty = S))` | `` |
| 1096 | `gam` | File setup or helper | `y ~ s(area, bs = "mrf", xt = list(penalty = disconnected))` | `` |
| 1098 | `gam` | File setup or helper | `y ~ s(area, bs = "mrf", k = 5, xt = list(polys = polys))` | `` |
| 1100 | `gam` | File setup or helper | `y ~ fac + s(area, bs = "mrf", by = fac, xt = xt)` | `` |
| 1102 | `gam` | File setup or helper | `y ~ s(area, bs = "mrf", by = z, xt = xt)` | `` |
| 1104 | `bam` | File setup or helper | `y ~ s(area, bs = "mrf", k = 5, xt = xt)` | `` |
| 1108 | `gam` | File setup or helper | `y ~ s(area, bs = "mrf", k = 5, xt = xt)` | `` |
| 1110 | `gam` | File setup or helper | `y ~ s(area, bs = "mrf", xt = xt)` | `` |
| 1112 | `gam` | File setup or helper | `y ~ s(factor(area), bs = "mrf", k = 5, xt = xt)` | `` |
| 1124 | `gam` | File setup or helper | `y ~ f + s(x, k = 8) + s(z, k = 6)` | `` |
| 1126 | `gam` | File setup or helper | `y ~ s(log(x + 1), k = 8) + s(z, by = x, k = 6)` | `` |
| 1139 | `gam` | File setup or helper | `cbind(y, family_id) ~ factor(family_id) + s(x, k = 4)` | `gfam(list(get(name, asNamespace("mgcv"))(), gaussian(), poisson()))` |
| 1154 | `gam` | File setup or helper | `cbind(y, family_id) ~ factor(family_id) + s(x, k = 4)` | `gfam(list(get(name, asNamespace("mgcv"))(), gaussian()))` |
| 1169 | `gam` | File setup or helper | `cbind(y, family_id) ~ factor(family_id) + x` | `gfam(list(gaussian(), gaussian(), gaussian()))` |
| 1184 | `gam` | File setup or helper | `cbind(y, family_id) ~ factor(family_id) + s(x, k = 4)` | `gfam(list(Gamma(link = "log"), gaussian(), poisson()))` |
| 1200 | `mgcv::gam` | File setup or helper | `y ~ a * b + s(x, by = a, k = 5) + offset(log(exposure))` | `family` |
| 1216 | `mgcv::gam` | File setup or helper | `y ~ a + b + s(x, z, by = a, k = 8) + s(u, k = 4)` | `family` |
| 1226 | `mgcv::gam` | File setup or helper | `y ~ s(x, k = 6)` | `` |
| 1240 | `mgcv::gam` | File setup or helper | `y ~ group + s(x, by = group, bs = bs, k = 6)` | `` |
| 1242 | `mgcv::gam` | File setup or helper | `y ~ s(x, bs = bs, k = 6)` | `` |
| 1255 | `mgcv::gam` | File setup or helper | `y ~ x + s(z, k = 5) + f` | `` |
| 1269 | `gam` | File setup or helper | `y ~ a + b + x` | `` |
| 1288 | `fitter` | File setup or helper | `f` | `` |
| 1400 | `update` | File setup or helper | `f` | `` |
| 1402 | `fitter` | File setup or helper | `f` | `family` |
| 1416 | `mgcv::scasm` | File setup or helper | `y ~ s(x, bs = "sc", xt = "m+", k = 7) + offset(off)` | `` |
| 1431 | `mgcv::scasm` | File setup or helper | `form` | `` |
| 1441 | `mgcv::scasm` | File setup or helper | `y ~ s(x, bs = "sc", xt = "m+", k = 6) + offset(off)` | `` |
| 1450 | `gam` | File setup or helper | `mpg ~ s(wt, k = 4)` | `` |
| 1452 | `gam` | File setup or helper | `mpg ~ wt` | `` |
| 1463 | `fit` | File setup or helper | `y ~ x + z + fac * poly(w, 2) + s(w, k = 6) + offset(off)` | `` |
| 1478 | `gamm` | File setup or helper | `y ~ x + z + fac + s(w, k = 6)` | `` |
| 1480 | `gamm4::gamm4` | File setup or helper | `y ~ x + z + fac + s(w, k = 6)` | `` |
| 1485 | `gamm` | File setup or helper | `y ~ x + z + fac + s(w, k = 6)` | `` |
| 1487 | `gamm4::gamm4` | File setup or helper | `y ~ x + z + fac + s(w, k = 6)` | `` |
| 1498 | `scam::scam` | File setup or helper | `y ~ z + s(x, bs = "mpi", k = 6)` | `` |
| 1504 | `gam` | File setup or helper | `y ~ 1` | `` |
| 1509 | `bam` | File setup or helper | `y ~ te(x, z, k = c(5, 5))` | `` |
| 1526 | `mgcv::bam` | File setup or helper | `y ~ s(x, k = 5) + offset(off)` | `family` |
| 1528 | `mgcv::gam` | File setup or helper | `y ~ s(x, k = 5) + offset(off)` | `family` |
| 1548 | `mgcv::bam` | File setup or helper | `y ~ s(x, k = 5) + offset(off)` | `family` |
| 1550 | `mgcv::gam` | File setup or helper | `y ~ s(x, k = 5) + offset(off)` | `family` |
| 1566 | `mgcv::scasm` | File setup or helper | `form` | `` |
| 1582 | `mgcv::gam` | File setup or helper | `y ~ s(x, bs = bs, k = 7)` | `` |
| 1600 | `mgcv::gam` | File setup or helper | `y ~ s(x, k = 6) + offset(off)` | `family` |
| 1604 | `fit` | File setup or helper | `` | `` |
| 1605 | `fit` | File setup or helper | `` | `` |
| 1620 | `gam` | File setup or helper | `y/n ~ s(x0) + s(x1) + s(x2) + s(x3)` | `binomial` |
| 1633 | `mgcv::gam` | File setup or helper | `y ~ x` | `family` |
| 1649 | `mgcv::gam` | File setup or helper | `f` | `mgcv::multinom(K = k)` |
| 1670 | `fit` | File setup or helper | `formula` | `family` |
| 1678 | `glm` | File setup or helper | `count ~ spray` | `poisson()` |
| 1695 | `glmmTMB::glmmTMB` | File setup or helper | `y ~ s(x, k = 6) + (1 \| g)` | `` |

## tests/testthat/test-add-functions.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 50 | `gam` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `` |
| 51 | `gam` | File setup or helper | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `` |

## tests/testthat/test-ale-extremes.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 53 | `mgcv::gam` | extreme Gaussian scale gradients propagate into ALE uncertainty | `list(y ~ 1, ~x)` | `mgcv::gaulss(b = 0.2)` |
| 92 | `mgcv::gam` | extreme Tweedie power gradients propagate into ALE uncertainty | `list(y ~ 1, ~x, ~1)` | `mgcv::twlss(a = 1.2, b = 1.8)` |

## tests/testthat/test-ale-worker-inputs.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 44 | `mgcv::gam` | ALE keeps custom contrasts, predictor classes and controls in the caller | `y ~ x + z + f` | `` |
| 73 | `mgcv::gam` | ALE worker models release fitting environments without changing predictions | `y ~ s(x, k = 5) + s(f, bs = "re") + z` | `` |

## tests/testthat/test-ale.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 76 | `mgcv::gam` | linear ALE and its uncertainty agree with analytic results | `y ~ x + z + offset(off)` | `` |
| 124 | `mgcv::gam` | linear ALE and its uncertainty agree with analytic results | `y ~ x` | `` |
| 128 | `mgcv::gam` | linear ALE and its uncertainty agree with analytic results | `y ~ 0 + x` | `` |
| 138 | `mgcv::gam` | smooths, interactions, factors and blocks use whole-model changes | `y ~ s(x, k = 6) + s(z, k = 5)` | `` |
| 150 | `mgcv::gam` | smooths, interactions, factors and blocks use whole-model changes | `y ~ te(x, z, k = c(4, 4))` | `` |
| 151 | `mgcv::gam` | smooths, interactions, factors and blocks use whole-model changes | `y ~ s(x, k = 4) + ti(x, z, k = c(4, 4))` | `` |
| 152 | `mgcv::gam` | smooths, interactions, factors and blocks use whole-model changes | `y ~ fac + s(x, by = fac, k = 4) + z` | `` |
| 173 | `mgcv::gam` | response effects and centred gradients agree with independent calculations | `y ~ x * z + offset(off)` | `poisson()` |
| 268 | `mgcv::gam` | distributional ALE selects the correct coefficient block and offsets | `list(y ~ x + offset(off), ~z + offset(off2))` | `mgcv::gaulss(link = list("log", "logb"), b = 0.1)` |
| 300 | `mgcv::gam` | distributional ALE selects the correct coefficient block and offsets | `list(y ~ x, ~x, 1 + 2 ~ z - 1)` | `mgcv::gaulss()` |
| 320 | `mgcv::gam` | raw expressions, reference rows and BAM predictions are respected | `y ~ s(log(x), k = 5) + I(x * z) + offset(log(x))` | `` |
| 334 | `mgcv::gam` | raw expressions, reference rows and BAM predictions are respected | `y ~ s(I(température), k = 5) + z` | `` |
| 338 | `mgcv::bam` | raw expressions, reference rows and BAM predictions are respected | `y ~ s(x, k = 5) + z + offset(off)` | `` |
| 348 | `mgcv::gam` | raw expressions, reference rows and BAM predictions are respected | `y ~ x + z` | `` |
| 356 | `mgcv::gam` | bins, invalid inputs, unsupported targets and RNG are handled | `y ~ x + z` | `` |
| 404 | `lm` | bins, invalid inputs, unsupported targets and RNG are handled | `y ~ x` | `` |
| 485 | `mgcv::gam` | draw.ale uses saved values and computation labels | `y ~ x + z` | `` |
| 547 | `mgcv::gam` | distributional fits give natural-parameter ALE for each predictor | `formula` | `f` |
| 594 | `mgcv::gam` | scalar mean families and smoothing uncertainty use the selected scale | `y ~ s(x, k = 4) + z` | `families[[j]]` |

## tests/testthat/test-assemble-gamlss.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 20 | `GJRM::gamlss` | File setup or helper | `formulas` | `` |

## tests/testthat/test-assemble-gamm.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 15 | `mgcv::gamm` | File setup or helper | `formula` | `` |

## tests/testthat/test-assemble-glmmtmb.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 17 | `glmmTMB::glmmTMB` | File setup or helper | `y ~ x + s(w, bs = "cc", k = 6) + (1 + x \| g)` | `` |
| 19 | `glmmTMB::glmmTMB` | File setup or helper | `y ~ x + s(w, bs = "cc", k = 6)` | `` |
| 21 | `glmmTMB::glmmTMB` | File setup or helper | `y ~ x` | `` |

## tests/testthat/test-assemble-parametric-selection.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-assemble.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-back-transform.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 12 | `gam` | File setup or helper | `formula` | `` |
| 28 | `lm` | issue 412 uses predictor coordinates rather than partial effects | `.partial ~ .value` | `` |
| 398 | `mgcv::gam` | raw grids work across smooth classes and fitters | `f` | `` |
| 399 | `mgcv::bam` | raw grids work across smooth classes and fitters | `f` | `` |
| 469 | `mgcv::gam` | drawing uses envir for recovered smooth observations | `y ~ s(foo(x), k = 6)` | `` |
| 470 | `mgcv::gam` | drawing uses envir for recovered smooth observations | `y ~ foo(x)` | `` |
| 527 | `mgcv::gam` | supplied raw grids do not require unavailable observation decorations | `y ~ s(log(x), k = 6)` | `` |
| 528 | `mgcv::gam` | supplied raw grids do not require unavailable observation decorations | `y ~ log(x)` | `` |
| 576 | `mgcv::gam` | transformed soap boundaries cannot silently lose their coordinates | `y ~ s(log(x), w, k = 30, bs = "so", xt = list(bnd = bnd, nmax = 60))` | `` |
| 596 | `mgcv::gam` | transformed soap boundaries cannot silently lose their coordinates | `y ~ s(v, w, k = 30, bs = "so", xt = list(bnd = bnd, nmax = 60))` | `` |

## tests/testthat/test-bam-ar.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-bam-dependence.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 18 | `mgcv::bam` | File setup or helper | `y ~ s(x, k = 5)` | `spec$family` |

## tests/testthat/test-bam-fixed-offsets.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 22 | `mgcv::bam` | File setup or helper | `f` | `if (poisson) stats::poisson() else stats::gaussian()` |
| 68 | `mgcv::bam` | BAM diagnostic offset restoration uses block prediction | `y ~ s(x, k = 5)` | `poisson()` |

## tests/testthat/test-basis-factor-by.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 13 | `mgcv::gam` | unweighted factor-by bases draw once in one to four dimensions | `stats::as.formula(paste("y ~ f +", term))` | `` |
| 88 | `mgcv::gam` | shared basis groups survive selection, subsetting and serialization | `y ~ f + s(x, by = f, k = 5) + s(z, by = f, k = 5)` | `` |
| 121 | `mgcv::gam` | unweighted basis options reach wrappers and configured workers | `y ~ f + te(w, x, z, d = c(1, 2), k = c(3, 5), by = f)` | `` |

## tests/testthat/test-basis-multivariate.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 22 | `mgcv::gam` | File setup or helper | `stats::as.formula(paste("y ~", term))` | `` |
| 35 | `mgcv::gam` | basis surfaces draw contours by default and allow opting out | `y ~ s(x, z, k = 10)` | `` |
| 204 | `mgcv::gam` | basis variable validation matches other smooth methods | `y ~ s(x, k = 4) + s(z, k = 4)` | `` |
| 272 | `mgcv::gam` | factor conditioning and spherical axes follow existing restrictions | `y ~ te(x, z, w, f, d = c(2, 1, 1), bs = c("tp", "cr", "re"), k = c(5, 3, 3))` | `` |
| 281 | `mgcv::gam` | factor conditioning and spherical axes follow existing restrictions | `y ~ s(x, z, bs = "sos", k = 8)` | `` |

## tests/testthat/test-basis-size.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-basis.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-bayesplot.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 14 | `mgcv::gam` | File setup or helper | `y ~ x` | `mgcv::ziP()` |
| 38 | `mgcv::gam` | binomial checks use success counts and fitted trial counts | `cbind(success, trials - success) ~ x` | `stats::binomial()` |
| 44 | `mgcv::gam` | binomial checks use success counts and fitted trial counts | `cbind(success, trials - success) ~ x` | `stats::binomial()` |
| 49 | `mgcv::gam` | binomial checks use success counts and fitted trial counts | `I(success/trials) ~ x` | `stats::binomial()` |
| 53 | `mgcv::gam` | binomial checks use success counts and fitted trial counts | `outcome ~ x` | `stats::binomial()` |
| 65 | `mgcv::gam` | bayesplot removes excluded rows from all inputs together | `y ~ x + group` | `` |
| 89 | `mgcv::gamm` | full mixed binomial checks retain actual trials with frequency weights | `cbind(success, trials - success) ~ s(x, k = 4)` | `stats::binomial()` |
| 97 | `gamm4::gamm4` | full mixed binomial checks retain actual trials with frequency weights | `cbind(success, trials - success) ~ s(x, k = 4)` | `stats::binomial()` |
| 150 | `mgcv::gam` | grouped selections retain component numbers and evaluate only their means | `cbind(y, component) ~ x` | `mgcv::gfam(list(normal, poisson()))` |

## tests/testthat/test-by-variables.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-censored-families.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 102 | `mgcv::gam` | fitted censored GAMs use estimated theta and preserve the seed | `y ~ s(x, k = 5)` | `fam` |

## tests/testthat/test-compare-smooths-unconditional.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 5 | `gam` | compare smooths unconditional works | `y ~ s(x, k = 8) + s(z, k = 8)` | `` |
| 6 | `gam` | compare smooths unconditional works | `y ~ s(x, k = 6) + s(z, k = 6)` | `` |

## tests/testthat/test-compare-smooths.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-concurvity.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-conditional-differences-simultaneous.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 136 | `gam` | deterministic differences collapse and inconsistent draws fail | `y ~ s(x, k = 5) + s(a, bs = "re")` | `` |
| 167 | `gam` | joint groups and incomplete strata retain coverage membership | `y ~ a + b + s(x, by = a, k = 5)` | `` |

## tests/testthat/test-conditional-differences.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 105 | `gam` | incomplete factor grids compare only shared strata | `y ~ a + b + s(x, k = 5)` | `` |
| 116 | `gam` | incomplete factor grids compare only shared strata | `y ~ a + s(x, by = b, k = 5)` | `` |
| 142 | `bam` | bam, MH samples, and plotting are supported | `y ~ a + s(x, by = a, k = 5)` | `` |
| 179 | `gam` | invalid inputs fail explicitly | `list(y ~ a + x, ~1)` | `gaulss()` |
| 188 | `gam` | random smooths and random intercepts can be explicitly excluded | `y ~ a + s(x, by = a, k = 5) + s(x, subject, bs = "fs", k = 4) + s(word, bs = "re")` | `` |

## tests/testthat/test-conditional-surfaces.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 169 | `mgcv::gam` | multiple distribution parameters have separate surface panels | `list(y ~ x + z, ~x)` | `mgcv::gaulss()` |
| 183 | `mgcv::gam` | observed combinations and nearby numeric facets stay separate | `y ~ a + b + s(x, z, by = a, k = 8) + s(u, k = 4)` | `` |

## tests/testthat/test-conditional-values-complete.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 49 | `gam` | only conditioned factors constrain observed combinations | `y ~ x` | `` |

## tests/testthat/test-conditional-values.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-confint-methods.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 141 | `mgcv::gam` | shift adds only the intercept to factor-by tensor product smooths | `y ~ te(x0, x1, by = f) + f` | `` |
| 268 | `mgcv::gam` | confint uses ordered bounds for a decreasing family inverse link | `y ~ s(x, k = 5)` | `Gamma(link = "inverse")` |

## tests/testthat/test-confint-simultaneous-uncertainty.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-cyclic-polar.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 83 | `mgcv::gam` | only eligible one dimensional smooths become polar | `y ~ s(x, bs = "cc", k = 6) + s(z, k = 5)` | `` |
| 90 | `mgcv::gam` | only eligible one dimensional smooths become polar | `y ~ te(x, z, bs = c("cc", "cr"), k = c(4, 4))` | `` |
| 99 | `mgcv::gam` | one dimensional tensors and positive cyclic SCAM smooths draw | `y ~ t2(x, bs = "cc", k = 7)` | `` |
| 105 | `scam::scam` | one dimensional tensors and positive cyclic SCAM smooths draw | `y ~ s(x, bs = "cpop", k = 7)` | `` |
| 115 | `mgcv::gam` | grouped cyclic curves require identical boundaries | `y ~ f + s(x, by = f, bs = "cc", k = 6)` | `` |
| 159 | `mgcv::gam` | transformed cyclic coordinates reject raw polar axes | `y ~ s(log(u), bs = "cc", k = 7)` | `` |
| 173 | `mgcv::gam` | fixed scales share the radial effect range | `y ~ s(x, bs = "cc", k = 6) + s(z, bs = "cc", k = 5)` | `` |
| 184 | `mgcv::gamm` | gamm and gamm4 wrappers forward cyclic controls | `y ~ s(x, bs = "cc", k = 6)` | `` |
| 189 | `gamm4::gamm4` | gamm and gamm4 wrappers forward cyclic controls | `y ~ s(x, bs = "cc", k = 6)` | `` |
| 198 | `glmmTMB::glmmTMB` | glmmTMB forwards cyclic metadata and plot controls | `y ~ s(x, bs = "cc", k = 6)` | `` |
| 210 | `sdmTMB::sdmTMB` | sdmTMB forwards cyclic metadata and plot controls | `y ~ s(x, bs = "cc", k = 6)` | `` |
| 226 | `GJRM::gamlss` | GJRM equation metadata distinguishes repeated cyclic smooths | `list(y ~ s(x, bs = "cc", k = 5), ~s(x, bs = "cc", k = 5))` | `` |
| 274 | `mgcv::gam` | supplied transformed coordinates cannot request raw polar axes | `y ~ s(log(u), bs = "cc", k = 7)` | `` |
| 290 | `mgcv::gam` | cyclic metadata distinguishes components sharing a label | `formula` | `` |
| 307 | `mgcv::gam` | duplicate cyclic tensor labels retain separate cycle boundaries | `y ~ te(x, bs = "cc", k = 7) + te(x, bs = "cp", k = 8)` | `` |
| 327 | `mgcv::gam` | one dimensional cyclic P spline tensor constructors draw | `formula` | `` |

## tests/testthat/test-data-combos-complete.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-data-sim-aliases.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-data-sim-families.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-data-sim-ocat.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-data-sim-shapes.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 43 | `mgcv::scasm` | paste("scasm reference recipe fits and enforces", shape) | `form` | `` |
| 55 | `mgcv::scasm` | documented univariate scasm recipes fit and enforce their shapes | `y ~ s(x, bs = "sc", xt = "m+", k = 8)` | `` |
| 60 | `mgcv::scasm` | documented univariate scasm recipes fit and enforce their shapes | `y ~ s(x, bs = "sc", xt = "+", k = 8) - 1` | `` |
| 70 | `mgcv::scasm` | documented additive, tensor and location-scale scasm recipes are usable | `y ~ s(x0, k = 6) + s(x1, bs = "sc", xt = "m+", k = 6) + s(x2, k = 6) + s(x3, bs = "sc", xt = "c+", k = 6)` | `stats::poisson()` |
| 75 | `mgcv::scasm` | documented additive, tensor and location-scale scasm recipes are usable | `y ~ te(x, z, bs = c("sc", "sc"), xt = list("m+", "m+"), k = c(6, 6))` | `` |
| 80 | `mgcv::scasm` | documented additive, tensor and location-scale scasm recipes are usable | `list(y ~ s(x, bs = "sc", xt = "c-"), ~s(z, bs = "sc", xt = "m+"))` | `mgcv::gammals()` |
| 90 | `mgcv::scasm` | native scasm bootstrap draws respect the reference constraints | `y ~ s(x, bs = "sc", xt = "m+", k = 6)` | `` |

## tests/testthat/test-data-sim.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-data-slice-by.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 101 | `mgcv::gam` | GAM slices recover grouping columns and respect supplied data | `y ~ fac + s(x, by = fac, k = 4) + z` | `` |
| 134 | `mgcv::gam` | grouped GAM slices recover raw transformed covariates | `y ~ fac + s(log(x), by = fac, k = 4)` | `` |
| 146 | `mgcv::gam` | GAM observed-only filtering is scoped to groups | `y ~ fac + x` | `` |

## tests/testthat/test-data-slice.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-datagen.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-dependence-core.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-derivative-step.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 7 | `mgcv::gam` | automatic response differences agree with analytical derivatives | `y ~ I(x/units) + I((x/units)^2)` | `poisson()` |
| 33 | `mgcv::gam` | step selection is independent of prediction batching and origin | `y ~ x + I(x^2)` | `` |
| 58 | `mgcv::gam` | explicit steps retain their absolute meaning | `y ~ x + I(x^2)` | `poisson()` |
| 76 | `mgcv::gam` | large coordinate offsets use representable increments | `y ~ I(x - origin) + I((x - origin)^2)` | `` |
| 89 | `mgcv::gam` | smooth derivatives choose steps on the requested scale | `y ~ s(log(x), k = 5)` | `` |
| 99 | `mgcv::gam` | constant and missing training coordinates have safe fallbacks | `y ~ x` | `` |
| 112 | `mgcv::gam` | second response derivatives tolerate small fitting differences | `y ~ s(x0) + s(x1) + s(x2) + s(x3)` | `mgcv::nb()` |

## tests/testthat/test-derivatives-numeric-by.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 5 | `mgcv::gam` | numeric-by derivatives hold the multiplier fixed | `y ~ s(x, by = z, k = 8) + s(w, k = 6)` | `` |

## tests/testthat/test-derivatives.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 408 | `gam` | derivatives() works for factor by smooths issue 47 | `y ~ x1 + s(x2) + fac + s(x0, by = fac)` | `` |
| 419 | `gam` | derivatives() works for factor by smooths issue 47 | `y ~ x1 + s(x2) + ofac + s(x0) + s(x0, by = ofac)` | `` |
| 429 | `gamm` | derivatives() works for factor by smooths issue 47 | `y ~ x1 + s(x2) + fac + s(x0, by = fac)` | `` |
| 465 | `gam` | derivatives() works for fs smooths issue 57 and 301 | `y_obs ~ s(t, unit, k = 5, bs = "fs", m = 2)` | `` |
| 476 | `gam` | derivatives() works for fs smooths issue 57 and 301 | `y_obs ~ s(t, unit, k = 5, bs = "fs", m = 2) + linearterm` | `` |
| 558 | `gam` | derivatives gives nice alerts for multivariate smooths | `y ~ s(x0) + s(x1, x2)` | `` |
| 561 | `gam` | derivatives gives nice alerts for multivariate smooths | `y ~ s(x1, x2)` | `` |

## tests/testthat/test-dharma-residuals.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 139 | `glm` | missing rows are restored once and binomial encodings agree | `cbind(y, n - y) ~ x` | `binomial()` |
| 140 | `glm` | missing rows are restored once and binomial encodings agree | `I(y/n) ~ x` | `binomial()` |
| 147 | `update` | missing rows are restored once and binomial encodings agree | `m` | `` |
| 150 | `glm` | missing rows are restored once and binomial encodings agree | `binary ~ x` | `binomial()` |
| 163 | `lm` | generators need neither CDFs nor response moments | `dist ~ speed` | `` |
| 207 | `glmmTMB::glmmTMB` | glmmTMB adapters use DHARMa residuals before conventional calculations | `y ~ x + (1 \| g)` | `poisson()` |
| 224 | `mgcv::gamm` | gamm and native mixed-model adapters retain their conditioning defaults | `y ~ s(x, k = 5)` | `` |
| 225 | `lme4::lmer` | gamm and native mixed-model adapters retain their conditioning defaults | `y ~ x + (1 \| g)` | `` |
| 282 | `sdmTMB::sdmTMB` | sdmTMB component and field options reach the response generator | `y ~ x` | `sdmTMB::delta_gamma()` |
| 303 | `GJRM::gamlss` | GJRM diagnostics use the final fitted response distribution | `list(y ~ x, ~1)` | `"N"` |
| 372 | `glmmTMB::glmmTMB` | DHARMa appraisal retains conditioning for posterior predictive panels | `y ~ x + (1 \| g)` | `poisson()` |

## tests/testthat/test-diagnostic-core.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 40 | `lm` | LM and GLM dispersion and seeded references agree across entry points | `y ~ x` | `` |
| 41 | `glm` | LM and GLM dispersion and seeded references agree across entry points | `y ~ x` | `Gamma(link = "log")` |
| 60 | `glm` | calibrated PIT QQ and worm bands use a uniform reference | `y ~ x` | `poisson()` |
| 78 | `lm` | new LM calibrated residuals restore exclusions and preserve RNG absence | `y ~ x` | `` |
| 94 | `mgcv::gam` | multinomial native residual layouts match simulated references | `list(y ~ x, ~x)` | `mgcv::multinom(K = 2)` |
| 128 | `mgcv::bam` | BAM AR1 simulation and calibrated bands retain residual dependence | `y ~ s(x, k = 5)` | `` |
| 143 | `lm` | prepared diagnostic sources preserve original fitting-row identities | `y ~ x` | `` |

## tests/testthat/test-diagnostic-method.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 10 | `mgcv::gam` | Tweedie diagnostics default to simulation without computing quantiles | `y ~ x` | `fam` |
| 41 | `mgcv::gam` | automatic methods preserve other families and availability fallbacks | `y ~ x` | `` |

## tests/testthat/test-diagnostic-plot-labels.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 27 | `nlme::lme` | diagnostic captions can be replaced or omitted | `distance ~ age` | `` |

## tests/testthat/test-diagnostic-plots.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-diagnostic-posterior.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 5 | `lm` | ordinary posterior diagnostics respect weights, trials and offsets | `y ~ x + offset(off)` | `` |
| 15 | `glm` | ordinary posterior diagnostics respect weights, trials and offsets | `cbind(success, failure) ~ x + offset(off)` | `binomial()` |
| 20 | `glm` | ordinary posterior diagnostics respect weights, trials and offsets | `success ~ x` | `quasipoisson()` |
| 82 | `glmmTMB::glmmTMB` | joint glmmTMB posterior diagnostics include all predictors | `y ~ x + (1 \| group)` | `poisson()` |
| 115 | `GJRM::gamlss` | GJRM posterior responses draw final mean and scale coefficients jointly | `if (fam == "P") list(y ~ x) else list(y ~ x, ~x)` | `fam` |
| 124 | `GJRM::gamlss` | GJRM posterior responses draw final mean and scale coefficients jointly | `list(y ~ x)` | `"N"` |
| 135 | `gamm4::gamm4` | gamm4 full posterior diagnostics retain joint effect uncertainty | `y ~ s(x, k = 5)` | `` |
| 151 | `mgcv::gam` | GAM posterior checks retain fitting-argument offsets | `y ~ s(x, k = 5)` | `` |
| 211 | `sdmTMB::sdmTMB` | delta posterior checks retain selected components and rows | `y ~ x` | `sdmTMB::delta_gamma()` |

## tests/testthat/test-diagnostic-review.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 4 | `lm` | ordinary-model residual and scalable APIs reject unsupported conditioning | `y ~ x` | `` |
| 4 | `glm` | ordinary-model residual and scalable APIs reject unsupported conditioning | `y ~ x` | `` |
| 5 | `mgcv::gam` | ordinary-model residual and scalable APIs reject unsupported conditioning | `y ~ s(x, k = 5)` | `` |
| 22 | `glmmTMB::glmmTMB` | glmmTMB proxy residual APIs honor conditioning overrides | `y ~ 1 + (1 \| g)` | `poisson()` |
| 34 | `glmmTMB::glmmTMB` | appraisal retains distribution checks when a response mean is undefined | `y ~ 1 + (1 \| g)` | `glmmTMB::t_family()` |
| 52 | `sdmTMB::sdmTMB` | prepared field diagnostics use requested conventions in every plot path | `y ~ 1 + (1 \| group)` | `gaussian()` |
| 73 | `mgcv::gam` | gaulss diagnostic references follow the fitted likelihood weight semantics | `list(y ~ 1, ~1)` | `mgcv::gaulss()` |
| 74 | `mgcv::gam` | gaulss diagnostic references follow the fitted likelihood weight semantics | `list(y ~ 1, ~1)` | `mgcv::gaulss()` |

## tests/testthat/test-diagnostic-weights.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 8 | `lm` | diagnostics preserve base lm weights and missing rows | `y ~ x` | `` |
| 9 | `lm` | diagnostics preserve base lm weights and missing rows | `y ~ x` | `` |
| 10 | `lm` | diagnostics preserve base lm weights and missing rows | `y ~ x` | `` |
| 11 | `lm` | diagnostics preserve base lm weights and missing rows | `y ~ x` | `` |
| 29 | `glm` | diagnostics preserve base lm weights and missing rows | `y ~ x` | `` |

## tests/testthat/test-difference-smooths-alignment.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 8 | `mgcv::gam` | smooth differences align covariates across factor levels | `y ~ group + s(x, by = group, k = 6) + s(w, k = 5)` | `` |
| 10 | `mgcv::gam` | smooth differences align covariates across factor levels | `y ~ group + te(x, z, by = group, k = c(4, 4)) + s(w, k = 5)` | `` |

## tests/testthat/test-difference-smooths-simultaneous.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 35 | `mgcv::gam` | File setup or helper | `formula` | `` |
| 134 | `do.call` | simultaneous differences delegate through BAM and GAMM methods | `difference_smooths.gam` | `` |

## tests/testthat/test-difference-smooths.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 139 | `gam` | difference smooths works with a decomposed model issue 223 | `Petal.Width ~ Species + s(Sepal.Length, k = 4) + s(Sepal.Length, by = Species, k = 4) + s(Sepal.Width, k = 4) + s(Sepal.Width,      by = Species, k = 4) + ti(Sepal.Length, Sepal.Width, k = 4) + ti(Sepal.Length, Sepal.Width, by = Species, k = 4)` | `` |

## tests/testthat/test-discrete-offset-rows.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 9 | `mgcv::bam` | discrete fitted intervals restore formula-offset missing rows once | `y ~ s(x, k = 5) + offset(off)` | `poisson()` |
| 41 | `mgcv::bam` | discrete prediction accepts stored frames and explicit tibbles | `y ~ s(x, k = 5)` | `poisson()` |
| 43 | `mgcv::bam` | discrete prediction accepts stored frames and explicit tibbles | `y ~ s(x, k = 5)` | `poisson()` |

## tests/testthat/test-distribution-helpers.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-distributional-fitted-samples.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 14 | `mgcv::gam` | multinomial fitted draws honour scale and each predictor's offset | `list(y ~ x + offset(off), ~z + offset(off2))` | `mgcv::multinom(K = 2)` |
| 53 | `mgcv::gam` | one-predictor multinomial draws include the reference category | `list(y ~ x + offset(off))` | `mgcv::multinom(K = 1)` |
| 75 | `mgcv::gam` | Gaussian location-scale fitted draws retain their mean interpretation | `list(y ~ x + offset(off), ~z + offset(off2))` | `mgcv::gaulss(link = list(location_link, "logb"))` |
| 100 | `mgcv::gam` | zero-inflated Poisson fitted draws use both predictors for the mean | `list(y ~ x + offset(off), ~z + offset(off2))` | `mgcv::ziplss()` |

## tests/testthat/test-draw-core.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 45 | `gam` | ordered factor labels and grouped panels survive drawing | `y ~ f + s(x, k = 4) + s(x, by = f, k = 4)` | `` |
| 62 | `gam` | parametric drawing retains values and validation | `y ~ x + f + s(z, k = 4)` | `` |
| 76 | `gam` | small four dimensional smooths can be evaluated and drawn | `y ~ te(x, z, u, v, k = c(3, 3, 3, 3))` | `` |
| 86 | `gam` | small tensor random effects retain curves and surfaces | `y ~ s(z, k = 4) + ti(x, f, bs = c("tp", "re"), k = c(4, 3))` | `` |
| 87 | `gam` | small tensor random effects retain curves and surfaces | `y ~ s(z, k = 4) + ti(x, z, f, bs = c("tp", "tp", "re"), k = c(4, 4, 3))` | `` |
| 99 | `bam` | small AR and matrix covariate models can be drawn | `y ~ s(x, k = 5)` | `` |
| 103 | `gam` | small AR and matrix covariate models can be drawn | `y ~ s(X, by = L, k = 5)` | `` |
| 111 | `gam` | small bivariate smooth differences preserve factor comparisons | `y ~ f + s(x, z, by = f, k = 8)` | `` |

## tests/testthat/test-draw-gam.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-draw-label-control.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 217 | `mgcv::gam` | model generics apply controls to every returned panel | `y ~ f + s(x, k = 5)` | `` |
| 269 | `mgcv::gam` | label controls work throughout the drawing methods | `y ~ f + s(x, by = f, k = 4) + s(z, k = 4)` | `` |

## tests/testthat/test-draw-methods.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 195 | `gam` | draw() can handle non-standard names -- a function call as a name | `y ~ s(log2(time))` | `` |
| 253 | `gam` | draw() works with parametric terms | `y ~ x0 + s(x1) + s(x2) + s(x3)` | `` |
| 273 | `gam` | draw() works with parametric terms | `y ~ x0 + s(x1) + s(x2) + s(x3)` | `` |
| 542 | `gam` | draw() can handle a mixture of numeric and factor random effects | `y ~ s(x2, fac, bs = "re")` | `` |
| 554 | `gam` | draw.gam uses fixed scales if asked for them: #73 | `y ~ s(x1) + s(x2) + ti(x1, x2)` | `` |

## tests/testthat/test-draw-parametric-effects.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 92 | `gam` | issue 45 parametric effects for lss models remains fixed | `list(y ~ s(x2) + x3, ~s(x0) + x1)` | `ziplss()` |
| 105 | `gam` | issue 45 parametric effects for lss models remains fixed | `list(y ~ s(x2) + x4, ~s(x0) + x1)` | `ziplss()` |
| 118 | `gam` | issue 45 parametric effects for lss models remains fixed | `list(y ~ s(x2) + x3 + x4, ~s(x0) + x1)` | `ziplss()` |
| 170 | `gam` | parametric effects works with missing data in model fit | `response ~ treatment + s(time, by = treatment, k = 5) + s(subject, bs = "re")` | `` |

## tests/testthat/test-draw-sampling-diagnostics.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 108 | `mgcv::gam` | derived samples retain original coefficient traces without changing draws | `y ~ s(x, k = 4)` | `` |

## tests/testthat/test-draw-smooth-estimates.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 189 | `gam` | draw.smooth_estimates works for trivar te with a 2d marginal | `y ~ te(x0, x1, x2, k = c(3, 9), d = c(1, 2), bs = c("cr", "ds"))` | `` |
| 216 | `bam` | draw.smooth_estimates works for quadvar te with 2d marginals | `y ~ te(x0, x1, x2, x3, k = c(3, 3, 3, 3), d = c(2, 2), bs = c("ds", "ds"))` | `` |
| 247 | `gam` | draw.smooth_estimates works for trivar t2 with a 2d marginal | `y ~ t2(x0, x1, x2, k = c(3, 9), d = c(1, 2), bs = c("cr", "ds"))` | `` |
| 273 | `bam` | draw.smooth_estimates works for quadvar t2 with 2d marginals | `y ~ t2(x0, x1, x2, x3, k = c(3, 3, 3, 3), d = c(2, 2), bs = c("ds", "ds"))` | `` |
| 306 | `bam` | draw.gam works with sos spline chlorophyll a | `chl ~ s(lat, lon, bs = "sos")` | `` |

## tests/testthat/test-draw-tensor-factors.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 13 | `gam` | two-factor tensors show every combination with its uncertainty | `form` | `` |
| 49 | `gam` | mixed tensor margins use the continuous covariate on x in either order | `as.formula(paste0("y ~ ", constructor, "(", term, ")"))` | `` |
| 75 | `gam` | three-variable T2 terms retain surfaces faceted by a factor | `y ~ t2(week, latitude, species, bs = c("cc", "tp", "re"), k = c(4, 4, 3), full = full)` | `` |

## tests/testthat/test-draw-time-varying.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-draw-variance-components.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 110 | `gam` | proportion plots preserve signed estimates, bounds and total selection | `y ~ total + z` | `` |
| 138 | `gam` | proportion panels contain the correct shared terms and totals | `list(y ~ x, ~z, 1 + 2 ~ 0 + s(w, k = 6))` | `gaulss()` |
| 181 | `gam` | missing intervals and undefined proportions retain component labels | `list(y ~ x, ~1)` | `gaulss()` |

## tests/testthat/test-edf.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-effects.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-evaluate-parametric-terms.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 5 | `gam` | File setup or helper | `vocab ~ nativeBorn * ageGroup` | `` |
| 60 | `gam` | File setup or helper | `y ~ x0 + s(x1) + s(x2) + s(x3)` | `` |

## tests/testthat/test-evenly.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-expression-wrappers.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 8 | `mgcv::gam` | File setup or helper | `y ~ s(audit_smooth_expression(x), k = 5) + offset(audit_offset_expression(exposure))` | `` |
| 13 | `mgcv::gam` | File setup or helper | `y ~ s(coordinate, k = 5) + offset(off)` | `` |
| 120 | `scam::scam` | SCAM simulation forwards expression environments | `y ~ s(audit_smooth_expression(x), k = 5, bs = "mpi") + offset(audit_offset_expression(exposure))` | `` |

## tests/testthat/test-factor-smooth-controls.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 12 | `mgcv::gam` | File setup or helper | `y ~ s(w, k = 4) + s(x, by = f, k = 4) + s(z, f, bs = "fs", k = 4) + s(w, g, bs = "sz", k = 4)` | `` |
| 14 | `mgcv::gam` | File setup or helper | `y ~ s(x, k = 4) + s(x, by = o, k = 4)` | `` |
| 15 | `mgcv::gam` | File setup or helper | `y ~ s(x, k = 4) + s(x, by = o, k = 4) + s(z, by = f, k = 4)` | `` |
| 17 | `mgcv::gam` | File setup or helper | `y ~ s(x, k = 4) + s(x, f, bs = "sz", k = 4) + s(x, g, bs = "sz", k = 4) + s(x, f, g, bs = "sz", k = 4)` | `` |
| 202 | `mgcv::gam` | ordered deviations keep their fitted levels and reference smooth separate | `y ~ s(x, k = 4) + s(x, by = o, k = 4)` | `` |
| 337 | `mgcv::gam` | factor-by titles preserve colons in covariates and custom labels | `y ~ f + s(I(x + nchar(":f")), by = f, k = 4)` | `` |

## tests/testthat/test-factor-support.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 47 | `do.call` | pointwise and simultaneous predictions retain their joint calculations | `fitted_values` | `` |

## tests/testthat/test-family-diagnostics.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 38 | `mgcv::gam` | factories receive compact fitted parameters and correct diagnostic panel means | `y ~ x` | `f` |
| 120 | `mgcv::gam` | traditional reference residuals apply an explicitly supplied observation process | `y ~ 1` | `` |
| 178 | `mgcv::gam` | matrix response moments define component plots for unfamiliar families | `list(cbind(y1, y2) ~ 1, ~1)` | `mgcv::mvn(2)` |

## tests/testthat/test-family-mh.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 68 | `mgcv::gam` | extended families and alternative links use their fitted evaluators | `y ~ s(x, k = 5) + offset(off)` | `families[[i]]` |
| 83 | `mgcv::gam` | censoring and excluded rows survive family setup | `y ~ s(x, k = 5) + offset(off)` | `mgcv::cnorm()` |
| 99 | `mgcv::gam` | multiple predictors retain fitting offsets and weights | `list(y ~ s(x, k = 5) + offset(off), ~s(z, k = 5) + offset(off))` | `mgcv::gaulss()` |
| 172 | `mgcv::gam` | family MH reproduces analytic Gaussian and Poisson posteriors | `y ~ 1` | `` |
| 178 | `mgcv::gam` | family MH reproduces analytic Gaussian and Poisson posteriors | `y ~ 1` | `poisson()` |
| 226 | `mgcv::gam` | family penalties retain shared smoothing and tensor terms | `formula` | `poisson()` |
| 230 | `mgcv::gam` | family penalties retain shared smoothing and tensor terms | `y ~ s(x, k = 5)` | `poisson()` |
| 238 | `mgcv::gam` | family callbacks preserve binomial encodings | `cbind(success, 5 - success) ~ s(x, k = 5)` | `binomial()` |
| 242 | `mgcv::gam` | family callbacks preserve binomial encodings | `y ~ s(x, k = 5)` | `binomial()` |
| 255 | `mgcv::gam` | shared predictors and multivariate covariance coefficients keep their mapping | `formula` | `family` |
| 267 | `mgcv::gam` | family callbacks reject proposals outside a link's domain | `y ~ x` | `poisson(link = "identity")` |

## tests/testthat/test-family-quantiles.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 146 | `mgcv::gam` | uniform QQ and worm plots use the extended quantile helpers | `formula` | `case$fam` |

## tests/testthat/test-family-utils.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-fderiv.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 3 | `gam` | File setup or helper | `y ~ s(x0) + s(x1) + fac` | `` |
| 36 | `gam` | fderiv() can handle offsets | `y ~ fac + s(x1) + offset(x0)` | `` |

## tests/testthat/test-fitted-parameters.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 230 | `mgcv::gam` | category expectation derivatives retain category identifiers | `list(y ~ x, ~x)` | `mgcv::multinom(K = 2)` |

## tests/testthat/test-fitted-simultaneous.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 19 | `mgcv::gam` | fitted simultaneous intervals cover arbitrary factor combinations | `y ~ fac + s(x, k = 6)` | `` |
| 23 | `do.call` | fitted simultaneous intervals cover arbitrary factor combinations | `fitted_joint_reference` | `` |
| 25 | `do.call` | fitted simultaneous intervals cover arbitrary factor combinations | `fitted_values` | `` |
| 51 | `mgcv::gam` | response intervals preserve offsets and monotone link transformations | `y ~ s(log(x), k = 5) + offset(log(exposure))` | `poisson()` |
| 68 | `mgcv::gam` | response intervals preserve offsets and monotone link transformations | `y ~ s(x, k = 5)` | `Gamma()` |
| 87 | `mgcv::gam` | simultaneous fitted values restore missing rows and preserve RNG | `y ~ s(x, k = 5)` | `` |
| 118 | `mgcv::bam` | BAM, GAMM and covariance fallback use the same joint calculation | `formula(m_gam)` | `` |
| 151 | `mgcv::gam` | supported extended families use their joint coefficient covariance | `y ~ s(x, k = 5)` | `fam` |
| 169 | `mgcv::gam` | excluded random effects and model environments share prediction semantics | `y ~ s(transform(x), k = 5) + s(g, bs = "re")` | `` |
| 185 | `mgcv::gam` | excluded random effects and model environments share prediction semantics | `y ~ s(x, k = 5)` | `` |

## tests/testthat/test-fitted-values.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 187 | `mgcv::gam` | fitted intervals respect increasing and decreasing inverse links | `y ~ s(x, k = 5)` | `Gamma(link = link_name)` |

## tests/testthat/test-fitting-offsets.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 16 | `mgcv::bam` | fitting-argument offsets survive ordinary bam storage and row restoration | `f` | `poisson()` |
| 54 | `gamm4::gamm4` | gamm4 GAM samples recover argument offsets without group effects | `f` | `poisson()` |

## tests/testthat/test-fix-family.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-fixed-effects-smooths.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 8 | `gam` | fixed_effects() excludes smooth coefficients by index | `y ~ linear + te(x, z, k = c(4, 4)) + ti(u, v, k = c(4, 4)) + t2(w, q, k = c(4, 4)) + s(r, k = 5)` | `` |
| 13 | `gam` | fixed_effects() excludes smooth coefficients by index | `y ~ linear + f + s(x, by = f, k = 5)` | `` |
| 14 | `gam` | fixed_effects() excludes smooth coefficients by index | `y ~ 0 + linear + te(x, z, k = c(4, 4))` | `` |
| 15 | `gam` | fixed_effects() excludes smooth coefficients by index | `y ~ linear` | `` |
| 16 | `gam` | fixed_effects() excludes smooth coefficients by index | `y ~ 0 + s(x, k = 5)` | `` |

## tests/testthat/test-gamm-forwarding.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-gamm4-conditioning-default.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-gamm4.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-gfam-context.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-gfam-diagnostics.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-gfam-fitted-samples.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 120 | `do.call` | seeded Gaussian and MH paths preserve joint draws and caller RNG | `fitted_samples` | `` |
| 121 | `do.call` | seeded Gaussian and MH paths preserve joint draws and caller RNG | `fitted_samples` | `` |
| 124 | `do.call` | seeded Gaussian and MH paths preserve joint draws and caller RNG | `fitted_samples` | `` |

## tests/testthat/test-gfam-quantile-diagnostics.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-gfam-response-selection.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-gfam-simulate.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 315 | `mgcv::gam` | grouped simulation evaluates transformed predictors and offsets | `cbind(y, family_id) ~ s(log(x), k = 5) + offset(log(exp(off)))` | `mgcv::gfam(list(binomial(), gaussian(), poisson()))` |

## tests/testthat/test-gfam.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 9 | `gam` | gfam data sim and model works | `cbind(y, index) ~ s(x0) + s(x1) + s(x2) + s(x3)` | `gfam(list(binomial, tw, gaussian))` |
| 36 | `gam` | gratia can handle a two species negbin model with gfam | `cbind(y, spp) ~ f_spp + s(x, by = f_spp)` | `gfam(list("nb", "nb"))` |

## tests/testthat/test-gjrm-diagnostics-families.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 47 | `GJRM::gamlss` | File setup or helper | `formulas` | `family` |

## tests/testthat/test-gjrm-diagnostics-review.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-gjrm-indexing-review.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 47 | `do.call` | File setup or helper | `fit` | `` |

## tests/testthat/test-gjrm-joint-adapter.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-gjrm-joint-diagnostics.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-gjrm-joint-smooths.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-gjrm-joint-tensor.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-gjrm-methods.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 26 | `GJRM::gamlss` | draw works for a simple GJRM gamlss | `fl` | `` |
| 40 | `GJRM::gamlss` | GJRM layouts count smooth and parametric panels when rendering | `list(y1 ~ x1 + s(x2), ~s(x3, k = 6))` | `` |

## tests/testthat/test-gjrm-plot-review.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 34 | `gam` | saved factor-by smooth groups keep ordinary GAM terms separate | `y ~ f + s(x, by = f, k = 4) + s(z, by = f, k = 4)` | `` |

## tests/testthat/test-gjrm-selection.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 14 | `GJRM::gamlss` | File setup or helper | `list(y ~ a + s(x, k = 4), second)` | `` |
| 93 | `GJRM::gamlss` | GJRM models without smooths can still draw parametric effects | `list(y ~ x, ~1)` | `` |
| 231 | `GJRM::gamlss` | GJRM estimation skips parametric equations without losing labels | `formula` | `` |
| 240 | `GJRM::gamlss` | GJRM estimation skips parametric equations without losing labels | `list(y ~ s(x, k = 4))` | `` |
| 243 | `GJRM::gamlss` | GJRM estimation skips parametric equations without losing labels | `list(y ~ s(x, k = 4) + s(z, k = 4), ~s(x, k = 4))` | `` |
| 263 | `GJRM::gamlss` | GJRM estimation forwards surface grids and coordinate controls | `list(y ~ s(fun(x), k = 4), ~s(z, a, k = 8))` | `` |

## tests/testthat/test-glmmtmb-dependence.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 15 | `glmmTMB::glmmTMB` | glmmTMB dependence follows fitted latent covariance and conditioning | `count ~ x` | `poisson()` |
| 16 | `glmmTMB::glmmTMB` | glmmTMB dependence follows fitted latent covariance and conditioning | `count ~ x + (1 \| id)` | `poisson()` |
| 18 | `glmmTMB::glmmTMB` | glmmTMB dependence follows fitted latent covariance and conditioning | `y ~ x + (1 + x \| g)` | `` |
| 48 | `glmmTMB::glmmTMB` | structured temporal covariance remains joint under marginal sampling | `y ~ ar1(time + 0 \| g)` | `` |
| 74 | `glmmTMB::glmmTMB` | native spatial and slope covariance are not replaced by marginal draws | `y ~ exp(position + 0 \| g)` | `` |
| 87 | `glmmTMB::glmmTMB` | nonlinear marginal residuals share one native integrated response law | `count ~ x + (1 \| g)` | `poisson()` |
| 116 | `glmmTMB::glmmTMB` | zero-inflation and dispersion shared effects count as dependence | `count ~ x` | `poisson()` |
| 120 | `glmmTMB::glmmTMB` | zero-inflation and dispersion shared effects count as dependence | `y ~ x` | `` |
| 134 | `glmmTMB::glmmTMB` | native marginal ordinal draws retain the observed category scale | `category ~ x + (1 \| g)` | `glmmTMB::ordinal()` |
| 150 | `glmmTMB::glmmTMB` | native nonGaussian temporal draws retain the latent correlation law | `y ~ ar1(time + 0 \| g)` | `poisson()` |
| 174 | `glmmTMB::glmmTMB` | zero-trial matrix rows have calibrated residuals without native response residuals | `cbind(success, failure) ~ x` | `binomial()` |
| 196 | `glmmTMB::glmmTMB` | Student-t population moment capabilities survive marginal calibration | `y ~ x + (1 \| g)` | `glmmTMB::t_family()` |
| 275 | `glmmTMB::glmmTMB` | prepared glmmTMB diagnostics honor conditioning changes | `y ~ 1 + (1 \| group)` | `poisson()` |

## tests/testthat/test-glmmtmb-effects.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 17 | `glmmTMB::glmmTMB` | glmmTMB parametric terms exclude smooth coefficients and recover term contributions | `y ~ x * a + log(z) + poly(w, 2) + s(z, k = 5) + (1 + x \| g)` | `` |
| 52 | `glmmTMB::glmmTMB` | glmmTMB parametric-only models and transformed prediction data work | `y ~ x + log(z) + poly(w, 2)` | `` |
| 70 | `glmmTMB::glmmTMB` | glmmTMB parametric-only models and transformed prediction data work | `y ~ 0 + x` | `` |
| 79 | `glmmTMB::glmmTMB` | glmmTMB effects retain rendering metadata across model parts | `y ~ a * b + log(z) + s(w, k = 5)` | `` |
| 105 | `glmmTMB::glmmTMB` | parametric labels and uncertainty follow each glmmTMB linear predictor | `y ~ x + s(w, k = 5)` | `` |
| 116 | `glmmTMB::glmmTMB` | parametric labels and uncertainty follow each glmmTMB linear predictor | `y ~ x` | `` |
| 126 | `glmmTMB::glmmTMB` | native random effects retain block ordering and exclude smooth blocks | `y ~ x + s(w, k = 5) + (1 \| h) + (1 + x \| g) + (0 + z \| g)` | `` |
| 148 | `glmmTMB::glmmTMB` | ranef_lines replaces only simple intercept-slope pairs with random contributions | `y ~ x + (1 + x \| g) + (1 \| h)` | `` |
| 190 | `glmmTMB::glmmTMB` | complex and separate random-effect terms retain QQ panels | `y ~ x * z + (1 + x + z + x:z \| g)` | `` |
| 196 | `glmmTMB::glmmTMB` | complex and separate random-effect terms retain QQ panels | `y ~ x + (1 \| g) + (0 + x \| g)` | `` |
| 201 | `glmmTMB::glmmTMB` | complex and separate random-effect terms retain QQ panels | `y ~ a + (1 + a \| g)` | `` |
| 215 | `glmmTMB::glmmTMB` | native and parametric terms are labelled across all three predictors | `y ~ x + (1 \| g)` | `glmmTMB::nbinom2()` |
| 241 | `glmmTMB::glmmTMB` | effect grids use fitting rows after subsets and missingness | `y ~ x + (1 + x \| g)` | `` |
| 254 | `glmmTMB::glmmTMB` | new glmmTMB effect plots have stable rendering | `y ~ x + s(w, k = 5) + (1 + x \| g)` | `` |

## tests/testthat/test-glmmtmb.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 9 | `glmmTMB::glmmTMB` | smooth coefficients reconstruct mixed model predictors in formula order | `f` | `` |
| 38 | `glmmTMB::glmmTMB` | joint covariance matches native ML and REML prediction intervals | `y ~ s(x, k = 6)` | `` |
| 57 | `glmmTMB::glmmTMB` | smooth plotting supports transformed, bivariate and numeric by smooths | `f` | `` |
| 62 | `glmmTMB::glmmTMB` | smooth plotting supports transformed, bivariate and numeric by smooths | `y ~ s(x, k = 6)` | `` |
| 68 | `glmmTMB::glmmTMB` | smooth plotting supports transformed, bivariate and numeric by smooths | `y ~ x` | `` |
| 78 | `glmmTMB::glmmTMB` | smooth counts exclude native random effects and parametric terms | `f` | `` |
| 82 | `glmmTMB::glmmTMB` | smooth counts exclude native random effects and parametric terms | `y ~ s(x, k = 6) + s(z, k = 5)` | `` |
| 92 | `glmmTMB::glmmTMB` | all linear predictors share mgcv-style smooth selection and labels | `y ~ s(x, k = 5)` | `poisson` |
| 115 | `glmmTMB::glmmTMB` | all linear predictors share mgcv-style smooth selection and labels | `y ~ 1` | `poisson` |
| 121 | `glmmTMB::glmmTMB` | all linear predictors share mgcv-style smooth selection and labels | `y ~ s(x, k = 5)` | `` |
| 131 | `mgcv::gam` | all linear predictors share mgcv-style smooth selection and labels | `list(y ~ s(x, k = 5), ~s(x, k = 4))` | `mgcv::gaulss()` |
| 140 | `glmmTMB::glmmTMB` | all linear predictors share mgcv-style smooth selection and labels | `y ~ 1` | `` |
| 185 | `glmmTMB::glmmTMB` | diagnostics align excluded rows and binomial counts | `y ~ x` | `` |
| 192 | `glmmTMB::glmmTMB` | diagnostics align excluded rows and binomial counts | `cbind(y, n - y) ~ x` | `binomial` |
| 244 | `glmmTMB::glmmTMB` | QQ simulation works across supported glmmTMB distributions | `y ~ x` | `spec$f` |
| 253 | `glmmTMB::glmmTMB` | QQ simulation works across supported glmmTMB distributions | `y ~ x` | `glmmTMB::nbinom2()` |
| 257 | `glmmTMB::glmmTMB` | QQ simulation works across supported glmmTMB distributions | `y ~ x` | `glmmTMB::ziGamma(link = "log")` |
| 263 | `glmmTMB::glmmTMB` | Gaussian smooths agree with mgcv and covariance failures are explicit | `y ~ s(x, k = 6)` | `` |
| 264 | `mgcv::gam` | Gaussian smooths agree with mgcv and covariance failures are explicit | `y ~ s(x, k = 6)` | `` |
| 289 | `glmmTMB::glmmTMB` | fitted Gamma dispersion is converted from sigma to shape | `y ~ 1` | `Gamma(link = "log")` |
| 307 | `glmmTMB::glmmTMB` | draw includes all three predictors in one model-wide layout | `y ~ s(x, k = 4)` | `glmmTMB::nbinom2()` |

## tests/testthat/test-grouped-histogram-outlines.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-hgam-paper.R

Keep the paper workflow opt-in. Its model-fitting steps are part of the workflow being validated.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 32 | `bam` | draw() can plot CO2 model 1 | `log(uptake) ~ s(log(conc), k = 5, bs = "tp") + s(Plant_uo, k = 12, bs = "re")` | `gaussian()` |
| 47 | `bam` | draw() can plot CO2 model 2 | `log(uptake) ~ s(log(conc), k = 5, m = 2) + s(log(conc), Plant_uo, k = 5, bs = "fs", m = 2)` | `gaussian()` |
| 64 | `bam` | draw() can plot CO2 model 3 | `log(uptake) ~ s(log(conc), k = 5, m = 2, bs = "tp") + s(log(conc), by = Plant_uo, k = 5, m = 1, bs = "tp") + s(Plant_uo,      bs = "re", k = 12)` | `` |
| 79 | `bam` | draw() can plot CO2 model 4 | `log(uptake) ~ s(log(conc), Plant_uo, k = 5, bs = "fs", m = 2)` | `` |
| 93 | `bam` | draw() can plot CO2 model 5 | `log(uptake) ~ s(log(conc), by = Plant_uo, k = 5, bs = "tp", m = 2) + s(Plant_uo, bs = "re", k = 12)` | `` |
| 111 | `bam` | draw() can plot bird_move model 1 | `count ~ te(week, latitude, bs = c("cc", "tp"), k = c(10, 10))` | `poisson()` |
| 129 | `gam` | draw() can plot bird_move model 2 | `count ~ te(week, latitude, bs = c("cc", "tp"), k = c(10, 10), m = 2) + t2(week, latitude, species, bs = c("cc", "tp", "re"),      k = c(10, 10, 6), m = 2, full = TRUE)` | `poisson()` |
| 150 | `bam` | draw() can plot bird_move model 3 | `count ~ species + te(week, latitude, bs = c("cc", "tp"), k = c(10, 10), m = 2) + te(week, latitude, by = species, bs = c("cc",      "tp"), k = c(10, 10), m = 1)` | `poisson()` |
| 171 | `bam` | draw() can plot bird_move model 4 | `count ~ t2(week, latitude, species, bs = c("cc", "tp", "re"), k = c(10, 10, 6), m = c(2, 2, 2))` | `poisson()` |
| 188 | `bam` | draw() can plot bird_move model 5 | `count ~ species + te(week, latitude, by = species, bs = c("cc", "tp"), k = c(10, 10), m = 2)` | `poisson()` |
| 204 | `bam` | draw() can plot zoo_comm_mod model 4 | `density_adj ~ s(day, taxon, bs = "fs", k = 10, xt = list(bs = "cc")) + s(taxon, year_f, bs = "re")` | `Gamma(link = "log")` |
| 223 | `bam` | draw() can plot zoo_comm_mod model 5 | `density_adj ~ s(day, by = taxon, k = 10, bs = "cc") + s(taxon, bs = "re") + s(taxon, year_f, bs = "re")` | `Gamma(link = "log")` |
| 250 | `gam` | gratia handles a complex gfam bird move | `cbind(count, fam) ~ te(week, latitude, bs = c("cc", "tp"), k = c(10, 10), m = 2) + t2(week, latitude, species, bs = c("cc",      "tp", "re"), k = c(10, 10, 6), m = 2, full = TRUE)` | `gfam(list(nb, nb, nb, nb, nb, nb))` |

## tests/testthat/test-inverse-gaussian.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 189 | `mgcv::gam` | inverse Gaussian fitted diagnostics use native CDF and quantiles | `y ~ x` | `stats::inverse.gaussian(link = "log")` |

## tests/testthat/test-issue-310.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 9 | `mgcv::bam` | issue 310: discrete bam parametric effects respect the fitting subset | `y ~ f1 + s(x, by = f1, k = 10, bs = "tp") + s(x, f2, k = 10, bs = "fs", xt = list(bs = "cr"))` | `` |

## tests/testthat/test-log-lik.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 24 | `mgcv::gam` | ordinary likelihoods retain native weights, scale and offsets | `y ~ x + offset(off)` | `f` |
| 48 | `mgcv::gam` | binomial encodings use trials once and retain frequency weights | `cbind(success, trials - success) ~ x + offset(off)` | `binomial()` |
| 50 | `mgcv::gam` | binomial encodings use trials once and retain frequency weights | `I(success/trials) ~ x + offset(off)` | `binomial()` |
| 52 | `mgcv::gam` | binomial encodings use trials once and retain frequency weights | `I(success > trials/2) ~ x + offset(off)` | `binomial()` |
| 108 | `mgcv::gam` | distributional likelihoods agree with normalized densities | `list(y ~ x, ~1, ~1, ~1)` | `mgcv::shash()` |
| 162 | `mgcv::gam` | single-predictor hurdle Poisson likelihoods include truncation constants | `y ~ x + offset(off)` | `mgcv::ziP(theta = c(0.3, 0.5), b = 0.2)` |
| 214 | `mgcv::gam` | fixed-power Tweedie likelihoods retain fitted dispersion and weights | `y ~ x + offset(off)` | `mgcv::Tweedie(p = 1.5)` |
| 234 | `mgcv::gam` | extended-family weights multiply normalized log densities | `y ~ x + offset(off)` | `f` |
| 272 | `mgcv::gam` | multinomial likelihoods select observed-category probabilities | `list(y ~ x + offset(off), ~z + offset(off2))` | `mgcv::multinom(K = 2)` |
| 324 | `mgcv::gam` | fitting rows and transformed responses retain their identity | `log(y) ~ x + offset(off)` | `` |
| 351 | `mgcv::gam` | unsupported observation likelihoods and invalid draws fail clearly | `y ~ x` | `quasipoisson()` |
| 353 | `mgcv::bam` | unsupported observation likelihoods and invalid draws fail clearly | `y ~ s(x, k = 5)` | `` |
| 364 | `mgcv::gam` | family likelihood hooks receive each draw's parameter values | `y ~ x + offset(off)` | `` |
| 406 | `mgcv::gam` | zero fitting weights and zero binomial trials contribute zero | `y ~ x` | `` |
| 411 | `mgcv::gam` | zero fitting weights and zero binomial trials contribute zero | `cbind(success, trials - success) ~ x` | `binomial()` |
| 420 | `mgcv::gam` | one-observation callbacks agree with matrix extraction for every family | `list(y ~ x, ~1, ~1, ~1)` | `mgcv::shash()` |
| 425 | `mgcv::gam` | one-observation callbacks agree with matrix extraction for every family | `y ~ x + offset(off)` | `f` |
| 427 | `mgcv::gam` | one-observation callbacks agree with matrix extraction for every family | `y ~ x` | `mgcv::ziP(theta = c(0.3, 0.5), b = 0.2)` |
| 429 | `mgcv::gam` | one-observation callbacks agree with matrix extraction for every family | `cbind(y, 4 - y) ~ x + offset(off)` | `binomial()` |

## tests/testthat/test-loo.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 6 | `mgcv::gam` | LOO and WAIC match direct calls with the same coefficient draws | `y ~ s(x, k = 5)` | `` |
| 32 | `mgcv::bam` | BAM methods use retained observations and sampler arguments | `y ~ s(x, k = 5)` | `` |
| 111 | `mgcv::gam` | likelihoods reject dependent custom response distributions | `y ~ x` | `` |
| 134 | `mgcv::gam` | likelihoods reject dependent custom response distributions | `cbind(y, family_id) ~ x` | `mgcv::gfam(list(gaussian(), gaussian()))` |
| 148 | `mgcv::gam` | multivariate likelihoods honour explicit family hooks | `list(y1 ~ x, y2 ~ x)` | `mgcv::mvn(d = 2)` |

## tests/testthat/test-lp-matrix.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-mcmc-chains.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 138 | `mgcv::gam` | penalty mapping, binomial encodings, and bam are supported | `form` | `` |
| 145 | `mgcv::gam` | penalty mapping, binomial encodings, and bam are supported | `cbind(success, 5 - success) ~ s(x, k = 5)` | `binomial()` |
| 181 | `mgcv::gam` | MH and the alternative prototypes reproduce known posteriors | `y ~ 1` | `poisson()` |
| 202 | `mgcv::gam` | chain IDs survive missing rows, offsets, and multinomial predictions | `y ~ s(x, k = 5)` | `` |
| 214 | `mgcv::gam` | chain IDs survive missing rows, offsets, and multinomial predictions | `list(y ~ s(x, k = 4), ~s(x, k = 4))` | `mgcv::multinom(K = 2)` |
| 246 | `mgcv::gam` | the Poisson target agrees with numerical posterior integration | `y ~ 1` | `poisson()` |

## tests/testthat/test-missing-rows.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 20 | `fit` | File setup or helper | `formula` | `family` |
| 27 | `fit` | File setup or helper | `formula` | `family` |
| 118 | `fit` | lm and glm dispersion and diagnostics survive exclusion | `y ~ x` | `` |
| 119 | `fit` | lm and glm dispersion and diagnostics survive exclusion | `y ~ x` | `` |
| 210 | `gam` | ordinal simulation never invents values for missing predictors | `y ~ s(x, k = 5)` | `ocat(R = 3)` |
| 255 | `gam` | subsets, factors and offsets use recorded exclusions | `y ~ s(x, k = 5) + f + offset(off)` | `` |
| 277 | `gam` | multi-parameter and multivariate restoration uses the row dimension | `list(y ~ s(x, k = 5), ~1)` | `gaulss()` |
| 283 | `gam` | multi-parameter and multivariate restoration uses the row dimension | `list(y ~ s(x, k = 5), z ~ s(x, k = 5))` | `mvn(d = 2)` |
| 320 | `gamm` | mixed model adapters do not restore already expanded components twice | `y ~ s(x, k = 5)` | `` |
| 321 | `gamm4::gamm4` | mixed model adapters do not restore already expanded components twice | `y ~ s(x, k = 5)` | `` |
| 347 | `gam` | binomial trials, supplied weights and RNG state stay aligned | `cbind(success, failure) ~ s(x, k = 5)` | `binomial()` |
| 353 | `gam` | binomial trials, supplied weights and RNG state stay aligned | `cbind(success, failure) ~ s(x, k = 5)` | `binomial()` |
| 394 | `gam` | factor-by evaluation preserves missing row covariates | `y ~ f + s(x, by = f, k = 5)` | `` |
| 409 | `scam::scam` | scam missing rows follow its supported NA option | `y ~ s(x, k = 5, bs = "mpd")` | `` |
| 423 | `gam` | one evaluable row does not change draw ordering or weight alignment | `y ~ s(x, k = 5)` | `` |

## tests/testthat/test-mixed-dependence.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 18 | `nlme::lme` | native lme variance weights do not imply dependent responses | `y ~ x` | `` |
| 26 | `update` | native lme variance weights do not imply dependent responses | `independent` | `` |
| 39 | `lme4::lmer` | native mer models distinguish conditional and shared marginal effects | `y ~ x + (1 \| g)` | `` |
| 51 | `lme4::glmer` | non-Gaussian marginal mer calibration uses native joint draws | `y ~ x + (1 \| g)` | `poisson()` |
| 77 | `mgcv::gamm` | correlated PQL keeps marginal families without inventing a copula | `y ~ s(x, k = 5)` | `poisson()` |
| 104 | `lme4::glmer` | observation-specific GLMM effects leave marginal responses independent | `y ~ 1 + (1 \| id)` | `poisson()` |
| 120 | `nlme::lme` | native lme and merMod dispatch through the shared diagnostics | `y ~ x` | `` |
| 121 | `lme4::lmer` | native lme and merMod dispatch through the shared diagnostics | `y ~ x + (1 \| g)` | `` |
| 139 | `nlme::lme` | native mixed posterior draws and PPCs reuse joint kernels and rows | `y ~ x` | `` |
| 143 | `lme4::lmer` | native mixed posterior draws and PPCs reuse joint kernels and rows | `y ~ x + (1 \| g)` | `` |
| 166 | `lme4::glmer` | simulation-integrated laws do not claim native canonical deviances | `y ~ x + (1 \| g)` | `poisson()` |
| 172 | `lme4::lmer` | simulation-integrated laws do not claim native canonical deviances | `y ~ x + (1 \| g)` | `` |
| 187 | `lme4::lmer` | proxy conditioning overrides reprepare the original native model | `y ~ x + (1 \| g)` | `` |
| 204 | `mgcv::gamm` | uncorrelated PQL PPC draws use the approximate family response law | `y ~ x + offset(off)` | `poisson()` |
| 216 | `mgcv::gamm` | uncorrelated PQL PPC draws use the approximate family response law | `y ~ x + offset(off)` | `poisson()` |
| 232 | `lme4::glmer` | native grouped-binomial posterior draws preserve the trial scale | `cbind(y, n - y) ~ x + (1 \| g)` | `binomial()` |

## tests/testthat/test-mixed-diagnostic-adapters.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 6 | `mgcv::gamm` | gamm conditional diagnostics use the full mixed-model fit | `y ~ s(x, k = 5)` | `` |
| 26 | `mgcv::gamm` | Gaussian gamm draws retain correlated errors in fitting order | `y ~ s(x, k = 5)` | `` |
| 41 | `gamm4::gamm4` | gamm4 defaults to the full conditional fit and keeps explicit component checks | `y ~ s(x, k = 5)` | `` |
| 59 | `mgcv::gamm` | Gaussian gamm posterior effects retain native joint uncertainty | `y ~ s(x, k = 5)` | `` |
| 77 | `mgcv::gamm` | gamm PQL conditional calibration uses full effects and offsets | `y ~ s(x, k = 5) + offset(off)` | `poisson()` |
| 94 | `gamm4::gamm4` | full gamm4 simulations use the diagnostic response scale and rows | `y ~ s(x, k = 5)` | `` |
| 113 | `gamm4::gamm4` | explicit gamm4 posterior modes share the PPC sampler and rows | `y ~ s(x, k = 5)` | `` |
| 135 | `gamm4::gamm4` | nonlinear gamm4 marginal simulation stays separate from residual moments | `y ~ x` | `poisson()` |

## tests/testthat/test-model-context.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 3 | `mgcv::gam` | File setup or helper | `y ~ s(log(x), k = 5)` | `` |
| 21 | `mgcv::gam` | explicit environments resolve closures without modifying the model | `y ~ s(fun(x), k = 5)` | `` |
| 43 | `mgcv::gam` | raw recovery verifies data and retained fitting rows | `y ~ s(log(x), k = 5)` | `` |
| 53 | `mgcv::gam` | known training evaluations can explain a stored-column fallback | `y ~ s(fun(x), k = 5)` | `` |
| 65 | `mgcv::gam` | component contexts keep distinct predictors | `list(y ~ s(log(x), k = 5), ~s(sqrt(z), k = 5))` | `mgcv::gaulss()` |
| 76 | `mgcv::gam` | full-model preparation does not borrow global covariate vectors | `y ~ s(log(x), k = 5)` | `` |

## tests/testthat/test-model-utilities.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-moments.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 13 | `mgcv::gam` | File setup or helper | `formula` | `family` |
| 20 | `fit` | File setup or helper | `normal` | `` |
| 21 | `fit` | File setup or helper | `positive` | `` |
| 22 | `fit` | File setup or helper | `positive` | `` |
| 23 | `fit` | File setup or helper | `count` | `` |
| 24 | `fit` | File setup or helper | `rbinom(n, 1, plogis(d$x))` | `` |
| 25 | `fit` | File setup or helper | `rnbinom(n, mu = mu, size = 3)` | `` |
| 26 | `fit` | File setup or helper | `rbeta(n, plogis(d$x) * 8, (1 - plogis(d$x)) * 8)` | `` |
| 27 | `fit` | File setup or helper | `mu + rt(n, df = 6)/3` | `` |
| 28 | `fit` | File setup or helper | `tweedie` | `` |
| 29 | `fit` | File setup or helper | `tweedie` | `` |
| 30 | `fit` | File setup or helper | `count` | `` |
| 31 | `fit` | File setup or helper | `normal` | `` |
| 32 | `fit` | File setup or helper | `positive` | `` |
| 33 | `fit` | File setup or helper | `mu - 0.4 * log(-log(runif(n)))` | `` |
| 36 | `fit` | File setup or helper | `mu + 0.4 * expm1(-0.1 * log(-log(runif(n))))/0.1` | `` |
| 38 | `fit` | File setup or helper | `normal` | `` |
| 39 | `fit` | File setup or helper | `tweedie` | `` |
| 40 | `fit` | File setup or helper | `rbinom(n, 1, 0.7) * (1 + rpois(n, mu))` | `` |
| 42 | `fit` | File setup or helper | `rbinom(n, 1, 0.7) * (1 + rpois(n, mu))` | `` |
| 43 | `fit` | File setup or helper | `normal` | `` |
| 44 | `fit` | File setup or helper | `mu + rlogis(n)/4` | `` |
| 45 | `fit` | File setup or helper | `count` | `` |
| 46 | `fit` | File setup or helper | `sample(1:4, n, replace = TRUE)` | `` |
| 47 | `fit` | File setup or helper | `sample(0:2, n, replace = TRUE)` | `` |
| 52 | `mgcv::gam` | File setup or helper | `list(y1 ~ x, y2 ~ x)` | `mgcv::mvn(d = 2)` |
| 241 | `mgcv::gam` | prediction rows, offsets and conditional grid metadata are retained | `y ~ x` | `poisson()` |
| 319 | `mgcv::gam` | grouped moments use component families with joint parameter uncertainty | `cbind(y, index) ~ group + x` | `mgcv::gfam(list(binomial(), gaussian(), poisson()))` |
| 353 | `mgcv::gam` | fixed Box-Cox zero gives original-response lognormal moments | `y ~ x` | `family` |
| 406 | `mgcv::bam` | BAM supports coefficient uncertainty and rejects unavailable global curvature | `y ~ s(x, k = 5)` | `poisson()` |
| 410 | `mgcv::bam` | BAM supports coefficient uncertainty and rejects unavailable global curvature | `y ~ s(x, k = 5)` | `` |
| 510 | `mgcv::gam` | likelihood dispersion and coefficient covariance use the same estimator | `y ~ x` | `Gamma(link = "log")` |
| 542 | `mgcv::gam` | global shape covariance agrees with an independent likelihood Hessian | `y ~ x` | `mgcv::nb()` |
| 554 | `mgcv::gam` | global shape covariance agrees with an independent likelihood Hessian | `y ~ x` | `mgcv::nb(theta = exp(t))` |
| 577 | `mgcv::gam` | linked and fixed smoothing parameters map into the joint covariance | `y ~ s(x, k = 5, id = 1) + s(z, k = 5, id = 1)` | `Gamma(link = "log")` |
| 579 | `mgcv::gam` | linked and fixed smoothing parameters map into the joint covariance | `y ~ s(x, k = 5) + s(z, k = 5)` | `Gamma(link = "log")` |
| 581 | `mgcv::gam` | linked and fixed smoothing parameters map into the joint covariance | `count ~ s(x, k = 5, id = 1) + s(z, k = 5, id = 1)` | `mgcv::nb()` |
| 609 | `mgcv::gam` | outer covariance uses curvature at the fitted parameters | `mpg ~ s(wt, k = 5)` | `Gamma(link = "log")` |

## tests/testthat/test-mrf-simultaneous.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-mrf.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-multinom-distribution.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 102 | `mgcv::gam` | fitted categorical distributions randomise across the observed mass | `list(y ~ x, ~x)` | `mgcv::multinom(K = 2)` |
| 151 | `mgcv::gam` | categorical fitted paths support binary responses and omitted rows | `list(y ~ x)` | `mgcv::multinom(K = 1)` |

## tests/testthat/test-multinom-surrogate.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 241 | `mgcv::gam` | fitted families retain categories absent from the fitting sample | `list(y ~ x, ~x)` | `mgcv::multinom(K = 2)` |

## tests/testthat/test-n-2d.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 25 | `gam` | surface panels retain independent slice controls and tensor order | `y ~ te(x0, x1, x2, x3, k = c(3, 3, 3, 3))` | `` |
| 28 | `gam` | surface panels retain independent slice controls and tensor order | `y ~ te(x0, x1, x2, d = c(1, 2), k = c(3, 10))` | `` |
| 35 | `gam` | factor smooth curves and factor-by surfaces retain levels | `y ~ fac + s(x0, x1, by = fac, k = 10)` | `` |
| 44 | `gam` | factor smooth curves and factor-by surfaces retain levels | `y ~ s(x0, fac, bs = bs, k = 4)` | `` |

## tests/testthat/test-native-lme-covariance.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 8 | `nlme::lme` | native blocked covariance uses its fitted predictor column order | `y ~ x + z` | `` |
| 44 | `nlme::lme` | native mixed covariance preserves fitted factor contrasts | `y ~ x + f` | `` |
| 61 | `nlme::lme` | native subset and omissions preserve fitted rows | `y ~ x` | `` |
| 86 | `nlme::lme` | native subset restores the fitted factor levels | `y ~ x + f` | `` |
| 111 | `lme4::glmer` | factor-binomial mixed laws retain one trial despite frequency weights | `y ~ x + (1 \| g)` | `binomial()` |
| 112 | `mgcv::gamm` | factor-binomial mixed laws retain one trial despite frequency weights | `y ~ x` | `binomial()` |

## tests/testthat/test-offset-sampling.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 20 | `mgcv::gam` | File setup or helper | `z ~ s(x, y, bs = "so", k = 12, xt = list(bnd = bnd, nmax = 40)) + offset(0.2 * x)` | `if (poisson) stats::poisson() else gaussian()` |

## tests/testthat/test-overview-parametric.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 15 | `gam` | overview reports coefficient-level parametric degrees of freedom | `form` | `` |

## tests/testthat/test-overview.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-parallel-ale.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 17 | `mgcv::gam` | File setup or helper | `y ~ g + s(x, by = g, k = 5) + te(x, z, k = c(4, 4)) + offset(log(x))` | `` |
| 21 | `mgcv::bam` | File setup or helper | `y ~ g + s(x, by = g, k = 5) + s(z, k = 5) + offset(log(x))` | `poisson()` |
| 24 | `mgcv::gam` | File setup or helper | `list(y ~ s(x, k = 5) + offset(off), ~s(z, k = 5))` | `mgcv::gaulss()` |
| 26 | `mgcv::gam` | File setup or helper | `y ~ s(stats::plogis(x), k = 5) + z` | `` |
| 30 | `mgcv::gam` | File setup or helper | `y ~ s(log(x), k = 5) + I(x * z) + offset(log(x))` | `` |
| 250 | `mgcv::gam` | ALE workers retain one caller draw matrix and the caller RNG state | `y ~ x + z` | `` |
| 294 | `mgcv::gam` | fresh ALE workers retain caller prediction methods and formula dependencies | `y ~ s(shifted(x), k = 5) + z` | `` |
| 325 | `mgcv::gam` | fresh ALE workers retain caller prediction methods and formula dependencies | `formulas[[name]]` | `` |
| 393 | `mgcv::gam` | ALE retains S3 transformations in the caller with fresh workers | `y ~ s(x, k = 5) + x:log(I(z))` | `` |
| 396 | `mgcv::gam` | ALE retains S3 transformations in the caller with fresh workers | `y ~ s(x, k = 5) + x:log(z)` | `` |

## tests/testthat/test-parallel-smooth-inputs.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 40 | `mgcv::gam` | random-effect margins release fitting environments without changing predictions | `y ~ te(x, f, bs = c("tp", "re"), k = c(4, 3)) + s(z, k = 4)` | `` |

## tests/testthat/test-parallel-smooth-operations.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 8 | `mgcv::gam` | File setup or helper | `y ~ te(x0, x1, k = c(3, 3)) + te(x0, x2, k = c(3, 3))` | `` |
| 76 | `mgcv::gam` | parallel calculations evaluate local expressions in the caller | `y ~ s(fun(x), k = 5) + s(sqrt(z), k = 5)` | `` |
| 77 | `mgcv::gam` | parallel calculations evaluate local expressions in the caller | `y ~ te(fun(x), z, k = c(3, 3)) + te(fun(x), x, k = c(3, 3))` | `` |
| 79 | `mgcv::gam` | parallel calculations evaluate local expressions in the caller | `y ~ g + s(fun(x), by = g, k = 5)` | `` |
| 113 | `mgcv::gam` | fresh workers preserve custom prediction methods and tensor margins | `y ~ s(x, bs = "gratia_worker", k = 5) + s(z, k = 5)` | `` |
| 115 | `mgcv::gam` | fresh workers preserve custom prediction methods and tensor margins | `y ~ te(x, z, bs = c("gratia_worker", "tp"), k = c(4, 4)) + te(x, w, k = c(4, 4))` | `` |
| 118 | `mgcv::gam` | fresh workers preserve custom prediction methods and tensor margins | `y ~ s(x, fac, bs = "fs", xt = list(bs = "gratia_worker"), k = 5) + s(z, k = 5)` | `` |
| 156 | `mgcv::gam` | fresh workers preserve optional prediction methods in gam fits | `y ~ s(x0, bs = "mpi", k = 5) + s(x1, k = 5)` | `` |
| 307 | `mgcv::gam` | empty and deterministic derivatives do not consume interval draws | `y ~ s(x0, by = x1, k = 5) + s(x2, k = 5)` | `` |
| 363 | `mgcv::scasm` | bootstrap derivative workers retain smoothing-bias correction | `y ~ s(x0, bs = "sc", xt = "m+", k = 5) + s(x1, bs = "sc", xt = "m+", k = 5)` | `` |

## tests/testthat/test-parallel.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 29 | `mgcv::gam` | parallel evaluation forwards local expression environments | `y ~ s(fun(x), k = 5) + s(sqrt(z), k = 5)` | `` |
| 44 | `mgcv::gam` | parallel automatic raw grids retain custom tensor coordinates | `y ~ te(foo(x), sqrt(z), k = c(4, 4))` | `` |

## tests/testthat/test-parametric-effects-constant.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-parametric-effects-interactions.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 27 | `gam` | interaction components agree with coefficient blocks and fitted contrasts | `y ~ x * z + x * a + a * b` | `` |
| 40 | `gam` | interaction components agree with coefficient blocks and fitted contrasts | `y ~ x * a` | `` |
| 49 | `gam` | transformed interactions can retain raw axes and polynomials do | `y ~ poly(x, 3) * a + log(z):w + x + I(x^2)` | `` |
| 68 | `gam` | higher-dimensional grids allocate numeric axes and facets | `as.formula(paste("y ~", term))` | `` |
| 87 | `gam` | multiple linear predictors include and select interactions | `list(y ~ x0 * x1, ~x0 * x1)` | `gaulss()` |
| 99 | `gam` | multiple linear predictors include and select interactions | `list(y ~ x0:x1, ~1)` | `gaulss()` |
| 102 | `gam` | multiple linear predictors include and select interactions | `count ~ x0 * x1` | `nb()` |
| 110 | `gam` | supplied grids, reserved names and metadata survive selection and nesting | `y ~ .partial * odd.name + a:b` | `` |
| 125 | `gam` | supplied grids, reserved names and metadata survive selection and nesting | `y ~ x:flag` | `` |
| 133 | `gam` | assembly and direct drawing share interaction displays and transforms | `y ~ x:z:a + x:a + a:b + s(w, k = 5)` | `` |
| 154 | `gam` | interaction uncertainty includes smoothing selection when requested | `y ~ x * a + s(z, k = 6)` | `` |
| 167 | `gam` | interaction rugs use observations and intervals transform together | `y ~ x:a` | `` |
| 182 | `gam` | parametric surface masking changes plots but not estimates | `y ~ x:z` | `` |
| 194 | `gam` | interaction plots have representative visual coverage | `as.formula(paste("y ~", term))` | `` |
| 205 | `gam` | zero interaction slices retain surfaces without contour warnings | `y ~ x * z * a` | `` |
| 218 | `gam` | surface fills centre zero across dimensions and effect ranges | `as.formula(paste("y ~", term))` | `` |
| 239 | `gam` | surface fill ranges use finite unmasked values and respect custom scales | `y ~ x:z` | `` |
| 263 | `gam` | fixed parametric surfaces share displayed fill ranges through both entry points | `y ~ x:z + w:v` | `` |

## tests/testthat/test-parametric-effects.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 100 | `gam` | issue 212 remains fixed | `y ~ fac + poly(x0, 2, raw = TRUE) + poly(x1, 2, raw = TRUE) + poly(x2, 2, raw = TRUE) + poly(x3, 2, raw = TRUE)` | `` |

## tests/testthat/test-parametric-model-labels.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 16 | `mgcv::gam` | File setup or helper | `list(y ~ x + f + s(z, k = 4), ~x + f + s(z, k = 4))` | `mgcv::gaulss()` |
| 72 | `mgcv::gam` | parametric labels use coefficient membership rather than suffixes | `list(category ~ 1, ~1, 1 + 2 ~ x - 1)` | `mgcv::multinom(K = 2)` |
| 81 | `mgcv::gam` | parametric labels use coefficient membership rather than suffixes | `list(y ~ 1, ~x - 1)` | `mgcv::gaulss()` |
| 90 | `mgcv::gam` | parametric interactions and reserved covariates retain model labels | `list(stats::as.formula(paste("y ~", term)), ~x)` | `mgcv::gaulss()` |
| 133 | `mgcv::gam` | mgcv related models keep single predictor plots and custom subtitles | `y ~ x + s(z, k = 6)` | `` |
| 134 | `mgcv::bam` | mgcv related models keep single predictor plots and custom subtitles | `y ~ x + s(z, k = 6)` | `` |
| 135 | `mgcv::gamm` | mgcv related models keep single predictor plots and custom subtitles | `y ~ x + s(z, k = 6)` | `` |
| 136 | `gamm4::gamm4` | mgcv related models keep single predictor plots and custom subtitles | `y ~ x + s(z, k = 6)` | `` |
| 137 | `scam::scam` | mgcv related models keep single predictor plots and custom subtitles | `y ~ x + s(z, bs = "mpi", k = 6)` | `` |
| 138 | `mgcv::scasm` | mgcv related models keep single predictor plots and custom subtitles | `y ~ x + s(z, bs = "sc", xt = "m+", k = 6)` | `` |
| 145 | `mgcv::scasm` | mgcv related models keep single predictor plots and custom subtitles | `list(y ~ x + s(z, bs = "sc", xt = "m+", k = 6), ~x)` | `mgcv::gaulss()` |
| 157 | `GJRM::gamlss` | GJRM parametric labels use parent equations and preserve plot names | `list(y ~ x, ~x)` | `` |
| 179 | `GJRM::gamlss` | GJRM parametric labels use parent equations and preserve plot names | `list(y ~ x)` | `` |
| 187 | `glmmTMB::glmmTMB` | glmmTMB parametric labels identify components without smooths | `y ~ x` | `glmmTMB::nbinom2()` |
| 199 | `glmmTMB::glmmTMB` | glmmTMB parametric labels identify components without smooths | `y ~ x` | `glmmTMB::nbinom2()` |
| 201 | `glmmTMB::glmmTMB` | glmmTMB parametric labels identify components without smooths | `y ~ x` | `poisson()` |
| 210 | `sdmTMB::sdmTMB` | sdmTMB ordinary terms retain delta labels | `positive ~ x` | `sdmTMB::delta_gamma()` |

## tests/testthat/test-parametric-overall-uncertainty.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 10 | `gam` | parametric intervals include intercept covariance without shifting effects | `form` | `` |
| 39 | `gam` | parametric intervals include intercept covariance without shifting effects | `y ~ x + a` | `` |
| 59 | `gam` | each linear predictor supplies only its own intercept uncertainty | `form` | `gaulss()` |
| 83 | `gam` | unavailable smoothing covariance warns once and uses Bayesian covariance | `y ~ x` | `` |

## tests/testthat/test-partial-derivatives-focal.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 5 | `gam` | partial derivatives focal works | `y ~ te(x, z, k = c(4, 4)) + te(w, z, k = c(4, 4))` | `` |
| 18 | `gam` | partial derivatives focal works | `y ~ s(w, k = 5) + te(x, z, k = c(4, 4))` | `` |

## tests/testthat/test-partial-derivatives.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 16 | `bam` | File setup or helper | `y ~ te(x1, x2, by = z1, bs = c("tp", "cc"), k = c(4, 4)) + te(x1, x2, z2, by = z1, bs = c("tp", "cc", "re"), k = c(4, 4,      4), m = 1)` | `gaussian()` |
| 25 | `gam` | partial derivatives filters excluded smooths at their original positions | `y ~ s(x1, k = 4) + te(x1, x2, k = c(4, 4)) + s(x2, k = 4) + s(z2, bs = "re")` | `` |

## tests/testthat/test-partial-residuals-lss.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 8 | `gam` | File setup or helper | `list(y ~ s(x, k = 5), ~1, ~1, ~1)` | `shash()` |
| 74 | `gam` | ordinary Gamma models retain weighted working partial residuals | `y ~ s(x0, k = 5) + s(x2, k = 5)` | `Gamma(link = "log")` |

## tests/testthat/test-penalty-geoms.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-penalty.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-plot-helpers.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-plot-themes.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-posterior-ar1.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 5 | `mgcv::bam` | AR1 posterior response draws retain correlation and segment resets | `y ~ s(x, k = 5)` | `` |
| 29 | `mgcv::bam` | raw and streamed AR1 posterior checks agree across batches | `y ~ s(x, k = 5) + offset(off)` | `` |
| 47 | `mgcv::bam` | working AR1 corrections do not silently produce independent PPC | `y ~ s(x, k = 5)` | `poisson()` |

## tests/testthat/test-posterior-native-parameterization.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 17 | `glmmTMB::glmmTMB` | glmm posterior responses retain native dispersion parameterizations | `y ~ x` | `family` |
| 47 | `glm` | weighted factor binomial PPC retains Bernoulli observation encoding | `y ~ x` | `binomial()` |
| 57 | `mgcv::gam` | weighted factor binomial PPC retains Bernoulli observation encoding | `y ~ s(x, k = 4)` | `binomial()` |
| 70 | `mgcv::gam` | explicit GAM sampling weights override fitted trial counts and retain exclusions | `y ~ s(x, k = 4)` | `binomial()` |
| 91 | `mgcv::bam` | AR1 GAM draws honour explicit precision weights without losing correlation | `y ~ s(x, k = 4)` | `` |

## tests/testthat/test-posterior-predictive-plot.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 97 | `gam` | binomial responses match the sampling scale | `cbind(success, failure) ~ s(x, k = 5)` | `binomial()` |
| 100 | `gam` | binomial responses match the sampling scale | `success/trials ~ s(x, k = 5)` | `binomial()` |
| 117 | `gam` | binomial responses match the sampling scale | `reformulate("s(x, k = 5)", response = response)` | `binomial()` |
| 133 | `gam` | missing fitting rows, subsets and offsets stay aligned | `y ~ s(x, k = 5) + offset(off)` | `` |

## tests/testthat/test-posterior-samples.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 309 | `gam` | posterior sampling funs work with offsets in formula issue 233 | `y ~ 1 + offset(log(denom))` | `"nb"` |

## tests/testthat/test-prepare-smooth-plot.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-prepare-surface-plot.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 188 | `mgcv::gam` | tile selection propagates through model drawing and assembly | `y ~ u + s(x, z, k = 8) + s(u, k = 4) + s(f, bs = "re")` | `` |
| 250 | `mgcv::gam` | tensor surfaces forward geometry and retain marginal ordering | `as.formula(paste0("y ~ ", constructor, "(", specs[i], ")"))` | `` |

## tests/testthat/test-qq-plot.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-quantile-residuals-missing.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 9 | `gam` | quantile residual diagnostics respect missing-row conventions | `y ~ s(x, k = 6)` | `poisson()` |
| 14 | `glm` | quantile residual diagnostics respect missing-row conventions | `y ~ x` | `poisson()` |
| 16 | `fit` | quantile residual diagnostics respect missing-row conventions | `na.exclude` | `` |
| 17 | `fit` | quantile residual diagnostics respect missing-row conventions | `na.omit` | `` |

## tests/testthat/test-quantile-residuals.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-random-effect-samples.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 18 | `mgcv::gam` | random slopes and crossed factor groups use independent coefficients | `y ~ s(x, k = 5) + s(x, g, h, bs = "re")` | `` |
| 123 | `mgcv::gam` | partial selection, offsets, NA restoration and BAM remain supported | `y ~ s(x, k = 5) + s(g, bs = "re") + s(h, bs = "re") + offset(off)` | `poisson()` |
| 150 | `mgcv::gam` | fixed and shared smoothing parameters retain their penalty mapping | `y ~ s(x, k = 5) + s(g, bs = "re")` | `` |
| 154 | `mgcv::gam` | fixed and shared smoothing parameters retain their penalty mapping | `y ~ s(x, by = h, k = 4, id = 1) + s(g, bs = "re")` | `` |
| 245 | `mgcv::gam` | fitting offsets retain each prediction interface's existing convention | `y ~ x + s(g, bs = "re")` | `poisson()` |
| 279 | `mgcv::gam` | transformed predictors and grouping expressions are evaluated consistently | `y ~ x + s(log(x), factor(id), bs = "fs", k = 4)` | `poisson()` |
| 292 | `mgcv::gam` | transformed predictors and grouping expressions are evaluated consistently | `y ~ id + x + s(x, factor(id), bs = "fs", k = 4)` | `poisson()` |
| 388 | `mgcv::gam` | minimum smoothing parameters cannot silently change new-effect covariance | `y ~ x + s(g, bs = "re")` | `poisson()` |
| 397 | `mgcv::gam` | minimum smoothing parameters cannot silently change new-effect covariance | `y ~ x + s(x, g, bs = "fs", k = 4)` | `poisson()` |
| 402 | `mgcv::gam` | minimum smoothing parameters cannot silently change new-effect covariance | `y ~ x + s(g, bs = "re")` | `poisson()` |
| 410 | `mgcv::gam` | factor-by fs curves keep their by levels when another group is replaced | `y ~ x + s(f, bs = "re") + s(x, g, bs = "fs", k = 4, by = f)` | `poisson()` |
| 455 | `mgcv::gam` | a random-effect grouping factor can also select its own by level | `y ~ x + s(f, bs = "re", by = f)` | `poisson()` |

## tests/testthat/test-residual-correlation-gaps.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 5 | `lm` | ordered residual correlation retains omitted positions for both NA actions | `y ~ x` | `` |
| 20 | `lm` | full interleaved series identify omitted membership without foreign gaps | `y ~ x` | `` |
| 25 | `lm` | full interleaved series identify omitted membership without foreign gaps | `y ~ x` | `` |
| 46 | `mgcv::bam` | BAM omission gaps remain inside native AR.start segments | `y ~ s(x, k = 4)` | `` |

## tests/testthat/test-residual-tails.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 83 | `mgcv::gam` | ordinary discrete probabilities and public seed handling agree | `y ~ s(x, k = 5) + offset(log(exposure))` | `fam` |
| 97 | `glm` | ordinary discrete probabilities and public seed handling agree | `y ~ x` | `poisson()` |

## tests/testthat/test-residuals-covariate-plot.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 3 | `mgcv::gam` | covariate selection preserves order and excludes special variables | `y ~ x + s(z, k = 5) + s(g, bs = "re") + offset(log(exposure))` | `` |
| 16 | `mgcv::gam` | terms accept only exact mgcv smooth labels | `y ~ f + te(x, z, k = c(4, 4)) + s(z, by = f, k = 4)` | `` |
| 41 | `mgcv::gam` | trends reveal a curved mean fitted as linear | `y ~ x` | `` |
| 42 | `mgcv::gam` | trends reveal a curved mean fitted as linear | `y ~ s(x, k = 15)` | `` |
| 62 | `mgcv::gam` | automatic trends respect eligible group sizes and explicit overrides | `y ~ x` | `` |
| 63 | `mgcv::gam` | automatic trends respect eligible group sizes and explicit overrides | `y ~ x` | `` |
| 96 | `mgcv::gam` | joint maps reveal an interaction missed by marginal trends | `y ~ x + z` | `` |
| 147 | `mgcv::gam` | raw transformed and omitted covariates use verified fitting data | `y ~ log(x) + s(z, k = 5)` | `` |
| 151 | `mgcv::gam` | raw transformed and omitted covariates use verified fitting data | `y ~ s(log(x), k = 5)` | `` |
| 173 | `mgcv::gam` | subsetting and both missing-value actions preserve row alignment | `y ~ x + s(z, k = 5)` | `` |
| 191 | `mgcv::gam` | multivariate responses and grouped families are separated | `list(y ~ s(x, k = 5), y2 ~ s(x, k = 5))` | `mgcv::mvn(d = 2)` |
| 202 | `mgcv::gam` | multivariate responses and grouped families are separated | `Y ~ s(x, k = 5)` | `mgcv::gfam(list(stats::gaussian(), stats::gaussian()))` |
| 214 | `mgcv::gam` | randomised residuals are shared across panels and preserve RNG state | `count ~ z` | `stats::poisson()` |
| 291 | `mgcv::gam` | supplied data can be reordered when row identities and values agree | `y ~ x` | `` |
| 299 | `mgcv::gam` | distributional predictors contribute covariates without extra residuals | `list(y ~ s(x, k = 5), ~z)` | `mgcv::gaulss()` |
| 310 | `mgcv::gamm` | GAM mixed models retain smooth selection and complete-model residuals | `y ~ s(x, k = 5) + z` | `` |
| 322 | `lme4::lmer` | direct lme4 models use fixed predictors by default | `y ~ x + (1 \| g)` | `` |
| 328 | `lme4::lmer` | direct lme4 models use fixed predictors by default | `y ~ 1 + (x \| g)` | `` |
| 335 | `glmmTMB::glmmTMB` | glmmTMB diagnostics use raw covariates and exact smooth labels | `y ~ s(x, k = 5) + z` | `stats::gaussian()` |
| 346 | `GJRM::gamlss` | GJRM smooth labels expand covariates across equations | `list(y ~ s(x, k = 5), ~s(z, k = 5))` | `"N"` |
| 358 | `sdmTMB::sdmTMB` | sdmTMB components keep their fitted observation rows | `y ~ s(x, k = 5)` | `` |
| 364 | `sdmTMB::sdmTMB` | sdmTMB components keep their fitted observation rows | `y ~ x` | `sdmTMB::delta_gamma()` |
| 376 | `mgcv::gam` | random-slope covariates remain default diagnostic axes | `y ~ s(x, g, bs = "re") + s(z, k = 5)` | `` |

## tests/testthat/test-residuals.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-response-derivatives-delta.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 59 | `mgcv::gam` | offsets and excluded terms enter the response gradient correctly | `y ~ x + s(z, k = 5) + offset(0.2 * x)` | `poisson()` |
| 127 | `mgcv::bam` | delta supports BAM smooths and preserves single-row predictions | `y ~ s(x, k = 6)` | `poisson()` |

## tests/testthat/test-response-derivatives.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-response-distributions.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 139 | `lm` | fitted GLM and weighted lm wrappers prepare the common distribution | `y ~ x` | `` |
| 142 | `glm` | fitted GLM and weighted lm wrappers prepare the common distribution | `y ~ x` | `Gamma(link = "log")` |
| 155 | `glmmTMB::glmmTMB` | glmmTMB adapters share count mixtures and binomial response scales | `y ~ x` | `glmmTMB::nbinom2()` |
| 168 | `glmmTMB::glmmTMB` | glmmTMB adapters share count mixtures and binomial response scales | `cbind(successes, trials - successes) ~ x` | `binomial()` |
| 237 | `glmmTMB::glmmTMB` | glmmTMB proxy preserves conditioning and native residual definitions | `count ~ mined + (1 \| site)` | `poisson()` |
| 240 | `glmmTMB::glmmTMB` | glmmTMB proxy preserves conditioning and native residual definitions | `count ~ mined + (1 \| site)` | `` |
| 342 | `glmmTMB::glmmTMB` | marginal Gaussian glmmTMB QQ residuals share population centering | `y ~ x + (1 \| g)` | `` |
| 357 | `glmmTMB::glmmTMB` | marginal Gaussian glmmTMB QQ residuals share population centering | `abs(round(y)) ~ x + (1 \| g)` | `poisson()` |
| 365 | `glmmTMB::glmmTMB` | unsupported glmmTMB distributions retain native diagnostic panels | `y ~ x` | `glmmTMB::compois()` |
| 378 | `glmmTMB::glmmTMB` | native glmmTMB lognormal dispersion is translated once | `Sepal.Length ~ Petal.Length` | `glmmTMB::lognormal()` |
| 394 | `mgcv::bam` | nonidentity Gaussian bam AR1 exposes only marginal calibration | `y ~ s(x, k = 5)` | `gaussian(link = "log")` |

## tests/testthat/test-response-law-audit.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 89 | `glm` | weighted factor-binomial response laws retain Bernoulli observations | `y ~ 1` | `binomial()` |
| 90 | `mgcv::gam` | weighted factor-binomial response laws retain Bernoulli observations | `y ~ 1` | `binomial()` |
| 163 | `glmmTMB::glmmTMB` | zero-trial glmmTMB rows have consistent native and marginal residuals | `cbind(success, failure) ~ x` | `binomial()` |
| 183 | `mgcv::gam` | grouped beta observed and reference laws share endpoint calibration | `cbind(y, fi) ~ factor(fi)` | `mgcv::gfam(list(mgcv::betar(theta = -0.1, eps = 0.05), gaussian()))` |
| 326 | `mgcv::gam` | small-scale fitted shash diagnostics reject invalid native deviance only | `list(y ~ 1, ~1, ~1, ~1)` | `mgcv::shash()` |

## tests/testthat/test-rfoo.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-rootograms-gaussian.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 11 | `gam` | rootograms gaussian works | `y ~ x` | `gaussian(link)` |
| 32 | `gam` | rootograms gaussian works | `y ~ x` | `` |
| 36 | `gam` | rootograms gaussian works | `y ~ x` | `` |

## tests/testthat/test-rootograms-labels.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-rootograms.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-rtw-vector.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-samplers-singular.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-samplers.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-sampling-fixed-contributions.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 27 | `fitter` | File setup or helper | `f` | `if (poisson) stats::poisson() else stats::gaussian()` |
| 100 | `mgcv::gam` | family target retains actually fitted general-family offsets | `list(y ~ x + offset(off), ~z)` | `mgcv::gaulss()` |
| 120 | `mgcv::bam` | ordinary and discrete bam targets evaluate original covariates | `y ~ s(x, k = 6) + offset(off)` | `poisson()` |
| 145 | `mgcv::gam` | LSS evaluation draws include real prescribed smooth boundaries | `list(z ~ s(x, y, bs = "so", k = 12, xt = list(bnd = bnd, nmax = 40)) + offset(off), ~offset(0.1 * y))` | `mgcv::gaulss()` |
| 187 | `mgcv::gam` | grouped evaluation draws include real prescribed smooth boundaries | `cbind(z, component) ~ s(x, y, bs = "so", k = 12, xt = list(bnd = bnd, nmax = 40)) + offset(off)` | `mgcv::gfam(list(gaussian(), gaussian()))` |

## tests/testthat/test-sampling-grouped.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-sampling-interface.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-sampling-likelihood-integration.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 56 | `mgcv::gam` | likelihood adapters use each MH backend and parallel chain streams | `mpg ~ s(wt, k = 5)` | `` |

## tests/testthat/test-sampling-lss.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 142 | `mgcv::gam` | fitting-call offsets and excluded fitting rows are retained for LSS | `list(y ~ x, ~z)` | `mgcv::gaulss()` |
| 201 | `do.call` | LSS generated draws and selected coefficients retain joint sampling | `fitted_samples` | `` |
| 220 | `mgcv::gam` | shared LSS coefficient mappings agree with native joint predictions | `list(y ~ z, ~1, 1 + 2 ~ s(x, k = 4) - 1)` | `mgcv::gaulss()` |
| 252 | `mgcv::gam` | censored draws are latent and survival sampling reports the missing process | `y ~ x` | `mgcv::cnorm()` |
| 261 | `mgcv::gam` | censored draws are latent and survival sampling reports the missing process | `t ~ x` | `mgcv::cox.ph()` |
| 270 | `mgcv::gam` | Cox predictor draws remain available without implying a response mean | `t ~ x` | `mgcv::cox.ph()` |
| 281 | `mgcv::gam` | LSS prediction retains fitted non-default links and bounds | `list(y ~ x, ~x + offset(off))` | `mgcv::gaulss(link = c("log", "logb"), b = 0.2)` |

## tests/testthat/test-sampling-mh-adjustment.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 160 | `mgcv::bam` | the adjustment works for BAM and is not a public method | `y ~ s(x, k = 6)` | `` |

## tests/testthat/test-sampling-review.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 2 | `mgcv::gam` | supplied draw identities survive reordering and transformations | `mpg ~ s(wt, k = 4) + s(hp, k = 4)` | `` |
| 119 | `mgcv::gamm` | the new evaluator rejects dependent GAMM components | `mpg ~ s(wt, k = 4)` | `` |

## tests/testthat/test-sampling-smoothing-family.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 56 | `mgcv::gam` | non-Gaussian mappings and fixed-sp refits agree across methods | `fixture$formula` | `mgcv::nb(theta = fit$family$getTheta(TRUE))` |
| 95 | `mgcv::gam` | native updates retain censoring and extended-family parameters | `fixture$formula` | `family` |
| 199 | `mgcv::gam` | family covariance flags require a retained matrix with known ordering | `fixture$formula` | `mgcv::nb()` |

## tests/testthat/test-sampling-smoothing.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 27 | `mgcv::gam` | smoothing conditionals agree with independent Gaussian refits | `fixture$formula` | `` |
| 106 | `mgcv::gam` | smoothing setup uses retained fitting rows and call-level offsets | `y ~ s(x, k = 5) + s(z, k = 5)` | `` |
| 115 | `mgcv::gam` | smoothing prototype rejects unsupported targets precisely | `y ~ s(x, k = 5)` | `` |
| 130 | `mgcv::bam` | smoothing prototype rejects unsupported targets precisely | `y ~ s(x, k = 5)` | `` |
| 132 | `mgcv::bam` | smoothing prototype rejects unsupported targets precisely | `y ~ s(x, k = 5)` | `` |
| 186 | `mgcv::bam` | bam ML and REML preserve conditionals and smoothing mappings | `fixture$formula` | `` |
| 217 | `mgcv::bam` | smoothing audit distinguishes actual fitting and parameter blocks | `fixture$formula` | `` |
| 221 | `mgcv::gam` | smoothing audit distinguishes actual fitting and parameter blocks | `list(y ~ s(x, k = 5), ~s(z, k = 5))` | `mgcv::gaulss()` |
| 228 | `mgcv::gam` | smoothing audit distinguishes actual fitting and parameter blocks | `count ~ s(x, k = 5) + s(z, k = 5)` | `mgcv::nb()` |
| 237 | `mgcv::gam` | smoothing audit distinguishes actual fitting and parameter blocks | `count ~ s(x, k = 5)` | `mgcv::nb(theta = 3)` |
| 242 | `mgcv::gam` | smoothing audit distinguishes actual fitting and parameter blocks | `positive ~ s(x, k = 5)` | `mgcv::tw()` |
| 255 | `mgcv::gam` | smoothing uncertainty excludes NCV and QNCV using the actual fit | `formula` | `` |
| 257 | `mgcv::bam` | smoothing uncertainty excludes NCV and QNCV using the actual fit | `formula` | `` |
| 307 | `mgcv::gam` | partly fixed bam fREML uses the dispersion retained in Vp | `fixture$formula` | `` |
| 335 | `mgcv::bam` | partly fixed fREML dispersion respects fitting rows and fixed scale | `y ~ s(x, k = 5, sp = 1) + s(z, k = 5)` | `` |

## tests/testthat/test-scalable-density.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-scalable-diagnostics.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 134 | `lm` | appraise switches using retained fitting rows and respects overrides | `y ~ x` | `` |
| 145 | `glm` | model adapters preserve weights, binomial proportions and multivariate rows | `cbind(y, n - y) ~ x` | `binomial()` |
| 156 | `lm` | model adapters preserve weights, binomial proportions and multivariate rows | `mpg ~ wt` | `` |
| 304 | `glmmTMB::glmmTMB` | glmmTMB scalable diagnostics use native residuals and conditional references | `y ~ x + (1 \| g)` | `` |
| 323 | `glmmTMB::glmmTMB` | glmmTMB scalable diagnostics use native residuals and conditional references | `cbind(y, n - y) ~ x` | `binomial()` |

## tests/testthat/test-scalable-gjrm.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 22 | `GJRM::gamlss` | GJRM adapters use final distributional means and residuals for each family | `f` | `if (fam == "GAi") "GA" else fam` |
| 126 | `GJRM::gamlss` | GJRM standard-normal bands and worm detrending are aligned | `list(y ~ x, ~1)` | `` |

## tests/testthat/test-scalable-grouped.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-scales.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-scam-basis-adapter.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 23 | `scam::scam` | File setup or helper | `form` | `` |
| 132 | `scam::scam` | fitted SCAM bases retain scale transformation parameters | `y ~ s(scale(x), bs = "mpi", k = 7)` | `` |

## tests/testthat/test-scam-captions.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 113 | `scam::scam` | File setup or helper | `y ~ s(age, bs = "mpi", k = 6) + s(dose, bs = "cv", k = 6)` | `` |
| 115 | `scam::scam` | File setup or helper | `y ~ group + s(age, by = group, bs = "mpi", k = 6)` | `` |
| 117 | `scam::scam` | File setup or helper | `y ~ s(age, bs = "lipl", k = 12, xt = list(xc = 0.37))` | `` |
| 119 | `scam::scam` | File setup or helper | `y ~ s(age, dose, bs = "tesmi1", k = c(5, 5), xt = "cc")` | `` |
| 215 | `scam::scam` | local SCAM captions retain model coordinates on original-scale plots | `y ~ s(log(age), bs = "lipl", k = 12, xt = list(xc = -0.75))` | `` |

## tests/testthat/test-scam-gam-api.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 12 | `scam::scam` | File setup or helper | `y ~ fac + z + s(x, bs = "mpi", k = 6)` | `` |
| 13 | `scam::scam` | File setup or helper | `y ~ fac + s(x, by = fac, bs = "mpi", k = 6)` | `` |
| 14 | `scam::scam` | File setup or helper | `y ~ s(x, z, bs = "tedmi", k = c(4, 4))` | `` |
| 15 | `scam::scam` | File setup or helper | `y ~ 0 + s(x, bs = "mpi", k = 6)` | `` |
| 181 | `scam::scam` | SCAM model matrices and concurvity omit plateau coefficients fixed to zero | `y ~ z + s(x, bs = "lipl", k = 12, xt = list(xc = 0.5))` | `` |
| 199 | `scam::scam` | SCAM concurvity does not depend on model-matrix registration | `y ~ s(z, bs = "mpi", k = 6) + s(x, bs = "lipl", k = 12, xt = list(xc = 0.5))` | `` |
| 249 | `scam::scam` | SCAM matrix extraction retains training offsets and excluded rows | `y ~ offset(z) + s(x, bs = "mpi", k = 6)` | `` |
| 293 | `scam::scam` | SCAM response-scale delta contrasts and derivatives use inverse-link gradients | `count ~ fac + s(x, bs = "mpi", k = 6)` | `poisson()` |

## tests/testthat/test-scam-methods.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-scam-prediction.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 19 | `scam::scam` | File setup or helper | `forms[[name]]` | `` |
| 22 | `scam::scam` | File setup or helper | `y ~ offset(o) + s(x, bs = "lipl", k = 12, xt = list(xc = 0.5))` | `` |
| 24 | `scam::scam` | File setup or helper | `y ~ offset(o) + s(x, bs = "lipl", k = 12, xt = list(xc = 0.5)) + s(z, bs = "lipl", k = 12, xt = list(xc = 0.6))` | `` |
| 82 | `scam::scam` | SCAM response draws retain formula offsets for Poisson and Gamma models | `y ~ offset(o) + s(x, bs = "mpi", k = 6)` | `fam` |
| 100 | `scam::scam` | SCAM prediction and draws restore excluded fitting and prediction rows | `y ~ offset(o) + s(x, bs = "mpi", k = 6)` | `` |
| 123 | `scam::scam` | canonical SCAM training predictions restore na.exclude once | `y ~ z + offset(o) + s(x, bs = "mpi", k = 6)` | `` |

## tests/testthat/test-scam-release-fixes.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 13 | `scam::scam` | File setup or helper | `y ~ z + s(x, bs = bs, k = 6)` | `` |
| 17 | `scam::scam` | File setup or helper | `y ~ fac + s(x, by = fac, bs = "mpi", k = 6)` | `` |
| 19 | `scam::scam` | File setup or helper | `y ~ by + s(x, by = by, bs = "mpi", k = 6)` | `` |
| 21 | `scam::scam` | File setup or helper | `y ~ 0 + s(x, bs = "mpi", k = 6)` | `` |
| 22 | `scam::scam` | File setup or helper | `y ~ s(x, bs = "cr", k = 6)` | `` |

## tests/testthat/test-scam-response-dependence.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 11 | `scam::scam` | File setup or helper | `y ~ s(x, bs = "mpi", k = 6)` | `` |

## tests/testthat/test-scam-sampling.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 17 | `scam::scam` | File setup or helper | `f` | `` |
| 20 | `scam::scam` | File setup or helper | `y ~ s(x, bs = "mpi", k = 6)` | `` |

## tests/testthat/test-scam-smooth-coverage.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 37 | `scam::scam` | File setup or helper | `form` | `` |
| 142 | `scam::scam` | SCAM cyclic margins and interactions keep their fitted matrices | `y ~ s(x, z, bs = bs, k = c(5, 5), xt = "cc")` | `` |
| 157 | `scam::scam` | SCAM cyclic margins and interactions keep their fitted matrices | `y ~ ti(x, k = 5) + ti(z, k = 5) + s(x, z, bs = bs, k = c(5, 5))` | `` |
| 177 | `scam::scam` | SCAM matrix-valued numeric-by terms evaluate linear functionals | `y ~ s(X, by = L, bs = "mpiBy", k = 6)` | `` |

## tests/testthat/test-scasm-constraints.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 23 | `mgcv::scasm` | shape metadata records codes without changing smooth types | `y ~ s(x, bs = "sc", xt = c("m+", "c+"), k = 6)` | `` |
| 33 | `mgcv::scasm` | retention is checked for each tensor rather than the whole model | `y ~ s(x, bs = "sc", xt = "m+", k = 6) + ti(x, z, bs = c("sc", "sc"), xt = list("m+", "m+"), k = c(5, 5))` | `` |
| 44 | `mgcv::scasm` | retention is checked for each tensor rather than the whole model | `y ~ t2(x, z, bs = c("sc", "sc"), xt = list("m+", "m+"), k = c(5, 5))` | `` |
| 49 | `mgcv::scasm` | retention is checked for each tensor rather than the whole model | `y ~ te(x, z, bs = c("sc", "cr"), xt = list("m+", NULL), k = c(5, 5))` | `` |
| 60 | `mgcv::gam` | ordinary gam requests are not labelled as retained constraints | `y ~ s(x, bs = "sc", xt = "m+", k = 6)` | `` |
| 63 | `mgcv::gam` | ordinary gam requests are not labelled as retained constraints | `y ~ s(x, bs = "cr", k = 6)` | `` |
| 69 | `mgcv::scasm` | captions preserve literal text and use plotmath arrows | `y ~ s(x, bs = "sc", xt = c("m+", "c+"), k = 6)` | `` |
| 88 | `mgcv::scasm` | general tensor constraints are described separately from margins | `y ~ te(x, z, bs = c("sc", "cr"), xt = list("m+", NULL), k = c(5, 5))` | `` |
| 130 | `mgcv::gam` | ordinary multidimensional tensor margins need no shape metadata | `y ~ te(x, z, w, bs = c("cr", "ds"), d = c(1, 2), k = c(4, 8))` | `` |
| 142 | `mgcv::scasm` | shape captions describe multidimensional tensor margins locally | `y ~ te(x, z, w, bs = c("sc", "ds"), d = c(1, 2), xt = list("m+", NULL), k = c(5, 8))` | `` |

## tests/testthat/test-scasm-integration.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 46 | `mgcv::scasm` | location-scale bootstrap limits use existing parameter transformations | `list(y ~ s(x, bs = "sc", xt = "c-", k = 6), ~s(z, bs = "sc", xt = "m+", k = 6))` | `mgcv::gammals()` |
| 146 | `mgcv::scasm` | bootstrap intervals omit absent factor levels without warnings | `y ~ fac + s(x, by = fac, bs = "sc", xt = "m+", k = 6)` | `` |

## tests/testthat/test-scasm-intervals.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-scasm-sampling.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-scasm-shape-intervals.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 141 | `mgcv::gam` | automatic shape enforcement rejects unjustified restrictions | `y ~ s(x, k = 6)` | `` |
| 171 | `mgcv::scasm` | factor by smooths and multiple predictors retain component identity | `y ~ fac + s(x, by = fac, bs = "sc", xt = "m+", k = 5)` | `` |
| 179 | `mgcv::scasm` | factor by smooths and multiple predictors retain component identity | `list(y ~ s(x, bs = "sc", xt = "m+", k = 5), ~s(z, bs = "sc", xt = "m+", k = 5))` | `mgcv::gaulss()` |
| 191 | `mgcv::scasm` | transformed covariates enforce shape in evaluated model coordinates | `y ~ s(log(x), bs = "sc", xt = "m+", k = 5)` | `` |

## tests/testthat/test-scasm-shape-review.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 9 | `mgcv::scasm` | general pc inequalities cannot masquerade as named shapes | `y ~ s(x, bs = "sc", xt = "m+", k = 6, pc = pc)` | `` |
| 22 | `mgcv::scasm` | general pc inequalities cannot masquerade as named shapes | `y ~ te(x, z, bs = c("sc", "bs"), k = c(5, 5), xt = list("m+", NULL), np = FALSE, pc = tensor_pc)` | `` |
| 37 | `mgcv::scasm` | point and general equality constraints retain named restrictions | `y ~ s(x, bs = "sc", xt = "m+", k = 6, pc = pc)` | `` |
| 54 | `mgcv::scasm` | automatic shape inference excludes derivative-valued margins | `y ~ s(x, bs = "sc", xt = "m+", k = 5)` | `` |
| 59 | `mgcv::scasm` | automatic shape inference excludes derivative-valued margins | `y ~ te(x, z, bs = c("sc", "bs"), k = c(5, 5), xt = list("m+", NULL), np = FALSE)` | `` |
| 79 | `mgcv::scasm` | curvature eligibility permits rounding but rejects small knot changes | `y ~ s(x, bs = "sc", xt = "c+", k = 6)` | `` |

## tests/testthat/test-sdmtmb-dependence.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 16 | `sdmTMB::sdmTMB` | File setup or helper | `formula` | `family` |

## tests/testthat/test-sdmtmb-diagnostic-adapters.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 6 | `sdmTMB::sdmTMB` | sdmTMB fitted diagnostics agree with native conditional calibration | `y ~ x` | `` |
| 21 | `sdmTMB::sdmTMB` | sdmTMB delta diagnostics calibrate the entire response mixture | `y ~ x` | `sdmTMB::delta_gamma()` |
| 59 | `sdmTMB::sdmTMB` | sdmTMB diagnostics identify unsupported dispersion formulas | `y ~ x` | `` |
| 69 | `sdmTMB::sdmTMB` | sdmTMB binomial counts use trial-sized probability bounds | `cbind(y, n - y) ~ x` | `binomial()` |
| 95 | `sdmTMB::sdmTMB` | sdmTMB diagnostics retain fitted IID effects | `y ~ x + (1 \| g)` | `` |
| 113 | `sdmTMB::sdmTMB` | sdmTMB spatial calibration retains the fitted field | `y ~ 1` | `` |

## tests/testthat/test-sdmtmb-student-predictive.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 36 | `sdmTMB::sdmTMB` | sdmTMB Student-t quantiles follow fixed and estimated degrees of freedom | `y ~ x` | `sdmTMB::student(df = df)` |
| 79 | `sdmTMB::sdmTMB` | Student-t fields preserve conditioning and reject infinite Pearson variance | `y ~ 1 + (1 \| group)` | `sdmTMB::student(df = 1.5)` |
| 115 | `sdmTMB::sdmTMB` | native predictive families need no analytic diagnostic constructor | `cbind(y, trials - y) ~ x` | `sdmTMB::betabinomial()` |
| 138 | `sdmTMB::sdmTMB` | predictive observations retain missing rows and delta component selection | `y ~ x` | `sdmTMB::delta_gamma()` |

## tests/testthat/test-sdmtmb-thresholds.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 38 | `sdmTMB::sdmTMB` | File setup or helper | `formula` | `if (delta) sdmTMB::delta_lognormal() else gaussian()` |

## tests/testthat/test-sdmtmb-time-varying.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 66 | `sdmTMB::sdmTMB` | File setup or helper | `formula` | `if (delta) sdmTMB::delta_lognormal() else gaussian()` |
| 329 | `sdmTMB::sdmTMB` | a covariate named Intercept has a distinct temporal selector | `y ~ Intercept` | `` |

## tests/testthat/test-sdmtmb.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 21 | `sdmTMB::sdmTMB` | File setup or helper | `formula` | `` |
| 205 | `sdmTMB::sdmTMB` | spatial and IID effects are excluded from formula plots | `y ~ z + s(x, k = 5) + (1 \| g)` | `` |
| 214 | `sdmTMB::sdmTMB` | spatial and IID effects are excluded from formula plots | `y ~ z + s(x, k = 5)` | `` |
| 282 | `sdmTMB::sdmTMB` | sdmTMB methods require recent fits made with RTMB | `y ~ s(x, k = 5)` | `` |

## tests/testthat/test-setup-data-recovery.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 47 | `mgcv::gam` | parametric effects preserve explicit local evaluation environments | `y ~ x + s(fun(z), k = 5)` | `` |
| 60 | `mgcv::gam` | parametric effects use retained observations after missing rows | `y ~ x + s(z, k = 5)` | `` |

## tests/testthat/test-shape-confidence-set.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-shape-intervals.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 345 | `mgcv::scasm` | model plots honor the covariance requested for shape intervals | `y ~ s(x, bs = "sc", xt = "m+", k = 5)` | `` |

## tests/testthat/test-shape-review-evidence.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-shape-review-integration.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 6 | `mgcv::scasm` | shape controls require scalar logical flags | `y ~ s(x, bs = "sc", xt = "m+", k = 6)` | `` |
| 34 | `mgcv::scasm` | captions distinguish pc replacements from requested shapes | `y ~ s(x, bs = "sc", xt = "m+", k = 6, pc = pc)` | `` |

## tests/testthat/test-shape-review-numerical.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-simulate-methods.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-simulation-quantiles.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 40 | `mgcv::gam` | unnamed simulation quantiles retain all scalar numerical results | `y ~ x` | `families[[i]]` |

## tests/testthat/test-simultaneous-intervals.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-smooth-bands.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-smooth-coefs.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-smooth-estimates-discrete.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 4 | `bam` | smooth_estimates matches discrete prediction for diamonds | `price ~ s(color, clarity, bs = "re")` | `` |
| 37 | `bam` | discrete random effects preserve margin order and by variables | `f` | `` |
| 61 | `bam` | discrete smooth evaluation only needs the selected predictors | `y ~ s(x) + s(a, b, bs = "re")` | `` |

## tests/testthat/test-smooth-estimates-empty-scam.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 14 | `scam::scam` | File setup or helper | `y ~ f + s(x, by = f, bs = "mpi", k = 6)` | `` |

## tests/testthat/test-smooth-estimates-empty.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 16 | `mgcv::gam` | File setup or helper | `y1 ~ f + s(x, by = f, k = 4) + s(z, k = 4)` | `` |

## tests/testthat/test-smooth-estimates.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 4 | `gam` | File setup or helper | `y ~ s(x0) + s(x1, bs = "cr") + s(x2, bs = "ps") + s(x3, bs = "bs")` | `` |
| 7 | `gamm` | File setup or helper | `y ~ s(x0) + s(x1, bs = "cr") + s(x2, bs = "ps") + s(x3, bs = "bs")` | `` |
| 11 | `gam` | File setup or helper | `y ~ t2(x0, x1, x2)` | `` |
| 12 | `gam` | File setup or helper | `y ~ s(x0) + s(x1) + ti(x0, x1)` | `` |
| 15 | `gam` | File setup or helper | `y ~ fac + s(x0, x1, by = fac)` | `` |
| 40 | `gam` | File setup or helper | `y ~ s(x1, fac, bs = "fs", k = 5)` | `` |

## tests/testthat/test-smooth-model-labels.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 16 | `gam` | distributional smooth labels survive selection and saved estimates | `list(y ~ s(x, k = 5), ~s(x, k = 5))` | `gaulss()` |
| 37 | `gam` | distributional smooth labels survive selection and saved estimates | `list(y ~ 1, ~s(x, k = 5))` | `gaulss()` |
| 46 | `gam` | model subtitles coexist with by labels and respect user labels | `list(y ~ fac + s(x, by = fac, k = 5), ~s(z, k = 5))` | `gaulss()` |
| 69 | `gam` | model subtitles coexist with by labels and respect user labels | `y ~ s(x, by = fac, k = 5)` | `` |
| 73 | `gam` | model subtitles coexist with by labels and respect user labels | `y ~ s(x, k = 5) + s(z, k = 5)` | `` |
| 83 | `gam` | surfaces, shared smooths and unknown families retain predictor identity | `list(y ~ s(x, z, k = 8), ~s(z, k = 5))` | `gaulss()` |
| 92 | `gam` | surfaces, shared smooths and unknown families retain predictor identity | `list(y ~ 1, ~1, 1 + 2 ~ s(x, k = 5) - 1)` | `multinom(K = 2)` |
| 97 | `gam` | surfaces, shared smooths and unknown families retain predictor identity | `list(y ~ s(x, k = 4), y2 ~ s(z, k = 4))` | `mvn(d = 2)` |
| 106 | `gam` | Tweedie and hurdle smooths identify the actual parameter order | `list(y ~ s(x, k = 4), ~s(z, k = 4), ~s(x, k = 4))` | `twlss()` |
| 111 | `gam` | Tweedie and hurdle smooths identify the actual parameter order | `list(y ~ s(x, k = 4), ~s(z, k = 4))` | `ziplss()` |
| 120 | `glmmTMB::glmmTMB` | glmmTMB smooths use the agreed component subtitles | `y ~ s(x, k = 5)` | `` |
| 130 | `glmmTMB::glmmTMB` | glmmTMB smooths use the agreed component subtitles | `y ~ s(x, k = 4)` | `poisson()` |
| 137 | `glmmTMB::glmmTMB` | glmmTMB smooths use the agreed component subtitles | `y ~ s(x, k = 4)` | `poisson()` |
| 142 | `glmmTMB::glmmTMB` | glmmTMB smooths use the agreed component subtitles | `y ~ s(x, k = 4)` | `glmmTMB::nbinom2()` |
| 155 | `glmmTMB::glmmTMB` | single-predictor glmmTMB smooths omit model subtitles | `y ~ s(x, k = 4) + s(z, k = 4)` | `fam` |
| 169 | `GJRM::gamlss` | GJRM embedded GAMs use parent distribution parameters | `list(y ~ s(x, k = 5) + s(z, k = 5))` | `` |
| 176 | `GJRM::gamlss` | GJRM embedded GAMs use parent distribution parameters | `list(y ~ s(x, k = 5), ~s(x, k = 5))` | `` |
| 186 | `GJRM::gamlss` | GJRM embedded GAMs use parent distribution parameters | `list(y ~ x, ~s(z, k = 5))` | `` |
| 193 | `do.call` | GJRM embedded GAMs use parent distribution parameters | `GJRM::gamlss` | `` |
| 200 | `do.call` | GJRM embedded GAMs use parent distribution parameters | `GJRM::gamlss` | `` |

## tests/testthat/test-smooth-plot-variables-sos.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 10 | `mgcv::gam` | spherical variable validation retains fitted transformed coordinates | `y ~ s(I(latitude + 0), I(longitude + 0), bs = "sos", k = 10)` | `` |

## tests/testthat/test-smooth-plot-variables.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 24 | `mgcv::gam` | File setup or helper | `stats::as.formula(paste("y ~ f +", term))` | `` |
| 170 | `mgcv::gam` | character groups in data slices retain the fitted factor order | `stats::formula(original)` | `` |
| 240 | `mgcv::gam` | comparison plots preserve distinct factor labels and pair order | `stats::formula(original)` | `` |
| 256 | `mgcv::gam` | saved differences validate factor conditioning roles | `y ~ f + te(x, z, condition, by = f, bs = c("cr", "cr", "re"), k = c(3, 3, 3))` | `` |
| 304 | `mgcv::gam` | invalid and incompatible variable selections fail clearly | `y ~ s(x, k = 5) + s(w, k = 5)` | `` |
| 348 | `mgcv::gam` | factor-smooth variable roles keep the continuous axis first | `y ~ s(f, x, bs = "fs", k = 4)` | `` |
| 362 | `mgcv::gam` | spherical smooths reject changing longitude and latitude roles | `y ~ s(longitude, latitude, bs = "sos", k = 10)` | `` |

## tests/testthat/test-smooth-type.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-soap-components.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 142 | `update` | soap component fitting is unaffected by attaching sf | `m_soap_sep` | `` |

## tests/testthat/test-soap-derivatives-offset.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 17 | `mgcv::gam` | File setup or helper | `z ~ s(x, y, bs = "so", k = 12, xt = list(bnd = bnd, nmax = 40))` | `` |
| 90 | `mgcv::gam` | finite differences without a smooth offset retain native estimates and uncertainty | `z ~ te(x, y, k = c(4, 4))` | `` |

## tests/testthat/test-soap-films.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-spherical-geometry.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-subsetting.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-surface-3d-intervals.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 53 | `mgcv::gam` | 3D interval controls are validated even for flat plots | `y ~ s(x, z, k = 10)` | `` |
| 120 | `mgcv::gam` | GAM entry points preserve transformed and stored interval heights | `y ~ te(x, z, k = c(4, 4)) + s(w, k = 4) + x:v` | `` |
| 172 | `mgcv::gam` | tensor types and higher dimensional TPRS retain bounds and facets | `stats::as.formula(paste("y ~", term))` | `` |
| 185 | `mgcv::gam` | parametric-only models and saved effects use the same interval renderer | `y ~ x:z + x:w:f` | `` |
| 208 | `mgcv::bam` | GAM fitting wrappers forward surface bounds | `y ~ s(x, z, k = 10) + x:w` | `` |
| 209 | `mgcv::gamm` | GAM fitting wrappers forward surface bounds | `y ~ s(x, z, k = 10) + x:w` | `` |
| 212 | `gamm4::gamm4` | GAM fitting wrappers forward surface bounds | `y ~ s(x, z, k = 10) + x:w` | `` |
| 226 | `scam::scam` | SCAM and scasm surface bounds retain their fitted intervals | `y ~ s(x, z, k = 10) + x:w` | `` |
| 230 | `mgcv::scasm` | SCAM and scasm surface bounds retain their fitted intervals | `y ~ te(x, z, bs = c("sc", "cr"), xt = list("m+", NULL), k = c(5, 5)) + x:w` | `` |
| 246 | `GJRM::gamlss` | GJRM shares vertical ranges across equations without changing covariates | `list(y ~ s(x, z, k = 10) + x:w, ~s(x, z, k = 10) + x:w + s(v, k = 4))` | `` |
| 278 | `glmmTMB::glmmTMB` | glmmTMB forwards bounds for smooth and parametric distributional effects | `y ~ s(x, z, k = 10) + x:w` | `` |
| 287 | `glmmTMB::glmmTMB` | glmmTMB forwards bounds for smooth and parametric distributional effects | `y ~ x:z` | `` |
| 297 | `sdmTMB::sdmTMB` | sdmTMB drawing and assembly forward bounds for both delta components | `positive ~ s(x, z, k = 8) + x:w` | `sdmTMB::delta_gamma()` |
| 307 | `sdmTMB::sdmTMB` | sdmTMB drawing and assembly forward bounds for both delta components | `y ~ x:z` | `` |
| 318 | `mgcv::gam` | soap and original-scale surfaces retain bound support | `y ~ te(log(x), z, k = c(3, 3))` | `` |

## tests/testthat/test-surface-3d.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 177 | `mgcv::gam` | all model and estimate entry points forward 3D surfaces | `y ~ te(x, z, k = c(4, 4)) + s(w, k = 4) + x:z:f` | `` |
| 205 | `mgcv::gam` | tensor types and higher dimensional slices render in 3D | `stats::as.formula(paste("y ~", term))` | `` |
| 246 | `mgcv::gam` | numeric interactions keep shared fixed ranges and transformed heights | `y ~ x:z + x:w` | `` |
| 297 | `mgcv::gam` | named view lists are validated without changing themes or other panels | `y ~ te(x, z, k = c(3, 3))` | `` |
| 326 | `mgcv::gam` | scattered direct estimates and model assembly share support masking | `y ~ te(x, z, k = c(3, 3))` | `` |
| 334 | `mgcv::bam` | scattered direct estimates and model assembly share support masking | `y ~ te(x, z, k = c(3, 3))` | `` |
| 340 | `mgcv::gam` | fixed model ranges span smooth and parametric surfaces after transformation | `y ~ te(x, z, k = c(3, 3)) + x:w` | `` |
| 433 | `mgcv::gam` | original-scale 3D smooths retain support masking and nested metadata | `y ~ te(log(x), z, k = c(3, 3))` | `` |

## tests/testthat/test-surrogate-residuals.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 41 | `mgcv::gam` | binary outcomes, absent categories and bam fits keep category indices | `y ~ x + offset(off)` | `mgcv::ocat(R = 2)` |
| 47 | `mgcv::bam` | binary outcomes, absent categories and bam fits keep category indices | `y ~ s(x, k = 6) + offset(off)` | `mgcv::ocat(R = 4)` |
| 95 | `mgcv::gam` | missing rows, subsetting and both offset forms stay aligned | `y ~ s(x, k = 6)` | `mgcv::ocat(R = 4)` |
| 107 | `mgcv::gam` | missing rows, subsetting and both offset forms stay aligned | `y ~ s(x, k = 6) + offset(off)` | `mgcv::ocat(R = 4)` |
| 189 | `mgcv::gam` | surrogate plots preserve missing-row alignment | `y ~ s(x, k = 6) + offset(off)` | `mgcv::ocat(R = 4)` |
| 239 | `mgcv::gam` | unsupported models and reference methods fail explicitly | `y ~ x` | `` |
| 252 | `mgcv::gam` | unsupported models and reference methods fail explicitly | `cbind(y, rep(1L, nrow(surrogate_test_data()))) ~ x` | `mgcv::gfam(list(gaussian()))` |

## tests/testthat/test-tensor-penalties-empty-rows.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 49 | `mgcv::gam` | tensor marginal labels distinguish parents and margins | `y ~ te(x, z, k = 4) + t2(x, z, k = 4)` | `` |
| 79 | `mgcv::gam` | fixed tensors and zero penalties retain the tidy schema | `y ~ te(x, z, k = 4, fx = TRUE)` | `` |
| 96 | `mgcv::gam` | empty B-spline prediction matrices preserve coefficient dimensions | `y ~ s(x, bs = "bs", k = 6)` | `` |
| 113 | `mgcv::gam` | factor-by B-splines omit absent levels without native spline errors | `y ~ fac + s(x, by = fac, bs = "bs", k = 6)` | `` |
| 139 | `mgcv::scasm` | constrained tensors and factor-by curves allow empty smooth rows | `form` | `` |

## tests/testthat/test-tensor-re-plots.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-transformed-derivatives.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 3 | `mgcv::gam` | smooth and raw derivatives obey the chain rule | `y ~ s(log(x), k = 7)` | `` |
| 26 | `mgcv::gam` | derivatives resolve local functions and require ambiguous focal inputs | `y ~ s(fun(x), k = 5)` | `` |
| 30 | `mgcv::gam` | derivatives resolve local functions and require ambiguous focal inputs | `y ~ s(I(x/z), k = 6)` | `` |
| 37 | `mgcv::gam` | partial derivatives reevaluate tensor margins on the raw scale | `y ~ te(log(x), sqrt(z), k = c(4, 4))` | `` |

## tests/testthat/test-transformed-model-variants.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 5 | `mgcv::gam` | other scalar bases and ordered by factors accept raw expressions | `f` | `` |
| 12 | `mgcv::gam` | other scalar bases and ordered by factors accept raw expressions | `y ~ g + s(log(x), by = g, k = 5)` | `` |
| 22 | `mgcv::gam` | matrix covariates retain their rows and summation convention | `y ~ s(log(X), by = L, k = 6)` | `` |
| 32 | `mgcv::gam` | literal expression-like names are distinct from calls in adapters | `y ~ s(x, k = 5)` | `` |
| 43 | `mgcv::gam` | multi-predictor models preserve expression and offset membership | `list(y ~ s(log(x), k = 5), ~s(sqrt(z), k = 5))` | `mgcv::gaulss()` |
| 61 | `GJRM::gamlss` | GJRM drawing forwards environments to embedded GAMs | `list(y ~ s(fun(x), k = 5), ~1)` | `"N"` |
| 72 | `mgcv::gam` | comparisons and factor differences accept transformed coordinates | `y ~ g + s(fun(x), by = g, k = 5)` | `` |
| 84 | `mgcv::bam` | scasm tensor constraints and discrete random effects are retained | `y ~ s(g, bs = "re") + s(log(x), k = 5)` | `` |
| 88 | `mgcv::scasm` | scasm tensor constraints and discrete random effects are retained | `y ~ te(log(x), sqrt(z), bs = c("sc", "sc"), xt = list("m+", "m+"), k = c(6, 6))` | `` |
| 100 | `mgcv::gam` | Poisson predictions retain transformed offsets across fitters | `y ~ s(log(x), k = 5) + offset(log(w))` | `poisson()` |
| 101 | `mgcv::bam` | Poisson predictions retain transformed offsets across fitters | `y ~ s(log(x), k = 5) + offset(log(w))` | `poisson()` |
| 102 | `mgcv::bam` | Poisson predictions retain transformed offsets across fitters | `y ~ s(log(x), k = 5) + offset(log(w))` | `poisson()` |
| 105 | `mgcv::scasm` | Poisson predictions retain transformed offsets across fitters | `y ~ s(log(x), bs = "sc", xt = "m+", k = 6) + offset(log(w))` | `poisson()` |
| 107 | `scam::scam` | Poisson predictions retain transformed offsets across fitters | `y ~ s(log(x), bs = "mpi", k = 5) + offset(log(w))` | `poisson()` |
| 126 | `mgcv::gam` | higher dimensional and spherical smooths evaluate expressions | `f` | `` |
| 137 | `scam::scam` | scam smooth reparameterization restores missing transformed rows | `y ~ s(log(x), bs = "mpi", k = 5)` | `` |

## tests/testthat/test-transformed-prediction.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 5 | `mgcv::gam` | full predictions evaluate local functions and offsets at new rows | `y ~ s(fun(x), k = 6) + offset(log(w * z))` | `` |
| 24 | `mgcv::gam` | prediction and sampling preserve transformed missing observations | `y ~ s(log(x), k = 5) + offset(log(w))` | `` |
| 33 | `mgcv::gam` | prediction and sampling preserve transformed missing observations | `y ~ s(log(x), k = 5)` | `` |
| 40 | `mgcv::gam` | data slices recover raw covariates without reversing transformations | `y ~ s(log(x), k = 5) + offset(log(w))` | `` |
| 51 | `mgcv::gam` | offset overrides are structural and do not rename transformed values | `y ~ s(log(x), k = 5) + offset(log(w))` | `` |
| 55 | `mgcv::gam` | offset overrides are structural and do not rename transformed values | `y ~ s(x, k = 5) + offset(log(w * z))` | `` |
| 63 | `mgcv::gam` | prediction metadata retains fitted polynomial transformations | `y ~ poly(x, 2) + s(log(z), k = 5)` | `` |
| 73 | `mgcv::gam` | all missing or infinite transformed predictors preserve output rows | `y ~ s(log(x), k = 5)` | `` |
| 89 | `lm` | the expression engine preserves multiple offsets in terms metadata | `y ~ x + offset(log(w)) + offset(log(z))` | `` |

## tests/testthat/test-transformed-smooths.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 18 | `mgcv::gam` | transformed smooth matrices match native predictions across fitters | `f` | `` |
| 19 | `mgcv::bam` | transformed smooth matrices match native predictions across fitters | `f` | `` |
| 20 | `mgcv::bam` | transformed smooth matrices match native predictions across fitters | `f` | `` |
| 46 | `mgcv::gamm` | gamm and gamm4 wrappers evaluate raw transformed data | `y ~ s(log(x), k = 5)` | `` |
| 48 | `gamm4::gamm4` | gamm and gamm4 wrappers evaluate raw transformed data | `y ~ s(log(x), k = 5)` | `` |
| 61 | `mgcv::scasm` | shape constrained fits accept transformed covariates | `y ~ s(log(x), bs = "sc", xt = "m+", k = 5)` | `` |
| 70 | `scam::scam` | shape constrained fits accept transformed covariates | `f` | `` |
| 82 | `mgcv::gam` | local functions, basis values, sampling, and direct spline calls agree | `y ~ s(fun(x), k = 5)` | `` |
| 110 | `mgcv::gam` | selected smooths ignore unrelated data and preserve missing rows | `y ~ s(log(x), k = 5) + s(z, k = 5) + offset(log(w))` | `` |
| 127 | `mgcv::gam` | standalone bases use explicit environments for local functions | `y ~ s(fun(x), k = 5)` | `` |

## tests/testthat/test-tube-intervals.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 184 | `mgcv::gam` | tube wrappers forward controls and unsupported requests fail | `y ~ s(x, z, k = 10)` | `` |
| 186 | `mgcv::gam` | tube wrappers forward controls and unsupported requests fail | `y ~ s(x, by = z, k = 6)` | `` |

## tests/testthat/test-tube-regressions.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 6 | `mgcv::gam` | tube partitions merge rounding copies without changing the domain | `y ~ s(x, bs = "cr", k = 5)` | `` |
| 27 | `mgcv::gam` | tube model wrappers and constant numeric by terms retain predictions | `y ~ s(x, by = z, bs = "cr", k = 6)` | `` |
| 44 | `mgcv::bam` | tube model wrappers and constant numeric by terms retain predictions | `y ~ s(x, bs = "cr", k = 6)` | `` |
| 48 | `mgcv::gamm` | tube model wrappers and constant numeric by terms retain predictions | `y ~ s(x, bs = "cr", k = 6)` | `` |
| 57 | `gamm4::gamm4` | tube model wrappers and constant numeric by terms retain predictions | `y ~ s(x, bs = "cr", k = 6)` | `` |
| 67 | `mgcv::gam` | tube metadata survive nested estimates and use smooth coordinates | `y ~ s(x, bs = "cr", k = 6) + s(z, bs = "cr", k = 5)` | `` |
| 77 | `mgcv::gam` | tube metadata survive nested estimates and use smooth coordinates | `y ~ s(log(x), bs = "cr", k = 6)` | `` |
| 111 | `mgcv::gam` | tube calibration does not create an absent random-number state | `y ~ s(x, bs = "cr", k = 6)` | `` |
| 121 | `mgcv::gam` | subsampled thin plate knots do not truncate the coverage domain | `y ~ s(x, k = 6, xt = list(max.knots = 12))` | `` |
| 131 | `mgcv::gam` | tube contrasts reject coordinate-dependent group contributions at a single point | `y ~ group * x + s(x, by = group, bs = "cr", k = 6)` | `` |

## tests/testthat/test-tweedie-integration.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 65 | `mgcv::gam` | public diagnostics use mixtures without the optional backend | `y ~ x` | `fam` |

## tests/testthat/test-tweedie-mixture.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-utilities.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 80 | `gam` | File setup or helper | `crime ~ s(district, bs = "mrf", xt = xt)` | `` |
| 286 | `gam` | fix_offset can replace and offset only if there is one | `y ~ s(x0) + s(x1) + offset(x2)` | `` |
| 326 | `gam` | parametric_terms works for a gaulss GAM | `list(accel ~ s(times), ~s(times))` | `gaulss()` |
| 335 | `gam` | parametric_terms works for a gaussian GAM | `accel ~ s(times)` | `gaussian()` |

## tests/testthat/test-variance-components-formats.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 47 | `gam` | variance_comp agrees with gam.vcomp for ordinary GAM fits | `y ~ s(x, k = 8) + s(z, k = 8)` | `` |
| 48 | `gam` | variance_comp agrees with gam.vcomp for ordinary GAM fits | `y ~ s(x, k = 8) + s(z, k = 8)` | `` |
| 49 | `gam` | variance_comp agrees with gam.vcomp for ordinary GAM fits | `y ~ s(x, k = 8) + s(z, k = 8)` | `` |
| 50 | `gam` | variance_comp agrees with gam.vcomp for ordinary GAM fits | `y ~ s(x, k = 8)` | `` |
| 51 | `gam` | variance_comp agrees with gam.vcomp for ordinary GAM fits | `y ~ te(x, z, k = c(4, 4))` | `` |
| 52 | `gam` | variance_comp agrees with gam.vcomp for ordinary GAM fits | `y ~ s(x, k = 8) + s(z, k = 8)` | `` |
| 54 | `gam` | variance_comp agrees with gam.vcomp for ordinary GAM fits | `y ~ s(x, k = 5, fx = TRUE) + s(z, k = 8)` | `` |
| 68 | `gam` | shared smoothing parameters are returned for every penalty | `y ~ s(x, k = 8, id = 1) + s(z, k = 8, id = 1)` | `` |
| 85 | `gam` | fixed smoothing parameters retain scale uncertainty | `y ~ s(x, k = 8) + s(z, k = 8)` | `` |
| 106 | `gam` | fixed smoothing parameters retain scale uncertainty | `y ~ s(x, k = 8) + s(z, k = 8)` | `` |
| 121 | `gam` | parametric penalties include estimated, fixed and mapped parameters | `f` | `` |
| 133 | `gam` | parametric penalties include estimated, fixed and mapped parameters | `y ~ group` | `` |
| 151 | `bam` | bam uses complete penalty values and mappings | `y ~ s(x, k = 8) + s(z, k = 8)` | `` |
| 153 | `bam` | bam uses complete penalty values and mappings | `y ~ s(x, k = 8, id = 1) + s(z, k = 8, id = 1)` | `` |
| 155 | `bam` | bam uses complete penalty values and mappings | `y ~ s(x, k = 8) + s(z, k = 8)` | `` |
| 157 | `bam` | bam uses complete penalty values and mappings | `y ~ s(x, k = 8) + s(z, k = 8)` | `` |
| 159 | `bam` | bam uses complete penalty values and mappings | `y ~ group + s(x, k = 8)` | `` |
| 204 | `gam` | family parameters are excluded from the interval transformation | `f` | `nb()` |
| 214 | `gam` | fixed shared parameters are expanded for both GAM fitters | `f` | `` |
| 215 | `bam` | fixed shared parameters are expanded for both GAM fitters | `f` | `` |
| 216 | `bam` | fixed shared parameters are expanded for both GAM fitters | `f` | `` |
| 231 | `fit` | custom parametric mappings also work with bam and fixed parameters | `y ~ group` | `` |
| 250 | `gam` | unpenalized models return an empty variance component table | `y ~ s(x, k = 5, fx = TRUE)` | `` |

## tests/testthat/test-variance-components-gamm.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 33 | `gamm` | gamm smooth components agree with the GAM penalty scale | `y ~ s(x, k = 6) + s(z, k = 6)` | `` |
| 35 | `gam` | gamm smooth components agree with the GAM penalty scale | `y ~ s(x, k = 6) + s(z, k = 6)` | `` |
| 60 | `gamm` | gamm native random coefficients keep their variances and intervals | `y ~ s(x, k = 6) + s(z, k = 6)` | `` |
| 85 | `gamm` | gamm maps factor smooths and tensor penalties back to terms | `y ~ t2(x, z, k = c(4, 4))` | `` |
| 88 | `gamm` | gamm maps factor smooths and tensor penalties back to terms | `y ~ s(x, k = 6) + s(group, bs = "re")` | `` |
| 90 | `gamm` | gamm maps factor smooths and tensor penalties back to terms | `y ~ s(z, k = 5, fx = TRUE) + s(x, k = 6)` | `` |
| 107 | `gamm` | gamm handles random-only models and blocked native covariance | `y ~ x + z` | `` |
| 113 | `gamm` | gamm handles random-only models and blocked native covariance | `y ~ x + s(z, k = 6)` | `` |
| 132 | `gamm` | gamm respects residual weights, correlation and unavailable intervals | `y ~ s(x, k = 6) + s(z, k = 6)` | `` |
| 147 | `gamm` | gamm respects residual weights, correlation and unavailable intervals | `y ~ s(x, k = 6)` | `` |
| 157 | `gamm` | gamm PQL components retain the fitted working-model scale | `count ~ s(x, k = 6)` | `poisson()` |
| 182 | `gamm` | gamm intervals handle shared native variances and alternative coordinates | `y ~ s(x, k = 6) + s(z, k = 6)` | `` |
| 197 | `gamm` | gamm intervals handle shared native variances and alternative coordinates | `y ~ s(x, k = 6) + s(z, k = 6)` | `` |

## tests/testthat/test-variance-components-gamm4.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 17 | `gamm4::gamm4` | gamm4 smooth estimates use the common GAM penalty scale | `y ~ s(x, k = 5) + s(z, k = 5)` | `` |
| 19 | `gam` | gamm4 smooth estimates use the common GAM penalty scale | `y ~ s(x, k = 5) + s(z, k = 5)` | `` |
| 20 | `bam` | gamm4 smooth estimates use the common GAM penalty scale | `y ~ s(x, k = 5) + s(z, k = 5)` | `` |
| 21 | `gamm` | gamm4 smooth estimates use the common GAM penalty scale | `y ~ s(x, k = 5) + s(z, k = 5)` | `` |
| 51 | `gamm4::gamm4` | gamm4 maps every smooth penalty to its fitted block | `y ~ s(x, k = 5) + s(group, bs = "re")` | `` |
| 52 | `gamm4::gamm4` | gamm4 maps every smooth penalty to its fitted block | `y ~ s(x, k = 5, fx = TRUE) + s(z, k = 5)` | `` |
| 81 | `gamm4::gamm4` | gamm4 native blocks retain separate variance labels | `y ~ s(z, k = 5)` | `` |
| 91 | `gamm4::gamm4` | gamm4 native blocks retain separate variance labels | `y ~ x` | `` |
| 126 | `gamm4::gamm4` | gamm4 profiles generalized fits with a fixed scale | `count ~ s(x, k = 5)` | `fam` |
| 181 | `gamm4::gamm4` | gamm4 profiles separate t2 penalties | `y ~ t2(x, z, k = c(4, 4))` | `` |
| 200 | `gamm4::gamm4` | gamm4 preserves estimates when GLMM scale profiling is unavailable | `y ~ s(x, k = 5)` | `Gamma(link = "log")` |

## tests/testthat/test-variance-components-include-total.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 39 | `fit` | total rows can be omitted without changing individual term results | `y ~ total + z + s(w, k = 6)` | `` |
| 57 | `gam` | omitting totals handles several predictors and intercept-only models | `list(y ~ x, ~1, 1 + 2 ~ 0 + s(w, k = 6))` | `gaulss()` |
| 71 | `gamm` | mixed models can omit totals while retaining fitted random effects | `y ~ x + z + s(w, k = 6)` | `` |
| 73 | `gamm4::gamm4` | mixed models can omit totals while retaining fitted random effects | `y ~ x + z + s(w, k = 6)` | `` |
| 83 | `scam::scam` | SCAM term results can omit totals | `y ~ x + s(w, bs = "mpi", k = 6)` | `` |

## tests/testthat/test-variance-components-proportions.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 67 | `gam` | signed allocations and their intervals retain values outside zero to one | `y ~ x + z` | `` |
| 87 | `gam` | zero total variances have undefined proportions and defined draws set intervals | `y ~ x` | `` |
| 93 | `gam` | zero total variances have undefined proportions and defined draws set intervals | `y ~ 1 + offset(off)` | `` |
| 100 | `gam` | zero total variances have undefined proportions and defined draws set intervals | `y ~ x + z` | `` |
| 121 | `gam` | each predictor has separate allocations for its own and shared terms | `list(y ~ x, ~z, 1 + 2 ~ 0 + s(w, k = 6))` | `gaulss()` |
| 153 | `gam` | each predictor has separate allocations for its own and shared terms | `list(y ~ x + z, ~1)` | `gaulss()` |

## tests/testthat/test-variance-components-term-intervals.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 6 | `fit` | term intervals use joint coefficient draws and posterior quantiles | `y ~ x + z + fac * poly(w, 2) + s(w, k = 6) + offset(off)` | `` |
| 27 | `gam` | term interval controls are reproducible and validate inputs | `y ~ x + s(w, k = 6)` | `` |
| 66 | `bam` | term draws retain fitting rows, whole smooths and multiple predictors | `y ~ x + s(w, k = 6)` | `` |
| 68 | `gam` | term draws retain fitting rows, whole smooths and multiple predictors | `list(y ~ s(w, k = 6), ~x + z)` | `gaulss()` |
| 69 | `gamm` | term draws retain fitting rows, whole smooths and multiple predictors | `y ~ te(x, w, k = c(4, 4))` | `` |
| 70 | `gamm4::gamm4` | term draws retain fitting rows, whole smooths and multiple predictors | `y ~ t2(x, w, k = c(4, 4))` | `` |
| 160 | `gamm` | generalized and nested mixed models have posterior term intervals | `count ~ x + s(w, k = 6)` | `poisson()` |
| 162 | `gamm4::gamm4` | generalized and nested mixed models have posterior term intervals | `count ~ x + s(w, k = 6)` | `poisson()` |
| 196 | `gam` | term intervals include fixed, shared and parametric penalties | `y ~ x + s(w, k = 6, sp = 0.7)` | `` |
| 198 | `bam` | term intervals include fixed, shared and parametric penalties | `y ~ x + s(w, by = fac, id = 1, k = 6)` | `` |
| 212 | `gamm` | lme term intervals retain residual weights and correlation | `y ~ x + z + fac + s(w, k = 6)` | `` |
| 230 | `gamm4::gamm4` | zero random-effect variances give zero term intervals | `y ~ s(x, k = 6)` | `` |

## tests/testthat/test-variance-components-terms.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 37 | `fit` | term variances include complete parametric and smooth terms | `y ~ poly(z, 2) + fac * x + s(w, k = 6) + offset(off)` | `` |
| 47 | `gam` | term variances include complete parametric and smooth terms | `y ~ x + z` | `` |
| 68 | `do.call` | term averaging uses fitting observations rather than likelihood weights | `fit` | `` |
| 111 | `gamm` | mixed-model term covariances exclude varying formula offsets | `y ~ x + z + fac + s(w, k = 6) + offset(off)` | `` |
| 113 | `gamm4::gamm4` | mixed-model term covariances exclude varying formula offsets | `y ~ x + z + fac + s(w, k = 6) + offset(off)` | `` |
| 154 | `gamm` | PQL and generalized mixed contributions are on the link scale | `count ~ x + s(w, k = 6)` | `poisson()` |
| 156 | `gamm4::gamm4` | PQL and generalized mixed contributions are on the link scale | `count ~ x + s(w, k = 6)` | `poisson()` |
| 182 | `gamm` | nested and separate native blocks retain all contributions | `y ~ x + s(w, k = 6)` | `` |
| 189 | `gamm4::gamm4` | nested and separate native blocks retain all contributions | `y ~ x + s(w, k = 6)` | `` |
| 204 | `gam` | term variances handle empty and distributional models | `list(y ~ s(x, k = 6), ~z)` | `gaulss()` |

## tests/testthat/test-variance-components-totals.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 32 | `gam` | negative covariance reduces the total in the fitted values and draws | `y ~ x + z` | `` |
| 45 | `gam` | a single term and an intercept-only predictor have consistent totals | `y ~ x` | `` |
| 51 | `gam` | a single term and an intercept-only predictor have consistent totals | `y ~ 1 + offset(off)` | `` |
| 67 | `gam` | total labels remain distinct from formula term names | `y ~ total + s(w, k = 6)` | `` |
| 79 | `gam` | distributional GAMs have one total per predictor, including shared terms | `list(y ~ x + z + offset(off), ~1)` | `gaulss()` |
| 80 | `gam` | distributional GAMs have one total per predictor, including shared terms | `list(y ~ x + s(w, k = 6), ~s(z, k = 6))` | `gaulss()` |
| 81 | `gam` | distributional GAMs have one total per predictor, including shared terms | `list(y ~ 1, ~1, 1 + 2 ~ 0 + s(w, k = 6))` | `gaulss()` |
| 82 | `gam` | distributional GAMs have one total per predictor, including shared terms | `list(y ~ 1, ~1, 1 + 2 ~ 0 + x)` | `gaulss()` |

## tests/testthat/test-variance-components.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.

## tests/testthat/test-worm-plot-seed.R

Retain the specialized cases listed below unless README.md names a further replacement. Preserve the listed formula, family and test property when choosing a shared substitute.

| Line | Constructor or dynamic call | Test or helper | Formula or first argument | Family |
| --- | --- | --- | --- | --- |
| 5 | `gam` | worm_plot honours seeds and preserves caller RNG state | `y ~ s(x, k = 6)` | `poisson()` |

## tests/testthat/test-worm-plot.R

No direct fitting constructor or dynamic fitting call was found. This file uses existing models, calls a fixture provider, or does not need a fitted model.
