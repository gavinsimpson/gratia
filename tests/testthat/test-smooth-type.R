# Test smooth_type

test_that("smooth_type default method throws error", {
    expect_error(smooth_type(1), "Unknown type of smooth")
})

test_that("smooth_type works for TPRS smooths", {
    expect_silent(st <- smooth_type(get_smooth(m_gam, "s(x0)")))
    expect_identical(st, "TPRS")
})

test_that("smooth_type works for CRS smooths", {
    expect_silent(st <- smooth_type(get_smooth(su_m_penalty, "s(x0)")))
    expect_identical(st, "CRS")
})

test_that("smooth_type works for B spline smooths", {
    expect_silent(st <- smooth_type(get_smooth(su_m_penalty, "s(x1)")))
    expect_identical(st, "B spline")
})

test_that("smooth_type works for P spline smooths", {
    expect_silent(st <- smooth_type(get_smooth(su_m_penalty, "s(x3)")))
    expect_identical(st, "P spline")
})

test_that("smooth_type works for CRS shrinkage smooths", {
    expect_silent(st <- smooth_type(get_smooth(su_m_quick_eg1_shrink, "s(x1)")))
    expect_identical(st, "CRS (shrink)")
})

test_that("smooth_type works for TPRS shrinkage smooths", {
    expect_silent(st <- smooth_type(get_smooth(su_m_quick_eg1_shrink, "s(x0)")))
    expect_identical(st, "TPRS (shrink)")
})

test_that("smooth_type works for TPRS shrinkage smooths", {
    expect_silent(st <- smooth_type(get_smooth(su_m_quick_eg1_shrink, "s(x0)")))
    expect_identical(st, "TPRS (shrink)")
})

test_that("smooth_type works for te smooths", {
    expect_silent(st <- smooth_type(get_smooth(su_m_bivar_te, "te(x,z)")))
    expect_identical(st, "Tensor product")
})

test_that("smooth_type works for ti smooths", {
    expect_silent(st <- smooth_type(get_smooth(su_m_bivar_ti, "ti(x,z)")))
    expect_identical(st, "Tensor product int.")
})

test_that("smooth_type works for t2 smooths", {
    expect_silent(st <- smooth_type(get_smooth(su_m_bivar_t2, "t2(x,z)")))
    expect_identical(st, "Tensor product (T2)")
})

test_that("smooth_type works for sz smooths", {
    expect_silent(st <- smooth_type(get_smooth(m_sz, "s(fac,x2)")))
    expect_identical(st, "Constr. factor smooth")
})

test_that("smooth_type works for re smooths", {
    expect_silent(st <- smooth_type(get_smooth(rm1, "s(fac)")))
    expect_identical(st, "Random effect")
})

test_that("smooth_type works for sos smooths", {
    expect_silent(st <- smooth_type(get_smooth(m_sos, "s(latitude,longitude)")))
    expect_identical(st, "SOS")
})

test_that("smooth_type works for duchon spline smooths", {
    expect_silent(st <- smooth_type(get_smooth(su_m_bivar_ds, "s(x,z)")))
    expect_identical(st, "Duchon spline (2d)")
})

# SCAM smooths
test_that("smooth_type works for scam monotone decreasing smooths", {
    expect_silent(st <- smooth_type(get_smooth(sw, "s(Depth)")))
    expect_identical(st, "Mono. decr.")
})

test_that("smooth_type works for scam monotone decreasing smooths", {
    expect_silent(st <- smooth_type(get_smooth(sw_mdcx, "s(Depth)")))
    expect_identical(st, "Mono. decr. conv.")
})

test_that("smooth_type works for scam monotone decreasing smooths", {
    expect_silent(st <- smooth_type(get_smooth(sw_mdcv, "s(Depth)")))
    expect_identical(st, "Mono. decr. conc.")
})

test_that("smooth_type works for scam monotone increasing smooths", {
    expect_silent(st <- smooth_type(get_smooth(m_scam, "s(x2)")))
    expect_identical(st, "Mono. incr.")
})

test_that("smooth_type works for scam monotone increasing smooths", {
    expect_silent(st <- smooth_type(get_smooth(m_scam_micx, "s(x2)")))
    expect_identical(st, "Mono. incr. conv.")
})

test_that("smooth_type works for scam monotone increasing smooths", {
    expect_silent(st <- smooth_type(get_smooth(m_scam_micv, "s(x2)")))
    expect_identical(st, "Mono. incr. conc.")
})
