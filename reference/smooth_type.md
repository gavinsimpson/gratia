# Determine the type of smooth and return it in a human readable form

Soap-film boundary and wiggly components use the `sf.film` and
`sf.wiggly` classes introduced in mgcv 1.9-4.

## Usage

``` r
smooth_type(smooth)

# Default S3 method
smooth_type(smooth)

# S3 method for class 'tprs.smooth'
smooth_type(smooth)

# S3 method for class 'ts.smooth'
smooth_type(smooth)

# S3 method for class 'cr.smooth'
smooth_type(smooth)

# S3 method for class 'cs.smooth'
smooth_type(smooth)

# S3 method for class 'cyclic.smooth'
smooth_type(smooth)

# S3 method for class 'pspline.smooth'
smooth_type(smooth)

# S3 method for class 'cpspline.smooth'
smooth_type(smooth)

# S3 method for class 'Bspline.smooth'
smooth_type(smooth)

# S3 method for class 'duchon.spline'
smooth_type(smooth)

# S3 method for class 'fs.interaction'
smooth_type(smooth)

# S3 method for class 'sz.interaction'
smooth_type(smooth)

# S3 method for class 'gp.smooth'
smooth_type(smooth)

# S3 method for class 'mrf.smooth'
smooth_type(smooth)

# S3 method for class 'random.effect'
smooth_type(smooth)

# S3 method for class 'sf.wiggly'
smooth_type(smooth)

# S3 method for class 'sf.film'
smooth_type(smooth)

# S3 method for class 'soap.film'
smooth_type(smooth)

# S3 method for class 't2.smooth'
smooth_type(smooth)

# S3 method for class 'sos.smooth'
smooth_type(smooth)

# S3 method for class 'tensor.smooth'
smooth_type(smooth)

# S3 method for class 'mpi.smooth'
smooth_type(smooth)

# S3 method for class 'mpd.smooth'
smooth_type(smooth)

# S3 method for class 'cx.smooth'
smooth_type(smooth)

# S3 method for class 'cv.smooth'
smooth_type(smooth)

# S3 method for class 'micx.smooth'
smooth_type(smooth)

# S3 method for class 'micv.smooth'
smooth_type(smooth)

# S3 method for class 'mdcx.smooth'
smooth_type(smooth)

# S3 method for class 'mdcv.smooth'
smooth_type(smooth)

# S3 method for class 'miso.smooth'
smooth_type(smooth)

# S3 method for class 'mifo.smooth'
smooth_type(smooth)

# S3 method for class 'po.smooth'
smooth_type(smooth)

# S3 method for class 'ipo.smooth'
smooth_type(smooth)

# S3 method for class 'dpo.smooth'
smooth_type(smooth)

# S3 method for class 'cpopspline.smooth'
smooth_type(smooth)

# S3 method for class 'lmpi.smooth'
smooth_type(smooth)

# S3 method for class 'lipl.smooth'
smooth_type(smooth)

# S3 method for class 'tedmi.smooth'
smooth_type(smooth)

# S3 method for class 'tedmd.smooth'
smooth_type(smooth)

# S3 method for class 'temicx.smooth'
smooth_type(smooth)

# S3 method for class 'temicv.smooth'
smooth_type(smooth)

# S3 method for class 'tedecx.smooth'
smooth_type(smooth)

# S3 method for class 'tedecv.smooth'
smooth_type(smooth)

# S3 method for class 'tecxcx.smooth'
smooth_type(smooth)

# S3 method for class 'tecvcv.smooth'
smooth_type(smooth)

# S3 method for class 'tecxcv.smooth'
smooth_type(smooth)

# S3 method for class 'tesmi1.smooth'
smooth_type(smooth)

# S3 method for class 'tesmi2.smooth'
smooth_type(smooth)

# S3 method for class 'tesmd1.smooth'
smooth_type(smooth)

# S3 method for class 'tesmd2.smooth'
smooth_type(smooth)

# S3 method for class 'tescx.smooth'
smooth_type(smooth)

# S3 method for class 'tescv.smooth'
smooth_type(smooth)

# S3 method for class 'tismi.smooth'
smooth_type(smooth)

# S3 method for class 'tismd.smooth'
smooth_type(smooth)

# S3 method for class 'mpiBy.smooth'
smooth_type(smooth)

# S3 method for class 'mpdBy.smooth'
smooth_type(smooth)

# S3 method for class 'cxBy.smooth'
smooth_type(smooth)

# S3 method for class 'cvBy.smooth'
smooth_type(smooth)

# S3 method for class 'micxBy.smooth'
smooth_type(smooth)

# S3 method for class 'micvBy.smooth'
smooth_type(smooth)

# S3 method for class 'mdcxBy.smooth'
smooth_type(smooth)

# S3 method for class 'mdcvBy.smooth'
smooth_type(smooth)
```

## Arguments

- smooth:

  an object inheriting from class `mgcv.smooth`.

## Details

SCAM smooth types use compact constraint descriptions: `inc` for
increasing, `dec` for decreasing, and `pos` for positive. For tensor
smooths, the two descriptions follow the covariate order in the smooth
specification; `free` denotes an unconstrained direction. `Tensor int.`
identifies an interaction. Explicit `By` bases share the type label of
the corresponding ordinary basis.
