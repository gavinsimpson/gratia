# Fitting offsets and prescribed smooth values

The integration tests use real soap-film smooths with prescribed boundary values, formula offsets, fitting-argument offsets, unequal weights, and excluded observations. The tests distinguish the likelihood used by the fitted model from prediction at supplied data.

In installed mgcv 1.9-4, gam stores the boundary contribution in its fitting offset. Ordinary bam stores the formula and argument offsets without that boundary contribution in these examples. Independent Gaussian calculations reconstruct bam's fitted coefficient mode and covariance only when using its actual stored fitting offset. Native bam prediction nevertheless adds the prescribed boundary contribution. Adding that contribution to the posterior likelihood would change the target used for fitting.

MH targets and the smoothing-uncertainty prototype therefore reject BAM fits with nonzero prescribed smooth contributions until the fitting and prediction targets can be reconciled. The errors identify this inconsistency. Gaussian approximation and supplied-coefficient prediction retain native prediction behaviour. The large-model MH setup probes one fitting row for a fixed-contribution attribute; ordinary BAM models need no additional design pass. If that attribute exists but is initially zero, remaining fitting rows are checked in bounded blocks.

Ordinary gam targets retain their actual stored offsets exactly. Independent tests compare compiled, R, and family-callback targets, check gradients by numerical differences, and verify the exact Gaussian conditional. Ordinary and discrete BAM models without fixed contributions retain their existing target at original covariate values.

Prediction is a separate calculation. At supplied evaluation data, formula offsets and prescribed smooth values must both be added to the matrix product. Real gaulss and grouped Gaussian-family tests verify expected responses and seeded posterior response draws against independent calculations, including a gaulss derivative calculation. This is where prediction_offsets() is required.

A call-level offset column alone cannot establish a fitting offset: general.family fitters can retain that column while ignoring the argument in their likelihood. The family posterior target keeps stored fitting components unchanged, using prediction metadata only for missing components.
