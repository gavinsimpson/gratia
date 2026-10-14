# Prediction article validation

Source gratia 0.11.2.49 ; mgcv 1.9.4
Executed the entire article with pkgload current source and rendered it outside the sandbox.
All numerical comparisons below use native mgcv prediction and covariance methods or direct algebra.

| Independent comparison | Maximum absolute difference |
| --- | ---: |
| Response fitted values vs exp(predict.gam link) | 0 |
| Fitted link SE vs predict.gam se.fit | 0 |
| Response treatment difference vs native fitted means | 0 |
| Delta contrast SE vs inverse-link gradient and vcov | 8.88e-16 |
| Shared posterior contrast SE vs direct within-draw subtraction | 1.33e-15 |
| Shared posterior contrast quantiles vs direct calculation | 2.51e-14 |
| Automatic response derivative vs native central difference, h=0.01 | 2.79e-10 |
| Response derivative SE vs direct gradient finite difference | 3.86e-11 |
| Automatic second response derivative vs native central difference | 3.17e-09 |
| Raw concentration derivative vs log-coordinate derivative / concentration | 1.35e-12 |

At concentration 500, the fitted nonchilled-minus-chilled differences and delta SEs are:
        Type difference       se
      Quebec    5.91622 3.776065
 Mississippi   11.72469 2.448401

Verified the observed-factor grid contains exactly 12 plant/origin/treatment combinations and 84 predictions.
Verified na.exclude restores 84 default fitting rows with the excluded response prediction missing;
explicit missing-response rows predict successfully while missing concentration retains an NA row.
Verified five fitted posterior draws restore all 84 row positions, including the missing row in every draw.
Verified gaulss mean and scale link selections execute.

No implementation defect required a code change or regression test. Render corrections used documented envir
to locate fitting data in the article evaluation environment and explicitly added measured responses to
the display because default fitted_values returns predictors, not the response.

Render output: /private/tmp/gratia-prediction-render/prediction-comparisons.html.
All seven rendered PNG plots were inspected: observation and prediction facets,
conditional and smooth difference bands, delta/simulation rate comparison,
raw-axis smooth and parametric effects. Legends, facets and interval labels
were readable. The raw-axis effect plots were given concentration units and
explicit log-uptake contribution labels after this inspection.

Remaining bounds: detailed tensor, transformed-offset and local-function examples are in the existing
transformed-covariates vignette. No separate model was fitted for every supported family. Intervals
describe conditional means for zero plant effects and finite evaluation grids; they do not establish
population-marginal means, equivalence, monotonicity or unobserved-range effects.
