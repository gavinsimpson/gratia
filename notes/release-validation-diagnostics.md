# Diagnostics and grouped-survey article validation

All execution, rendering and numerical checks ran outside the sandbox. The
scripts load this checkout; installed gratia 0.11.2.34 was not used.
`notes/validate-release-articles.R` contains reproducible independent calculations,
separate from the user-facing articles.

| Comparison | Maximum absolute difference |
|---|---:|
| gfam means from explicit binomial/Poisson inverse links | 1.11e-16 |
| gfam means from native mgcv predictions | 1.11e-16 |
| gfam response draws from direct R binomial/Poisson generators | 0 |
| Poisson PIT from native CDF | 3.33e-16 |
| Poisson normal scores from native CDF | 4.06e-13 |
| lme conditional Gaussian scores from fitted means and residual SD | 8.04e-14 |
| Precision-weighted Gamma PIT from native CDF | 2.22e-16 |
| Censored normal PIT from native interval probabilities | 1.11e-16 |
| Binomial PIT from probabilities spanning one success | 1.11e-16 |

With 2000 Gaussian AR1 response draws, correlations at the first three fitting
positions matched rho^lag within 0.04; correlation across series boundaries
was below 0.05. Scalable heatmap counts retained all 12000 observations.
Working-AR1 BAM QQ scores omitted bands, and requested joint simulation,
simulated QQ references and PPCs produced errors as documented.

Native mgcv grouped response prediction initially disagreed because its response
expression could not be evaluated from prediction data containing only the
membership variable. Supplying an unused response placeholder and membership
in the two-column expression resolves this; the article now demonstrates the
correct native encoding. Explicit gratia family_index predictions agreed with
independent inverse links throughout. This was a documentation correction,
not an implementation change.

The complete gfam, model-checking and response-distributions articles rendered
successfully with rmarkdown using current source. The 8, 21 and 20 original
plots were extracted and visually inspected. The final site also executes the
added ordinary logistic regression comparison. Labels, family facets,
conditioning captions and discrete-response bars were inspected. The
conditional Orthodont residual tail and overstrong working-AR1 correction are
interpreted explicitly, rather than treating the simulation modes as
interchangeable. The Tweedie density overlay's smoothing of its zero mass is
explained and a separate zero-frequency calculation is shown.

Independent review checked BAM restrictions, conditioning definitions, censored
encoding, gaulss inverse-SD scale, GEV moment/deviance limits and gfam membership
against their implementations. It also identified guide-only families and
corrected the public API column of the central coverage table. No source defect
was found in these examples.
