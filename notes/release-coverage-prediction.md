# Prediction, comparisons and rates of change coverage

Evidence reviewed: NEWS entries since CRAN 0.11.2, the relevant public Rd
files, `R/conditional-differences.R`, `R/response-derivatives.R`, transformed
prediction and derivative tests, observed-combination tests, and existing
data-slices and transformed-covariates vignettes. The installed gratia is older
than the source, so all execution loads current source with pkgload.

| Feature | Public APIs | Existing documentation | Article section | Worked example |
| --- | --- | --- | --- | --- |
| Simultaneous complete-model prediction intervals | `fitted_values()` | Reference; NEWS; existing data-slices examples use pointwise intervals | Compare expected uptake on a supported grid | Gamma GAM for CO2; one band over 240 concentration/origin/treatment predictions |
| Conditional response and link contrasts, delta and shared posterior calculations | `conditional_differences()`, `draw()` | Reference; tests for both uncertainty methods and nonlinear links | Estimate the treatment difference directly | Nonchilled minus chilled uptake within each origin; Gamma log inverse link; delta simultaneous curve and posterior pointwise estimates |
| Simultaneous coverage within a comparison or across all comparisons | `conditional_differences(simultaneous_scope=)` | Reference and simultaneous tests | Estimate the treatment difference directly | Treatment pair jointly over two origins; six joint origin/treatment pairs with shared posterior draws and scope all |
| Simultaneous differences between centred smooths | `difference_smooths()`, `draw()` | Reference and simultaneous tests | Compare shape separately from complete responses | Treatment smooth difference on log-uptake scale; separate shape and full-response scientific questions |
| Delta uncertainty for response derivatives | `response_derivatives()` | Reference; delta regression tests | Where does uptake change fastest? | Nonchilled Quebec concentration curve; compare delta point estimates and intervals with posterior simulation |
| Automatic finite-difference steps, first and second derivatives | `derivatives()`, `partial_derivatives()`, `response_derivatives()` | Reference; transformed-covariates vignette; derivative tests | Where does uptake change fastest?; transformed-coordinate section | First derivative with automatic step; second derivative discussed and independently checked in validation notes |
| Smooth expressions of raw covariates and derivatives in raw units | `smooth_estimates()`, `draw()`, `derivatives(wrt=)`, `partial_derivatives(wrt=)` | Existing transformed-covariates vignette with offsets/local functions/tensors | Keep covariate transformations and derivative units distinct | CO2 smooth of log concentration; original-scale effect plot and explicit numerical chain-rule comparison; tensor workflow linked to worked existing example |
| Plot transformed smooth and parametric axes in raw units | `draw(back_transform=)`, `parametric_effects()` | Existing transformed-covariates vignette | Keep covariate transformations and derivative units distinct | Smooth concentration response and constrained log-linear alternative on original concentration axis |
| Group-specific numeric slices and factor helper alias | `data_slice(.by=)`, `all_levels()` | data-slices article; reference | Compare expected uptake; respect the study design | Separate concentration grids and actual plant identifiers within origin/treatment combinations; displayed treatment factor levels |
| Only observed categorical combinations | `conditional_values(complete=FALSE)` | Reference; observed-combinations tests | Respect the study design | Twelve actual plant/origin/treatment combinations, retaining continuous concentration grid |
| Information about unobserved categorical prediction combinations | `fitted_values()` | Reference; NEWS | Respect the study design | Explain intentionally excluded plant effect during hypothetical treatment contrast and why an included plant effect would change its meaning |
| Preserve rows excluded by na.exclude; explicit prediction NA handling | `fitted_values()`; samples and residual APIs discussed | Reference; na.exclude tests | Preserve observation positions when measurements are missing | One missing uptake fitting row; explicit missing response prediction versus missing concentration predictor |
| Public coefficient covariance extraction and covariance choices | `get_vcov()`, `difference_smooths()`, `derivatives()`, `smooth_estimates()` | Reference; NEWS; covariance tests | Which uncertainty is being propagated? | Extract whole-model covariance from CO2 fit; explain term subset and frequentist/smoothing uncertainty choices |
| Select links for distribution parameters | `link()`, `inv_link()` | Reference | Which uncertainty is being propagated? | Explicit gaulss mean/scale selections explained; multi-predictor methods' limitations preserved |

Optional dependencies: these worked examples require only gratia and its
mgcv/dplyr/ggplot2 dependencies, datasets from R, and rmarkdown/knitr to build.
No optional fitting package is required. Two small GAM alternatives supplement
the main 84-row model. Monte Carlo batches have 2,000 draws and fixed seeds.

Bounds: the transformed-covariates vignette supplies the detailed executable
tensor, offset and local-function examples. This article does not refit every
supported response family; family-specific diagnostics belong to the model
checking article. Posterior/partial-residual/simulation missing-row alignment
is explained and independently checked, rather than repeated as plots.
