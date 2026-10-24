# Repository instructions

## Communication

Use plain English in explanations, progress updates, commit messages,
documentation, package vignettes and articles, `NEWS.md` entries, GitHub
issues and pull requests, and comments in code. Describe what happens,
what causes it, and why it matters. Avoid compressed jargon and invented
compound labels. Introduce a technical term only when it helps, and
explain its meaning first. Preserve scientific precision through
concrete descriptions, equations or examples—not unexplained
terminology. Assume the reader understands statistics but should not
have to decode your shorthand. Before sending or saving text, replace
any phrase that would reasonably prompt “What does that mean?” with a
direct explanation.

State what the evidence shows. Add a limitation only when it materially
changes how the reader should interpret or use that result. Connect it
to a specific claim or decision in the surrounding text, and explain its
practical consequence. Do not introduce an unasked alternative claim
merely to disclaim it. Omit generic cautions about causality,
uncertainty or generalisation when they add no useful information.

When reviewing a qualification, ask: “What misunderstanding does this
sentence prevent, and has the surrounding text given the reader a reason
to have it?” If there is no concrete answer, delete it.

## Release notes

Keep each individual `NEWS.md` entry to 1–3 sentences, including each
entry within a group. Group related changes under a shared banner naming
the function or topic, with a separate entry for each change. Separate
individual entries with a blank line, including entries within a group.
Use multiple paragraphs within one bullet for grouped entries. Do not
use nested bullets.

## R output in documentation

In documentation, vignettes, articles and examples, show the R
function’s normal printed output. Readers should see what they would see
if they ran the example code in an R session. Do not subset output
columns unless absolutely necessary. Do not replace the output with
formatted tables using
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) or similar
tools. Use a formatted table only when preparing output for a journal
and the user explicitly requests a tabular format.

Introduce every displayed code chunk with narrative explaining what it
will do or show. Readers should know its purpose before reading the
code; a section heading alone is not enough. Broader explanations can
follow the chunk.

For simulations, explain what the data are designed to represent and
what the example is intended to demonstrate. Describe the roles of the
covariates, response components, group structure and noise where
relevant. Separate data generation from model fitting with narrative so
readers can follow each step.

## Plot annotations

Keep default plots free of implementation and methodological commentary.

- Preserve the established caption format. Adding a feature does not
  authorize expanding plot annotations.
- Include labels needed to read the plotted values: axes, units, legends
  and established basis or constraint labels.
- Do not repeat settings the user requested, or add interval sources,
  calibration details, numerical diagnostics, provenance or generic
  caveats.
- Put those details in documentation, returned metadata or diagnostic
  output.
- Before adding text, ask: “What specific misreading of this figure
  would this prevent?” If there is no concrete answer, omit it.
- Review rendered plots for unnecessary text before delivery, just as
  you review them for incorrect values.

## Starting new work

Unless the user explicitly instructs otherwise, always start new work on
a new feature branch created from `main`. Use the `gls/` branch prefix.

Before creating the branch, check the current branch and working-tree
status. Create the feature branch with `main` as the explicit starting
point, for example:

``` sh
git switch -c gls/descriptive-name main
```

Do not branch from the current feature branch or carry unrelated work
into the new branch. Preserve uncommitted changes without discarding or
including them in unrelated work. If the user asks to continue an
existing branch, reuse it.

Before merging into `main`, inspect the commits and diff relative to
`main` and verify that the merge includes only the intended work.
Resolve any accidentally inherited commits before merging.

## Documentation topics and the pkgdown reference index

When adding a documentation topic, update the reference index in
`pkgdown/_pkgdown.yml` in the same change. Place each public topic in
the appropriate reference section. For a topic intended only for
internal use, mark it with `@keywords internal` in its roxygen source
instead.

Before committing, check that every new public topic is included in the
index and validate the reference index with pkgdown. A missing topic
causes the website build to fail.

## Reusing models in tests

Reuse models fitted in `tests/testthat/setup.R` by default. Before
adding a model fit to a test, check whether an existing setup model
provides the class, family, fitted terms, data and retained fitting
information needed by the assertion. Avoid repeated fitting because CRAN
test run time is critical.

Add a new fit only when the test needs a model structure or fitting
property that the existing setup models do not provide. Relevant
differences include smooth and random-effect structures, families and
links, offsets, weights, missing-row handling and fitting methods. Keep
independent reference fits or fresh native model state when those are
part of what the test checks.

Put new reusable model structures and their fixture builders in
`tests/testthat/setup.R`, using existing simulated data wherever
possible. Construct expensive or optional models on first use and cache
them for reuse, rather than adding unconditional setup work or fitting
them repeatedly in individual tests.

## Running tests

Do not run the `hgam-paper` tests unless the user specifically asks for
them. Keep the HGAM paper tests out of routine validation, including
full-suite runs. Use `make test-hgam-paper` only when specifically
checking the paper code; the existing weekly/manual CI job also runs
this check. Do not enable `GRATIA_RUN_HGAM` for ordinary package
changes.

Run all package tests and checks outside the sandbox from the outset,
including targeted tests and the full test suite. Request elevated
execution with `sandbox_permissions="require_escalated"` for test
commands; do not first attempt them inside the sandbox. Parallel-worker
tests need local socket permissions that the sandbox blocks, so a
sandboxed run can produce misleading failures and require an unnecessary
rerun. This instruction does not bypass tool approval requirements.
