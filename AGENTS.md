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

## Running tests

Run all package tests and checks outside the sandbox from the outset,
including targeted tests and the full test suite. Request elevated
execution with `sandbox_permissions="require_escalated"` for test
commands; do not first attempt them inside the sandbox. Parallel-worker
tests need local socket permissions that the sandbox blocks, so a
sandboxed run can produce misleading failures and require an unnecessary
rerun. This instruction does not bypass tool approval requirements.
