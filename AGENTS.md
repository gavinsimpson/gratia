# Repository instructions

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
