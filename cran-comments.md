This is a patch release of gratia that is largely motivated by the issues on 
gratia's CRAN Check Results page arising from breaking changes in the scam
package. The new version also addresses the soft deprecation of `case_match()`
in dplyr v 1.2.0 that was recently released.

The package has been checked under:

* r-devel on Windows via winbuilder,
* r-devel on Ubuntu via GitHub Actions, and
* r-release on MacOS X, ubuntu, and Windows

No problems were observed. I note the `INFO` from `R CMD check` regarding the
large number of imports; gratia is designed to work within the tidyverse of
packages, and most of the dependencies in Imports are to those packages which
are considered (relatively) stable and dependable. This is unavoidable when
working within the tidyverse.

Reverse dependency checks completed on 7 November 2026, with no regressions
against the CRAN version of gratia (0.11.2). Details are below.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependency check results

The [GitHub Actions run](https://github.com/gavinsimpson/gratia/actions/runs/42411631120)
checked gratia 0.11.2-74 at commit `7331531c9` on
`gls/revdep-0-11-2-74`, using the "most dependents" setting. gratia and all
six reverse dependencies completed with no errors, warnings or NOTEs. Symbol
stripping was disabled, and the previous compiled-code NOTEs for gratia and
galamm were absent.

The optional suggested packages cmdstanr and INLA (for gdpar), and gurobi
(for smimodel), were unavailable. The workflow uses
`_R_CHECK_FORCE_SUGGESTS_=FALSE`, so checks continued without them; functionality
requiring those packages was not covered.

These results predate the subsequent plot-subtitle and documentation changes
on the release candidate branch.

```text
------- Check results summary ------
Check status summary:
                  OK
  Source packages  1
  Reverse depends  6

Check results summary:
gratia ... OK
rdepends_cgaim ... OK
rdepends_galamm ... OK
rdepends_gdam ... OK
rdepends_gdpar ... OK
rdepends_smimodel ... OK
rdepends_stgam ... OK

------- Check for regressions ------
No changes between old and new version
```
