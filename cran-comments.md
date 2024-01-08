This is a patch release responding to the emailed request for fixes to address the problems shown on the Package Check Results page for package gratia.

All of the requested or highlighted issues have been fixed, including the M1mac specific discrepancy.

* The Rd markup issue raised with R-devel on Debian is fixed.
* The discrepancy in example reference output on M1mac is fixed; after some testing changing the amount of data used in the example, etc. this seems to be a real difference in the GAM estimated by mgcv for these data on the M1mac platform, which propagates to the output from gratia. I have changed the example by removing the code block related to the error. I now check for changes in output from this function via a snapshot test rather than in the example output. The removed example was somewhat unnecessary (it was an additional example not the only example) and I am conscious that the package has a not insignificant check time.
* General discrepancies in example reference output have been fixed by regenerating the output with the latest versions of the packages gratia depends upon.

## revdepcheck results

We checked 2 reverse dependencies (2 from CRAN + 0 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
 