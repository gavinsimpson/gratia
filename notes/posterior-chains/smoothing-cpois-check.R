# Check the rejected native fit independently of the smoothing prototype.
# An uncensored cpois likelihood must equal the ordinary Poisson likelihood.
# Run from the repository root, adding --load-all for the current checkout.
args <- commandArgs(TRUE)
if ("--load-all" %in% args) pkgload::load_all(quiet = TRUE) else library("gratia")
sampling_smoothing_family_bindings <- get("sampling_smoothing_family_bindings", asNamespace("gratia"))
source('tests/testthat/helper-smoothing-families.R')
f <- smoothing_family_fixture('poisson')
d <- f$data
d$w <- rep(1, nrow(d))
m <- mgcv::bam(y ~ s(x,k=5) + s(z,k=5) + offset(o), data=d, weights=w,
  family=sampling_smoothing_family_bindings(mgcv::cpois()), method='fREML',
  control=mgcv::gam.control(epsilon=1e-10,maxit=200L))
ref <- mgcv::gam(y ~ s(x,k=5) + s(z,k=5) + offset(o), data=d, weights=w,
  family=poisson(), method='REML', sp=m$sp)
X <- predict(m,type='lpmatrix')
S <- matrix(0,ncol(X),ncol(X)); k<-0L
for(sm in m$smooth) for(block in sm$S){k<-k+1L;j<-sm$first.para:sm$last.para;S[j,j]<-S[j,j]+m$sp[k]*block}
# Compare both fits in the original bam coefficient coordinates.
Xr <- predict(ref,type='lpmatrix')
beta <- qr.solve(X,drop(Xr%*%coef(ref)))
objective <- function(b) sum(m$family$dev.resids(m$y,exp(drop(X%*%b)+m$offset),rep(1,nrow(X)))) + drop(crossprod(b,S%*%b))
grad <- function(b) vapply(seq_along(b),function(j){h<-1e-5;v<-b;v[j]<-v[j]+h;a<-objective(v);v[j]<-v[j]-2*h;(a-objective(v))/(2*h)},numeric(1L))
record <- capture.output({
cat("R", as.character(getRversion()), "; mgcv", as.character(packageVersion("mgcv")), "\n")
cat('Uncensored cpois bam versus independent fixed-sp ordinary Poisson gam, unit weights:\n')
cat('native bam converged:',m$converged,'\n')
cat('objective at bam:',objective(coef(m)),'at reference:',objective(beta),'\n')
cat('max absolute objective gradient at bam:',max(abs(grad(coef(m)))),'at reference:',max(abs(grad(beta))),'\n')
})
writeLines(trimws(record, which = "right"), "notes/posterior-chains/smoothing-cpois-check.txt")
cat(record, sep = "\n")
