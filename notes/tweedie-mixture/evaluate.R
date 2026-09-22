# Run from the repository root: Rscript notes/tweedie-mixture/evaluate.R
# Writes reproducible measurements; does not change package defaults or commit.
suppressPackageStartupMessages(library(mgcv))
pkgload::load_all(quiet = TRUE)
outdir <- 'notes/tweedie-mixture/results'
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
profiles <- list(
  strict = list(cdf_tol = 1e-12, probability_tol = 1e-8, quantile_tol = 1e-10),
  diagnostic = list(cdf_tol = 1e-7, probability_tol = 1e-5, quantile_tol = 1e-6),
  loose = list(cdf_tol = 1e-5, probability_tol = 1e-3, quantile_tol = 1e-4)
)
tight <- list(cdf_tol = 1e-15, probability_tol = 1e-11, quantile_tol = 1e-13)
qmix <- function(u, mu, power, phi, profile, ...) {
  do.call(qtweedie_mixture, c(list(prob=u,mu=mu,power=power,phi=phi),profile,list(...)))
}
pmix <- function(y, mu, power, phi, profile, ...) {
  profile$quantile_tol <- NULL
  do.call(ptweedie_mixture, c(list(q=y,mu=mu,power=power,phi=phi),profile,list(...)))
}
score <- function(y, mu, power, phi, profile) {
  lf <- pmix(y,mu,power,phi,profile,log_p=TRUE)
  ls <- pmix(y,mu,power,phi,profile,log_p=TRUE,lower_tail=FALSE)
  ifelse(lf <= ls, qnorm(lf,log.p=TRUE),qnorm(ls,lower.tail=FALSE,log.p=TRUE))
}
timed <- function(fun, reps = 10L) {
  invisible(fun())
  times <- replicate(3, system.time(for(i in seq_len(reps)) fun())[['elapsed']]/reps)
  median(times)
}
rows <- list()
settings <- data.frame(name=c('moderate','near_poisson','near_gamma','small_dispersion'),
  power=c(1.5,1.05,1.95,1.5),phi=c(1,1,1,.01))
for (j in seq_len(nrow(settings))) {
  s <- settings[j,]; mu <- exp(seq(0,2,length.out=60)); u <- ppoints(60)
  cat('Quantile comparison:',s$name,'\n')
  base_time <- system.time(base <- tweedie::qtweedie(u,mu=mu,xi=s$power,phi=s$phi))[['elapsed']]
  strict_q <- qmix(u,mu,s$power,s$phi,tight)
  fam <- mgcv::Tweedie(p=s$power)
  resid <- function(y) sign(y-mu)*sqrt(pmax(0,fam$dev.resids(y,mu,rep(1,60))))
  for (name in names(profiles)) {
    profile <- profiles[[name]]
    actual <- qmix(u,mu,s$power,s$phi,profile,details=TRUE)
    positive <- actual$quantile>0
    z <- score(actual$quantile,mu,s$power,s$phi,tight)
    cdf <- pmix(actual$quantile,mu,s$power,s$phi,tight)
    elapsed <- timed(function() qmix(u,mu,s$power,s$phi,profile))
    rows[[length(rows)+1]] <- data.frame(case=s$name,profile=name,power=s$power,phi=s$phi,
      seconds=elapsed,reference_seconds=base_time,speedup=base_time/elapsed,
      max_abs_q_difference=max(abs(actual$quantile-base)),
      max_scaled_q_difference=max(abs(actual$quantile-strict_q)/pmax(mu,abs(strict_q))),
      max_probability_error=max(abs(cdf[positive]-u[positive])),
      max_normal_score_error=max(abs(z[positive]-qnorm(u[positive]))),
      max_deviance_residual_difference=max(abs(resid(actual$quantile)-resid(strict_q))),
      max_terms=max(actual$terms),fallbacks=sum(actual$method=='fallback'))
  }
}
write.csv(do.call(rbind,rows),file.path(outdir,'quantile-benchmarks.csv'),row.names=FALSE)

# Stress scan does NOT invoke the potentially expensive inversion fallback.
# It records the boundary between mixture success and fallback instead.
cat('Stress scan\n')
cases <- expand.grid(power=c(1.01,1.1,1.5,1.9,1.99,1.9999),
  mu=c(.001,1,1000),phi=c(.001,1,100))
rows <- list()
for(j in seq_len(nrow(cases))) {
  c <- cases[j,]; lambda <- c$mu^(2-c$power)/(c$phi*(2-c$power))
  lp <- unique(c(log(c(1e-12,1e-6,.01,.1,.5,.9,.99,1-1e-6)),-1e-12,-1e-30,-lambda+1e-8))
  for (name in names(profiles)) for(u in lp[lp<0]) {
    if(u<=-lambda) {
      r <- list(terms=0,log_error=0,root=-Inf); status <- 'atom'
    } else {
      profile<-profiles[[name]]
      r<-do.call(tweedie_mixture_quantile_one,c(list(logu=u,lambda=lambda,
        alpha=(2-c$power)/(c$power-1)),profile,list(max_terms=10000,allow_partial=FALSE)))
      status<-if(is.null(r$reason)) 'mixture' else r$reason
      if(status=='mixture' && exp(log(c$mu)+r$root)==0) status<-'underflow'
    }
    rows[[length(rows)+1]]<-data.frame(c,profile=name,logu=u,lambda=lambda,
      status=status,terms=r$terms,log_tail_error=r$log_error)
  }
}
write.csv(do.call(rbind,rows),file.path(outdir,'stress-scan.csv'),row.names=FALSE)

# CDF/PIT comparisons: same zero-atom randomisation, varying fitted means.
cat('CDF and PIT comparisons\n')
set.seed(410)
rows <- checks <- list()
for(j in seq_len(nrow(settings))) {
  s<-settings[j,]; mu<-exp(seq(0,2,length.out=269))
  y<-mgcv::rTweedie(mu,p=s$power,phi=s$phi)
  atom<-exp(-mu^(2-s$power)/(s$phi*(2-s$power)))
  zero_u<-runif(length(y))*atom
  base_time<-system.time(base<-tweedie::ptweedie(y,mu=mu,xi=s$power,phi=s$phi))[['elapsed']]
  strict_cdf<-pmix(y,mu,s$power,s$phi,tight)
  worst<-which.max(abs(strict_cdf-base))
  series<-tweedie::ptweedie_series(y[worst],mu=mu[worst],power=s$power,phi=s$phi)
  lambda<-mu[worst]^(2-s$power)/(s$phi*(2-s$power))
  k<-seq.int(max(1,floor(lambda-15*sqrt(lambda))-50),ceiling(lambda+15*sqrt(lambda))+50)
  wide<-exp(-lambda)+sum(dpois(k,lambda)*pgamma(y[worst],
    shape=k*(2-s$power)/(s$power-1),scale=s$phi*(s$power-1)*mu[worst]^(s$power-1)))
  checks[[j]]<-data.frame(case=s$name,y=y[worst],mu=mu[worst],power=s$power,phi=s$phi,
    mixture=strict_cdf[worst],inversion=base[worst],package_series=series,wide_sum=wide)
  strict_z<-score(y,mu,s$power,s$phi,tight)
  strict_z[y==0]<-qnorm(zero_u[y==0])
  for(name in names(profiles)) {
    profile<-profiles[[name]]
    actual<-pmix(y,mu,s$power,s$phi,profile)
    z<-score(y,mu,s$power,s$phi,profile);z[y==0]<-qnorm(zero_u[y==0])
    rows[[length(rows)+1]]<-data.frame(case=s$name,profile=name,
      seconds=timed(function() pmix(y,mu,s$power,s$phi,profile),reps=3),
      reference_seconds=base_time,max_cdf_difference=max(abs(actual-base)),
      max_pit_difference=max(abs(actual-strict_cdf)),
      max_normal_residual_difference=max(abs(z-strict_z)))
  }
}
write.csv(do.call(rbind,rows),file.path(outdir,'pit-benchmarks.csv'),row.names=FALSE)
write.csv(do.call(rbind,checks),file.path(outdir,'reference-crosschecks.csv'),row.names=FALSE)

# Real fitted-model QQ calculations, using the existing residual computation.
# Override only a copied family's qf; package defaults are never modified.
cat('Fitted-model uniform diagnostics\n')
set.seed(410)
dat<-data.frame(x=seq(0,1,length.out=269))
dat$y<-mgcv::rTweedie(exp(1+sin(2*pi*dat$x)),p=1.5,phi=1)
m<-mgcv::gam(y~s(x,k=8),family=mgcv::tw(),data=dat,method='REML')
run_qq<-function(profile=NULL,seed=42) {
  model<-m
  if(!is.null(profile)) {
    power<-model$family$getTheta(TRUE)
    model$family$qf<-function(p,mu,wt,scale,log_p=FALSE) {
      qmix(p,mu,power,scale,profile,log_p=log_p)
    }
  }
  withr::with_seed(seed,gratia:::qq_uniform(model,n=10,type='deviance'))
}
base_time<-system.time(base<-run_qq())[['elapsed']]
rows<-list()
for(name in names(profiles)) {
  elapsed<-system.time(actual<-run_qq(profiles[[name]]))[['elapsed']]
  rows[[name]]<-data.frame(profile=name,seconds=elapsed,reference_seconds=base_time,
    max_reference_quantile_difference=max(abs(actual$theoretical-base$theoretical)))
}
noise<-run_qq(profiles$strict,seed=43)$theoretical-run_qq(profiles$strict,seed=42)$theoretical
qq_results<-do.call(rbind,rows)
qq_results$seed_change_rms<-sqrt(mean(noise^2))
qq_results$seed_change_max<-max(abs(noise))
write.csv(qq_results,file.path(outdir,'model-diagnostics.csv'),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(outdir,'session-info.txt'))
cat('Results written to',outdir,'\n')
