#include <Rcpp.h>
#include <R_ext/BLAS.h>
#include <cmath>
using namespace Rcpp;

// These evaluations omit constants independent of the coefficients.
static double softplus(double x) {
  return std::max(x, 0.0) + std::log1p(std::exp(-std::abs(x)));
}
static double logistic(double x) {
  return x >= 0 ? 1.0 / (1.0 + std::exp(-x)) : std::exp(x) / (1.0 + std::exp(x));
}
static int family_kind(const std::string& kind) {
  if (kind == "gaussian") return 0;
  if (kind == "poisson") return 1;
  if (kind == "binomial") return 2;
  if (kind == "gamma") return 3;
  if (kind == "nb") return 4;
  stop("Unsupported sampling likelihood.");
  return -1;
}
static double likelihood(const NumericMatrix& X, const NumericVector& beta,
                         const NumericVector& y, const NumericVector& w,
                         const NumericVector& offset, int kind, double phi,
                         double theta, NumericVector& eta, NumericVector& score) {
  int n = X.nrow(), p = X.ncol(), inc = 1;
  double one = 1, zero = 0;
  F77_CALL(dgemv)("N", &n, &p, &one, X.begin(), &n, beta.begin(), &inc,
    &zero, eta.begin(), &inc FCONE);
  double value = 0;
  for (int i = 0; i < n; ++i) {
    if (w[i] == 0) { score[i] = 0; continue; }
    double e = eta[i] + offset[i], v, g;
    if (kind == 0) {
      double residual = y[i] - e;
      v = -residual * residual / (2 * phi); g = residual / phi;
    } else if (kind == 1) {
      double mu = std::exp(e);
      v = (y[i] * e - mu) / phi; g = (y[i] - mu) / phi;
    } else if (kind == 2) {
      v = (y[i] * e - softplus(e)) / phi; g = (y[i] - logistic(e)) / phi;
    } else if (kind == 3) {
      double ratio = y[i] * std::exp(-e);
      v = (-ratio - e) / phi; g = (ratio - 1) / phi;
    } else {
      double z = e - std::log(theta);
      v = (y[i] * e - (y[i] + theta) * softplus(z)) / phi;
      g = (y[i] - (y[i] + theta) * logistic(z)) / phi;
    }
    value += w[i] * v; score[i] = w[i] * g;
  }
  return std::isfinite(value) ? value : R_NegInf;
}

// [[Rcpp::export]]
List sampling_likelihood_cpp(NumericMatrix X, NumericVector beta,
                            NumericVector y, NumericVector w, NumericVector offset,
                            std::string kind, double phi, double theta, bool gradient) {
  int n = X.nrow(), p = X.ncol(), inc = 1;
  if (beta.size() != p || y.size() != n || w.size() != n || offset.size() != n)
    stop("Sampling likelihood dimensions do not match.");
  NumericVector eta(n), score(n), grad(gradient ? p : 0);
  double value = likelihood(X, beta, y, w, offset, family_kind(kind), phi, theta, eta, score);
  if (gradient) {
    double one = 1, zero = 0;
    F77_CALL(dgemv)("T", &n, &p, &one, X.begin(), &n, score.begin(), &inc,
      &zero, grad.begin(), &inc FCONE);
  }
  return List::create(_["value"] = value, _["gradient"] = grad);
}

static double posterior_value(const NumericMatrix& X, const NumericMatrix& S,
                              const NumericVector& beta, const NumericVector& y,
                              const NumericVector& w, const NumericVector& offset,
                              int kind, double phi, double theta, NumericVector& eta,
                              NumericVector& score, NumericVector& work) {
  int p = beta.size(), inc = 1;
  double one = 1, zero = 0;
  F77_CALL(dgemv)("N", &p, &p, &one, S.begin(), &p, beta.begin(), &inc,
    &zero, work.begin(), &inc FCONE);
  double penalty = 0;
  for (int j = 0; j < p; ++j) penalty += beta[j] * work[j];
  double value = likelihood(X, beta, y, w, offset, kind, phi, theta, eta, score) - penalty / (2 * phi);
  return std::isfinite(value) ? value : R_NegInf;
}
static double log_proposal(const NumericVector& b, const NumericVector& mu,
                           const NumericMatrix& R, double df, NumericVector& work) {
  int p = b.size(); double distance = 0;
  for (int j = 0; j < p; ++j) {
    double z = b[j] - mu[j];
    for (int k = 0; k < j; ++k) z -= R(k, j) * work[k];
    work[j] = z / R(j, j); distance += work[j] * work[j];
  }
  return -(df + p) / 2 * std::log1p(distance / df);
}
static void normal_step(NumericVector& out, const NumericMatrix& R,
                        NumericVector& z, double scale) {
  int p = out.size();
  for (int j = 0; j < p; ++j) z[j] = R::rnorm(0, 1);
  for (int j = 0; j < p; ++j) {
    double v = 0;
    for (int k = 0; k <= j; ++k) v += z[k] * R(k, j);
    out[j] = v * scale;
  }
}

// Both backends use the same proposals, acceptance decisions and RNG order.
template <typename Evaluator>
static List sampling_mh_loop(NumericVector mu, NumericMatrix R,
    NumericVector initial, int n, int burnin, int thin, double df,
    double rw_scale, Evaluator& evaluate) {
  int p = mu.size();
  NumericVector beta = clone(initial), candidate(p), step(p), z(p), qwork(p);
  NumericMatrix draws(n, p);
  double lp = evaluate(beta);
  if (!std::isfinite(lp)) stop("Non-finite posterior at the initial coefficients.");
  double lq = log_proposal(beta, mu, R, df, qwork);
  bool rw = std::isfinite(rw_scale) && rw_scale > 0;
  int retained = 0, accepted = 0, rw_accepted = 0;
  double total = static_cast<double>(burnin) + static_cast<double>(n) * thin;
  for (double iter = 1; iter <= total; ++iter) {
    if (std::fmod(iter, 256) == 0) checkUserInterrupt();
    normal_step(candidate, R, z, 1);
    double factor = std::sqrt(R::rchisq(df) / df);
    for (int j = 0; j < p; ++j) candidate[j] = mu[j] + candidate[j] / factor;
    double next_lp = evaluate(candidate);
    double next_lq = log_proposal(candidate, mu, R, df, qwork);
    bool keep = std::log(R::runif(0, 1)) < next_lp - lp + lq - next_lq;
    if (keep && std::isfinite(next_lp)) { std::copy(candidate.begin(), candidate.end(), beta.begin()); lp = next_lp; lq = next_lq; }
    else keep = false;
    if (iter > burnin) accepted += keep;
    if (rw) {
      normal_step(step, R, z, std::sqrt(rw_scale));
      for (int j = 0; j < p; ++j) candidate[j] = beta[j] + step[j];
      next_lp = evaluate(candidate);
      keep = std::log(R::runif(0, 1)) < next_lp - lp;
      if (keep && std::isfinite(next_lp)) {
        std::copy(candidate.begin(), candidate.end(), beta.begin()); lp = next_lp;
        lq = log_proposal(beta, mu, R, df, qwork);
      } else keep = false;
      if (iter > burnin) rw_accepted += keep;
    }
    if (iter > burnin && std::fmod(iter - burnin - 1, thin) == 0) {
      for (int j = 0; j < p; ++j) draws(retained, j) = beta[j];
      ++retained;
    }
  }
  double attempts = static_cast<double>(n) * thin;
  return List::create(_["bs"] = draws, _["accept"] = accepted / attempts,
    _["rw.accept"] = rw ? rw_accepted / attempts : NA_REAL);
}

static void check_chain_inputs(const NumericVector& mu, const NumericMatrix& R,
    const NumericVector& initial, int n, int burnin, int thin, double df) {
  int p = mu.size();
  if (p < 1 || R.nrow() != p || R.ncol() != p || initial.size() != p ||
      n < 1 || burnin < 0 || thin < 1 || !std::isfinite(df) || df <= 0)
    stop("Invalid compiled sampler inputs.");
}

// [[Rcpp::export]]
List sampling_mh_cpp(NumericMatrix X, NumericMatrix S, NumericVector mu,
                     NumericMatrix R, NumericVector initial, NumericVector y,
                     NumericVector w, NumericVector offset, std::string family,
                     double phi, double theta, int n, int burnin, int thin,
                     double df, double rw_scale) {
  check_chain_inputs(mu, R, initial, n, burnin, thin, df);
  int p = mu.size(), nr = X.nrow();
  if (X.ncol() != p || S.nrow() != p || S.ncol() != p ||
      y.size() != nr || w.size() != nr || offset.size() != nr)
    stop("Invalid compiled sampler inputs.");
  int kind = family_kind(family);
  NumericVector eta(nr), score(nr), work(p);
  auto evaluate = [&](const NumericVector& beta) {
    return posterior_value(X, S, beta, y, w, offset, kind, phi, theta, eta, score, work);
  };
  return sampling_mh_loop(mu, R, initial, n, burnin, thin, df, rw_scale, evaluate);
}

// Family callbacks run on the main R thread of each chain worker. The penalty
// and proposal factorisation are prepared once and shared across chains.
// [[Rcpp::export]]
List sampling_mh_family_cpp(NumericMatrix S, NumericVector mu, NumericMatrix R,
    NumericVector initial, double phi, int n, int burnin, int thin, double df,
    double rw_scale, Function log_likelihood) {
  check_chain_inputs(mu, R, initial, n, burnin, thin, df);
  int p = mu.size(), inc = 1;
  if (S.nrow() != p || S.ncol() != p || !std::isfinite(phi) || phi <= 0)
    stop("Invalid compiled family sampler inputs.");
  NumericVector work(p);
  auto evaluate = [&](const NumericVector& beta) {
    double one = 1, zero = 0;
    F77_CALL(dgemv)("N", &p, &p, &one, S.begin(), &p, beta.begin(), &inc,
      &zero, work.begin(), &inc FCONE);
    double penalty = 0;
    for (int j = 0; j < p; ++j) penalty += beta[j] * work[j];
    double value = as<double>(log_likelihood(beta)) - penalty / (2 * phi);
    return std::isfinite(value) ? value : R_NegInf;
  };
  return sampling_mh_loop(mu, R, initial, n, burnin, thin, df, rw_scale, evaluate);
}
