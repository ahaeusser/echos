
#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

Rcpp::List train_ridge(const arma::mat& X,
                       const arma::mat& y,
                       double lambda,
                       const arma::colvec& weights) {
  
  // Number of predictor variables (inclusive intercept term) and 
  // number of observations
  int p = X.n_cols, n = X.n_rows;
  
  // Intermediate calculations
  // Prepare identity matrix and lambda (intercept term is not regularized)
  arma::mat Ipp = arma::eye<arma::mat>(p, p);
  arma::mat Ipp_lambda = Ipp * lambda;
  Ipp_lambda(0, 0) = 0;
  
  // Prepare observation weights and transformed design and response matrix
  arma::colvec rw = sqrt(weights);
  arma::mat Xt = X.each_col() % rw;
  arma::mat yt = y.each_col() % rw;
  // Prepare cross products
  arma::mat XX = trans(Xt) * Xt;
  arma::mat Xy = trans(Xt) * yt;
  
  // Solve for regression coefficients wout
  arma::mat wout = solve(XX + Ipp_lambda, Xy, arma::solve_opts::fast);
  
  // Calulcate fitted values and residuals
  arma::mat yhat = X * wout;
  arma::mat res = y - yhat;
  
  // Effective degrees of freedom
  double df = trace((Xt * inv(XX + Ipp_lambda)) * trans(Xt));
  // Residual sum of squares (RSS)
  double rss = trace((trans(res) * res));
  // Akaike information criterion (AIC)
  double aic = n * log(rss) + 2 * df;
  // Bayesian information criterion (BIC)
  double bic = n * log(rss) + df * log(n);
  
  return Rcpp::List::create(Rcpp::Named("wout") = wout,
                            Rcpp::Named("yhat") = yhat,
                            Rcpp::Named("res") = res,
                            Rcpp::Named("df") = df,
                            Rcpp::Named("rss") = rss,
                            Rcpp::Named("aic") = aic,
                            Rcpp::Named("bic") = bic);
  
}