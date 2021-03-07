
#include <RcppArmadillo.h>

//' @name train_ridge
//' @title Estimate ridge regression
//' @description Estimate coefficients via ridge regression.
//' 
//' @param X Numeric matrix. The design matrix containing the predictor variables.
//' @param y Numeric matrix. The response variable(s).
//' @param lambda Numeric value. The regularization parameter.
//' @param weights Numeric vector. Observation weights.
//' 
//' @return A list containing the estimated coefficients, fitted values, etc.
//' @export

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
  
  // Calculate fitted values and residuals
  arma::mat yf = X * wout;
  arma::mat yr = y - yf;
  
  // Effective degrees of freedom
  double dof = trace((Xt * inv(XX + Ipp_lambda)) * trans(Xt));
  // Determinant of the residual variance-covariance matrix
  double det_sigma = det((trans(yr) * yr)) / n;
  
  // Convert n from int to double (due to integer-double division issue)
  double n_obs = n;
  
  // Akaike information criterion (AIC)
  double aic = log(det_sigma) + (2 / n_obs) * dof;
  
  // Hannan-Quinn information criterion (HQ)
  double hq = log(det_sigma) + (2 * log(log(n_obs)) / n_obs) * dof;
  
  // Bayesian information criterion (BIC)
  double bic = log(det_sigma) + (log(n_obs) / n_obs) * dof;
  
  return Rcpp::List::create(Rcpp::Named("wout") = wout,
                            Rcpp::Named("yf") = yf,
                            Rcpp::Named("yr") = yr,
                            Rcpp::Named("dof") = dof,
                            Rcpp::Named("det_sigma") = det_sigma,
                            Rcpp::Named("aic") = aic,
                            Rcpp::Named("hq") = hq,
                            Rcpp::Named("bic") = bic);
  
}
