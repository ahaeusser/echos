
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::mat run_reservoir(arma::mat inputs,
                        arma::mat win,
                        arma::mat wres,
                        double alpha) {
  
  // Number of observations for training
  int n_train = inputs.n_rows;
  // Number of internal states (nodes within reservoir)
  int n_res = wres.n_cols;
  // Preallocate empty matrices to store internal states and its updates
  arma::mat states_train = arma::zeros<arma::mat>(n_train, n_res);
  arma::mat states_train_upd = arma::zeros<arma::mat>(n_train, n_res);
  // Run reservoir
  for (int i=1; i<n_train; i++) {
    states_train_upd.row(i) = trans(tanh(win * trans(inputs.row(i)) + wres * trans(states_train.row(i - 1))));
    states_train.row(i) = alpha * states_train_upd.row(i) + (1 - alpha) * states_train.row(i - 1);
  }
  return states_train;
}