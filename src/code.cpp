#include <Rcpp.h>
//#include <RcppArmadillo.h>
using namespace Rcpp;


// [[Rcpp::export]]
int timesFour(int x) {
  return x * 4;
}

// [[Rcpp::depends(RcppArmadillo)]]
//arma::mat estimate_markov_armadillo_and(arma::mat m_TR, arma::cube& a_P )
//{
//  int rows = m_TR.n_rows;
//
//  for(int i = 1; i < rows; i++){
//    m_TR.row(i) = m_TR.row(i-1) * a_P.slice(i);
//  }
//
//  return m_TR;
//}


