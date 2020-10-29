// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// ArmaTDMarkovLoop
arma::mat ArmaTDMarkovLoop(arma::mat m_TR, arma::cube& a_P);
RcppExport SEXP _darkpeak_ArmaTDMarkovLoop(SEXP m_TRSEXP, SEXP a_PSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type m_TR(m_TRSEXP);
    Rcpp::traits::input_parameter< arma::cube& >::type a_P(a_PSEXP);
    rcpp_result_gen = Rcpp::wrap(ArmaTDMarkovLoop(m_TR, a_P));
    return rcpp_result_gen;
END_RCPP
}
// timesFour
int timesFour(int x);
RcppExport SEXP _darkpeak_timesFour(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(timesFour(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_darkpeak_ArmaTDMarkovLoop", (DL_FUNC) &_darkpeak_ArmaTDMarkovLoop, 2},
    {"_darkpeak_timesFour", (DL_FUNC) &_darkpeak_timesFour, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_darkpeak(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
