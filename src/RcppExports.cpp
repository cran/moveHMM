// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// dgamma_rcpp
arma::colvec dgamma_rcpp(NumericVector x, double mu, double sigma);
RcppExport SEXP moveHMM_dgamma_rcpp(SEXP xSEXP, SEXP muSEXP, SEXP sigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP);
    __result = Rcpp::wrap(dgamma_rcpp(x, mu, sigma));
    return __result;
END_RCPP
}
// dweibull_rcpp
arma::colvec dweibull_rcpp(NumericVector x, double shape, double scale);
RcppExport SEXP moveHMM_dweibull_rcpp(SEXP xSEXP, SEXP shapeSEXP, SEXP scaleSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type shape(shapeSEXP);
    Rcpp::traits::input_parameter< double >::type scale(scaleSEXP);
    __result = Rcpp::wrap(dweibull_rcpp(x, shape, scale));
    return __result;
END_RCPP
}
// dlnorm_rcpp
arma::colvec dlnorm_rcpp(NumericVector x, double meanlog, double sdlog);
RcppExport SEXP moveHMM_dlnorm_rcpp(SEXP xSEXP, SEXP meanlogSEXP, SEXP sdlogSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type meanlog(meanlogSEXP);
    Rcpp::traits::input_parameter< double >::type sdlog(sdlogSEXP);
    __result = Rcpp::wrap(dlnorm_rcpp(x, meanlog, sdlog));
    return __result;
END_RCPP
}
// dexp_rcpp
arma::colvec dexp_rcpp(NumericVector x, double rate, double foo);
RcppExport SEXP moveHMM_dexp_rcpp(SEXP xSEXP, SEXP rateSEXP, SEXP fooSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type rate(rateSEXP);
    Rcpp::traits::input_parameter< double >::type foo(fooSEXP);
    __result = Rcpp::wrap(dexp_rcpp(x, rate, foo));
    return __result;
END_RCPP
}
// dvm_rcpp
arma::colvec dvm_rcpp(NumericVector x, double mu, double kappa);
RcppExport SEXP moveHMM_dvm_rcpp(SEXP xSEXP, SEXP muSEXP, SEXP kappaSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type kappa(kappaSEXP);
    __result = Rcpp::wrap(dvm_rcpp(x, mu, kappa));
    return __result;
END_RCPP
}
// dwrpcauchy_rcpp
arma::colvec dwrpcauchy_rcpp(NumericVector x, double mu, double rho);
RcppExport SEXP moveHMM_dwrpcauchy_rcpp(SEXP xSEXP, SEXP muSEXP, SEXP rhoSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    __result = Rcpp::wrap(dwrpcauchy_rcpp(x, mu, rho));
    return __result;
END_RCPP
}
// nLogLike_rcpp
double nLogLike_rcpp(int nbStates, arma::mat beta, arma::mat covs, DataFrame data, std::string stepDist, std::string angleDist, arma::mat stepPar, arma::mat anglePar, arma::rowvec delta, IntegerVector aInd, bool zeroInflation, bool stationary);
RcppExport SEXP moveHMM_nLogLike_rcpp(SEXP nbStatesSEXP, SEXP betaSEXP, SEXP covsSEXP, SEXP dataSEXP, SEXP stepDistSEXP, SEXP angleDistSEXP, SEXP stepParSEXP, SEXP angleParSEXP, SEXP deltaSEXP, SEXP aIndSEXP, SEXP zeroInflationSEXP, SEXP stationarySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type nbStates(nbStatesSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type covs(covsSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::string >::type stepDist(stepDistSEXP);
    Rcpp::traits::input_parameter< std::string >::type angleDist(angleDistSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type stepPar(stepParSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type anglePar(angleParSEXP);
    Rcpp::traits::input_parameter< arma::rowvec >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type aInd(aIndSEXP);
    Rcpp::traits::input_parameter< bool >::type zeroInflation(zeroInflationSEXP);
    Rcpp::traits::input_parameter< bool >::type stationary(stationarySEXP);
    __result = Rcpp::wrap(nLogLike_rcpp(nbStates, beta, covs, data, stepDist, angleDist, stepPar, anglePar, delta, aInd, zeroInflation, stationary));
    return __result;
END_RCPP
}
// trMatrix_rcpp
arma::cube trMatrix_rcpp(int nbStates, arma::mat beta, arma::mat covs);
RcppExport SEXP moveHMM_trMatrix_rcpp(SEXP nbStatesSEXP, SEXP betaSEXP, SEXP covsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type nbStates(nbStatesSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type covs(covsSEXP);
    __result = Rcpp::wrap(trMatrix_rcpp(nbStates, beta, covs));
    return __result;
END_RCPP
}
