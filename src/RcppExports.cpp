// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// align_spectras
Rcpp::List align_spectras(const Rcpp::DataFrame q_spectra, const Rcpp::ListOf<Rcpp::DataFrame> l_spectras, const float min_deviation_mz, const float min_deviation_abd);
RcppExport SEXP _workflow_lipido_align_spectras(SEXP q_spectraSEXP, SEXP l_spectrasSEXP, SEXP min_deviation_mzSEXP, SEXP min_deviation_abdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::DataFrame >::type q_spectra(q_spectraSEXP);
    Rcpp::traits::input_parameter< const Rcpp::ListOf<Rcpp::DataFrame> >::type l_spectras(l_spectrasSEXP);
    Rcpp::traits::input_parameter< const float >::type min_deviation_mz(min_deviation_mzSEXP);
    Rcpp::traits::input_parameter< const float >::type min_deviation_abd(min_deviation_abdSEXP);
    rcpp_result_gen = Rcpp::wrap(align_spectras(q_spectra, l_spectras, min_deviation_mz, min_deviation_abd));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_workflow_lipido_align_spectras", (DL_FUNC) &_workflow_lipido_align_spectras, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_workflow_lipido(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
