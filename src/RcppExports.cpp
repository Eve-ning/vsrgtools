// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// cppCountForward
NumericVector cppCountForward(NumericVector unq_offsets, NumericVector obj_offsets, double window, bool is_sorted, Nullable<NumericVector> obj_weights);
RcppExport SEXP _osutools_cppCountForward(SEXP unq_offsetsSEXP, SEXP obj_offsetsSEXP, SEXP windowSEXP, SEXP is_sortedSEXP, SEXP obj_weightsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type unq_offsets(unq_offsetsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type obj_offsets(obj_offsetsSEXP);
    Rcpp::traits::input_parameter< double >::type window(windowSEXP);
    Rcpp::traits::input_parameter< bool >::type is_sorted(is_sortedSEXP);
    Rcpp::traits::input_parameter< Nullable<NumericVector> >::type obj_weights(obj_weightsSEXP);
    rcpp_result_gen = Rcpp::wrap(cppCountForward(unq_offsets, obj_offsets, window, is_sorted, obj_weights));
    return rcpp_result_gen;
END_RCPP
}
// cppBroadcast
NumericMatrix cppBroadcast(NumericVector offsets, LogicalMatrix resets);
RcppExport SEXP _osutools_cppBroadcast(SEXP offsetsSEXP, SEXP resetsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type offsets(offsetsSEXP);
    Rcpp::traits::input_parameter< LogicalMatrix >::type resets(resetsSEXP);
    rcpp_result_gen = Rcpp::wrap(cppBroadcast(offsets, resets));
    return rcpp_result_gen;
END_RCPP
}
// cppModelDensity
List cppModelDensity(NumericVector unq_offsets, List obj_offsets, double window, bool is_sorted);
RcppExport SEXP _osutools_cppModelDensity(SEXP unq_offsetsSEXP, SEXP obj_offsetsSEXP, SEXP windowSEXP, SEXP is_sortedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type unq_offsets(unq_offsetsSEXP);
    Rcpp::traits::input_parameter< List >::type obj_offsets(obj_offsetsSEXP);
    Rcpp::traits::input_parameter< double >::type window(windowSEXP);
    Rcpp::traits::input_parameter< bool >::type is_sorted(is_sortedSEXP);
    rcpp_result_gen = Rcpp::wrap(cppModelDensity(unq_offsets, obj_offsets, window, is_sorted));
    return rcpp_result_gen;
END_RCPP
}
// cppModelLongNote
List cppModelLongNote(DataFrame chart, std::string longNoteStartName, std::string longNoteEndName, std::string newName, bool labelStart, bool labelEnd);
RcppExport SEXP _osutools_cppModelLongNote(SEXP chartSEXP, SEXP longNoteStartNameSEXP, SEXP longNoteEndNameSEXP, SEXP newNameSEXP, SEXP labelStartSEXP, SEXP labelEndSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type chart(chartSEXP);
    Rcpp::traits::input_parameter< std::string >::type longNoteStartName(longNoteStartNameSEXP);
    Rcpp::traits::input_parameter< std::string >::type longNoteEndName(longNoteEndNameSEXP);
    Rcpp::traits::input_parameter< std::string >::type newName(newNameSEXP);
    Rcpp::traits::input_parameter< bool >::type labelStart(labelStartSEXP);
    Rcpp::traits::input_parameter< bool >::type labelEnd(labelEndSEXP);
    rcpp_result_gen = Rcpp::wrap(cppModelLongNote(chart, longNoteStartName, longNoteEndName, newName, labelStart, labelEnd));
    return rcpp_result_gen;
END_RCPP
}
// cppModelManipulation
List cppModelManipulation(NumericVector unq_offsets, List obj_offsets, double window, bool is_sorted);
RcppExport SEXP _osutools_cppModelManipulation(SEXP unq_offsetsSEXP, SEXP obj_offsetsSEXP, SEXP windowSEXP, SEXP is_sortedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type unq_offsets(unq_offsetsSEXP);
    Rcpp::traits::input_parameter< List >::type obj_offsets(obj_offsetsSEXP);
    Rcpp::traits::input_parameter< double >::type window(windowSEXP);
    Rcpp::traits::input_parameter< bool >::type is_sorted(is_sortedSEXP);
    rcpp_result_gen = Rcpp::wrap(cppModelManipulation(unq_offsets, obj_offsets, window, is_sorted));
    return rcpp_result_gen;
END_RCPP
}
// cppSimulateKey
DataFrame cppSimulateKey(NumericVector offsets, NumericVector values, double decay_ms, double decay_perc_s, double stress_init);
RcppExport SEXP _osutools_cppSimulateKey(SEXP offsetsSEXP, SEXP valuesSEXP, SEXP decay_msSEXP, SEXP decay_perc_sSEXP, SEXP stress_initSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type offsets(offsetsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type values(valuesSEXP);
    Rcpp::traits::input_parameter< double >::type decay_ms(decay_msSEXP);
    Rcpp::traits::input_parameter< double >::type decay_perc_s(decay_perc_sSEXP);
    Rcpp::traits::input_parameter< double >::type stress_init(stress_initSEXP);
    rcpp_result_gen = Rcpp::wrap(cppSimulateKey(offsets, values, decay_ms, decay_perc_s, stress_init));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_osutools_cppCountForward", (DL_FUNC) &_osutools_cppCountForward, 5},
    {"_osutools_cppBroadcast", (DL_FUNC) &_osutools_cppBroadcast, 2},
    {"_osutools_cppModelDensity", (DL_FUNC) &_osutools_cppModelDensity, 4},
    {"_osutools_cppModelLongNote", (DL_FUNC) &_osutools_cppModelLongNote, 6},
    {"_osutools_cppModelManipulation", (DL_FUNC) &_osutools_cppModelManipulation, 4},
    {"_osutools_cppSimulateKey", (DL_FUNC) &_osutools_cppSimulateKey, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_osutools(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
