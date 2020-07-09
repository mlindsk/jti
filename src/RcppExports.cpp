// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "jti_types.h"
#include <Rcpp.h>

using namespace Rcpp;

// mcs
Rcpp::List mcs(Rcpp::List& adj, bool check);
RcppExport SEXP _jti_mcs(SEXP adjSEXP, SEXP checkSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List& >::type adj(adjSEXP);
    Rcpp::traits::input_parameter< bool >::type check(checkSEXP);
    rcpp_result_gen = Rcpp::wrap(mcs(adj, check));
    return rcpp_result_gen;
END_RCPP
}
// perfect_cliques
VVS perfect_cliques(VVS& x);
RcppExport SEXP _jti_perfect_cliques(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< VVS& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(perfect_cliques(x));
    return rcpp_result_gen;
END_RCPP
}
// perfect_separators
Rcpp::List perfect_separators(VVS& x);
RcppExport SEXP _jti_perfect_separators(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< VVS& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(perfect_separators(x));
    return rcpp_result_gen;
END_RCPP
}
// parents
Rcpp::List parents(VS po, Rcpp::List ps);
RcppExport SEXP _jti_parents(SEXP poSEXP, SEXP psSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< VS >::type po(poSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type ps(psSEXP);
    rcpp_result_gen = Rcpp::wrap(parents(po, ps));
    return rcpp_result_gen;
END_RCPP
}
// rip
Rcpp::List rip(Rcpp::List& adj, bool check);
RcppExport SEXP _jti_rip(SEXP adjSEXP, SEXP checkSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List& >::type adj(adjSEXP);
    Rcpp::traits::input_parameter< bool >::type check(checkSEXP);
    rcpp_result_gen = Rcpp::wrap(rip(adj, check));
    return rcpp_result_gen;
END_RCPP
}
// set_intersect
VS set_intersect(VS& v1, VS& v2);
RcppExport SEXP _jti_set_intersect(SEXP v1SEXP, SEXP v2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< VS& >::type v1(v1SEXP);
    Rcpp::traits::input_parameter< VS& >::type v2(v2SEXP);
    rcpp_result_gen = Rcpp::wrap(set_intersect(v1, v2));
    return rcpp_result_gen;
END_RCPP
}
// set_union
VS set_union(VS& v1, VS& v2);
RcppExport SEXP _jti_set_union(SEXP v1SEXP, SEXP v2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< VS& >::type v1(v1SEXP);
    Rcpp::traits::input_parameter< VS& >::type v2(v2SEXP);
    rcpp_result_gen = Rcpp::wrap(set_union(v1, v2));
    return rcpp_result_gen;
END_RCPP
}
// set_diff
VS set_diff(VS& v1, VS& v2);
RcppExport SEXP _jti_set_diff(SEXP v1SEXP, SEXP v2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< VS& >::type v1(v1SEXP);
    Rcpp::traits::input_parameter< VS& >::type v2(v2SEXP);
    rcpp_result_gen = Rcpp::wrap(set_diff(v1, v2));
    return rcpp_result_gen;
END_RCPP
}
// set_eq
bool set_eq(VS& v1, VS& v2);
RcppExport SEXP _jti_set_eq(SEXP v1SEXP, SEXP v2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< VS& >::type v1(v1SEXP);
    Rcpp::traits::input_parameter< VS& >::type v2(v2SEXP);
    rcpp_result_gen = Rcpp::wrap(set_eq(v1, v2));
    return rcpp_result_gen;
END_RCPP
}
// set_in
bool set_in(std::string& a, VS& b);
RcppExport SEXP _jti_set_in(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string& >::type a(aSEXP);
    Rcpp::traits::input_parameter< VS& >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(set_in(a, b));
    return rcpp_result_gen;
END_RCPP
}
// set_issubeq
bool set_issubeq(VS& a, VS& b);
RcppExport SEXP _jti_set_issubeq(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< VS& >::type a(aSEXP);
    Rcpp::traits::input_parameter< VS& >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(set_issubeq(a, b));
    return rcpp_result_gen;
END_RCPP
}
// set_any
bool set_any(std::vector<bool>& v);
RcppExport SEXP _jti_set_any(SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<bool>& >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(set_any(v));
    return rcpp_result_gen;
END_RCPP
}
// count_unique
RIV count_unique(VS x);
RcppExport SEXP _jti_count_unique(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< VS >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(count_unique(x));
    return rcpp_result_gen;
END_RCPP
}
// matpr
VS matpr(Rcpp::CharacterMatrix A);
RcppExport SEXP _jti_matpr(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterMatrix >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(matpr(A));
    return rcpp_result_gen;
END_RCPP
}
// sptab_
RIV sptab_(RCM& A);
RcppExport SEXP _jti_sptab_(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RCM& >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(sptab_(A));
    return rcpp_result_gen;
END_RCPP
}
// count_unique2
Rcpp::Environment count_unique2(VS x);
RcppExport SEXP _jti_count_unique2(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< VS >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(count_unique2(x));
    return rcpp_result_gen;
END_RCPP
}
// sptab_2
Rcpp::Environment sptab_2(RCM& A);
RcppExport SEXP _jti_sptab_2(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RCM& >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(sptab_2(A));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_jti_mcs", (DL_FUNC) &_jti_mcs, 2},
    {"_jti_perfect_cliques", (DL_FUNC) &_jti_perfect_cliques, 1},
    {"_jti_perfect_separators", (DL_FUNC) &_jti_perfect_separators, 1},
    {"_jti_parents", (DL_FUNC) &_jti_parents, 2},
    {"_jti_rip", (DL_FUNC) &_jti_rip, 2},
    {"_jti_set_intersect", (DL_FUNC) &_jti_set_intersect, 2},
    {"_jti_set_union", (DL_FUNC) &_jti_set_union, 2},
    {"_jti_set_diff", (DL_FUNC) &_jti_set_diff, 2},
    {"_jti_set_eq", (DL_FUNC) &_jti_set_eq, 2},
    {"_jti_set_in", (DL_FUNC) &_jti_set_in, 2},
    {"_jti_set_issubeq", (DL_FUNC) &_jti_set_issubeq, 2},
    {"_jti_set_any", (DL_FUNC) &_jti_set_any, 1},
    {"_jti_count_unique", (DL_FUNC) &_jti_count_unique, 1},
    {"_jti_matpr", (DL_FUNC) &_jti_matpr, 1},
    {"_jti_sptab_", (DL_FUNC) &_jti_sptab_, 1},
    {"_jti_count_unique2", (DL_FUNC) &_jti_count_unique2, 1},
    {"_jti_sptab_2", (DL_FUNC) &_jti_sptab_2, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_jti(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
