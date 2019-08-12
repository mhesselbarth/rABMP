// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcpp_calculate_ci
NumericVector rcpp_calculate_ci(NumericMatrix matrix, double alpha, double beta, int max_dist);
RcppExport SEXP _rabmp_rcpp_calculate_ci(SEXP matrixSEXP, SEXP alphaSEXP, SEXP betaSEXP, SEXP max_distSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type matrix(matrixSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< int >::type max_dist(max_distSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_calculate_ci(matrix, alpha, beta, max_dist));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_calculate_distance_probability
NumericVector rcpp_calculate_distance_probability(double beta, double theta, int max_dist);
RcppExport SEXP _rabmp_rcpp_calculate_distance_probability(SEXP betaSEXP, SEXP thetaSEXP, SEXP max_distSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< double >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< int >::type max_dist(max_distSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_calculate_distance_probability(beta, theta, max_dist));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_calculate_mortality_probs
NumericVector rcpp_calculate_mortality_probs(StringVector species, NumericVector dbh, double int_beech_early, double dbh_beech_early, double int_beech_late, double dbh_beech_late, double dinc_beech, double int_ash, double dbh_ash, double int_others, double dbh_others);
RcppExport SEXP _rabmp_rcpp_calculate_mortality_probs(SEXP speciesSEXP, SEXP dbhSEXP, SEXP int_beech_earlySEXP, SEXP dbh_beech_earlySEXP, SEXP int_beech_lateSEXP, SEXP dbh_beech_lateSEXP, SEXP dinc_beechSEXP, SEXP int_ashSEXP, SEXP dbh_ashSEXP, SEXP int_othersSEXP, SEXP dbh_othersSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< StringVector >::type species(speciesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type dbh(dbhSEXP);
    Rcpp::traits::input_parameter< double >::type int_beech_early(int_beech_earlySEXP);
    Rcpp::traits::input_parameter< double >::type dbh_beech_early(dbh_beech_earlySEXP);
    Rcpp::traits::input_parameter< double >::type int_beech_late(int_beech_lateSEXP);
    Rcpp::traits::input_parameter< double >::type dbh_beech_late(dbh_beech_lateSEXP);
    Rcpp::traits::input_parameter< double >::type dinc_beech(dinc_beechSEXP);
    Rcpp::traits::input_parameter< double >::type int_ash(int_ashSEXP);
    Rcpp::traits::input_parameter< double >::type dbh_ash(dbh_ashSEXP);
    Rcpp::traits::input_parameter< double >::type int_others(int_othersSEXP);
    Rcpp::traits::input_parameter< double >::type dbh_others(dbh_othersSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_calculate_mortality_probs(species, dbh, int_beech_early, dbh_beech_early, int_beech_late, dbh_beech_late, dinc_beech, int_ash, dbh_ash, int_others, dbh_others));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_calculate_number_seeds
NumericVector rcpp_calculate_number_seeds(StringVector species, NumericVector dbh, double str_beech, double str_ash, double str_sycamore, double str_hornbeam, double str_others);
RcppExport SEXP _rabmp_rcpp_calculate_number_seeds(SEXP speciesSEXP, SEXP dbhSEXP, SEXP str_beechSEXP, SEXP str_ashSEXP, SEXP str_sycamoreSEXP, SEXP str_hornbeamSEXP, SEXP str_othersSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< StringVector >::type species(speciesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type dbh(dbhSEXP);
    Rcpp::traits::input_parameter< double >::type str_beech(str_beechSEXP);
    Rcpp::traits::input_parameter< double >::type str_ash(str_ashSEXP);
    Rcpp::traits::input_parameter< double >::type str_sycamore(str_sycamoreSEXP);
    Rcpp::traits::input_parameter< double >::type str_hornbeam(str_hornbeamSEXP);
    Rcpp::traits::input_parameter< double >::type str_others(str_othersSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_calculate_number_seeds(species, dbh, str_beech, str_ash, str_sycamore, str_hornbeam, str_others));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_create_seedlings
NumericMatrix rcpp_create_seedlings(NumericMatrix coords, NumericVector number, StringVector species, double beta_beech, double beta_ash, double beta_sycamore, double beta_hornbeam, double beta_others, int max_dist);
RcppExport SEXP _rabmp_rcpp_create_seedlings(SEXP coordsSEXP, SEXP numberSEXP, SEXP speciesSEXP, SEXP beta_beechSEXP, SEXP beta_ashSEXP, SEXP beta_sycamoreSEXP, SEXP beta_hornbeamSEXP, SEXP beta_othersSEXP, SEXP max_distSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type coords(coordsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type number(numberSEXP);
    Rcpp::traits::input_parameter< StringVector >::type species(speciesSEXP);
    Rcpp::traits::input_parameter< double >::type beta_beech(beta_beechSEXP);
    Rcpp::traits::input_parameter< double >::type beta_ash(beta_ashSEXP);
    Rcpp::traits::input_parameter< double >::type beta_sycamore(beta_sycamoreSEXP);
    Rcpp::traits::input_parameter< double >::type beta_hornbeam(beta_hornbeamSEXP);
    Rcpp::traits::input_parameter< double >::type beta_others(beta_othersSEXP);
    Rcpp::traits::input_parameter< int >::type max_dist(max_distSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_create_seedlings(coords, number, species, beta_beech, beta_ash, beta_sycamore, beta_hornbeam, beta_others, max_dist));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_random_distance
NumericVector rcpp_random_distance(int number_seeds, String species, double beta_beech, double beta_ash, double beta_sycamore, double beta_hornbeam, double beta_others, int max_dist);
RcppExport SEXP _rabmp_rcpp_random_distance(SEXP number_seedsSEXP, SEXP speciesSEXP, SEXP beta_beechSEXP, SEXP beta_ashSEXP, SEXP beta_sycamoreSEXP, SEXP beta_hornbeamSEXP, SEXP beta_othersSEXP, SEXP max_distSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type number_seeds(number_seedsSEXP);
    Rcpp::traits::input_parameter< String >::type species(speciesSEXP);
    Rcpp::traits::input_parameter< double >::type beta_beech(beta_beechSEXP);
    Rcpp::traits::input_parameter< double >::type beta_ash(beta_ashSEXP);
    Rcpp::traits::input_parameter< double >::type beta_sycamore(beta_sycamoreSEXP);
    Rcpp::traits::input_parameter< double >::type beta_hornbeam(beta_hornbeamSEXP);
    Rcpp::traits::input_parameter< double >::type beta_others(beta_othersSEXP);
    Rcpp::traits::input_parameter< int >::type max_dist(max_distSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_random_distance(number_seeds, species, beta_beech, beta_ash, beta_sycamore, beta_hornbeam, beta_others, max_dist));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rabmp_rcpp_calculate_ci", (DL_FUNC) &_rabmp_rcpp_calculate_ci, 4},
    {"_rabmp_rcpp_calculate_distance_probability", (DL_FUNC) &_rabmp_rcpp_calculate_distance_probability, 3},
    {"_rabmp_rcpp_calculate_mortality_probs", (DL_FUNC) &_rabmp_rcpp_calculate_mortality_probs, 11},
    {"_rabmp_rcpp_calculate_number_seeds", (DL_FUNC) &_rabmp_rcpp_calculate_number_seeds, 7},
    {"_rabmp_rcpp_create_seedlings", (DL_FUNC) &_rabmp_rcpp_create_seedlings, 9},
    {"_rabmp_rcpp_random_distance", (DL_FUNC) &_rabmp_rcpp_random_distance, 8},
    {NULL, NULL, 0}
};

RcppExport void R_init_rabmp(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
