#include <Rcpp.h>
using namespace Rcpp;

// this function is a copy of R's function which.
// Rcpp::export]]
//IntegerVector which(const LogicalVector& x) {
//  IntegerVector out;
//  for (int i = 0; i < x.size(); i++) {
//    if (x[i]) out.push_back(i);
//  }
//  return out;
//}


// find which elements are NA in a vector - integer
IntegerVector which_na_i(const IntegerVector& vec) {
  int n = vec.size();
  IntegerVector out;
  for (int k = 0; k < n; k++) {
    if (IntegerVector::is_na(vec[k])) out.push_back(k);
  }
  return out;
}

// find which elements are NA in a vector - character
IntegerVector which_na_s(const CharacterVector& vec) {
  int n = vec.size();
  IntegerVector out;
  for (int k = 0; k < n; k++) {
    if (CharacterVector::is_na(vec[k])) out.push_back(k);
  }
  return out;
}

// find which elements are NA in a vector - numeric
IntegerVector which_na_n(const NumericVector& vec) {
  int n = vec.size();
  IntegerVector out;
  for (int k = 0; k < n; k++) {
    if (NumericVector::is_na(vec[k])) out.push_back(k);
  }
  return out;
}

// find which elements are NA in a vector - logical
IntegerVector which_na_l(const LogicalVector& vec) {
  int n = vec.size();
  IntegerVector out;
  for (int k = 0; k < n; k++) {
    if (LogicalVector::is_na(vec[k])) out.push_back(k);
  }
  return out;
}

// [[Rcpp::export]]
SEXP which_na(SEXP x) {
  switch(TYPEOF(x)) {
    case INTSXP: return which_na_i(x);
    case REALSXP: return which_na_n(x);
    case STRSXP: return which_na_s(x);
    case LGLSXP: return which_na_l(x);
  }
  return R_NilValue;
}
