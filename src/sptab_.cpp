#include "sptab_.h"

// [[Rcpp::export]]
RE count_unique(VS x) {
  int n = x.size();
  RE env = new_env();
  for (int i = 0; i < n; i++) {
    auto s = x[i];
    if (env.exists(s)) {
      int curr = env[s];
      env[s] = curr + 1; 
    } else {
      env[s] = 1;
    }
  }
  return env;
}

// [[Rcpp::export]]
VS matpr(Rcpp::CharacterMatrix A, bool validate = true) {
  // Paste rows in a character matrix
  int n = A.nrow();
  int m = A.ncol();
  VS  x(n);
  for( int i = 0; i < n; i++ ) {
    auto row = A(i, Rcpp::_);
    std::string s;
    s = std::accumulate(row.begin(),row.end(), s);
    if (validate) {
      if (s.size() != m) {
	Rcpp::stop("Some values are longer than a single character. See '?to_chars'");
      }
    }    
    x[i] = s;
  }
  return x;
}

// [[Rcpp::export]]
RE sptab_(RCM & A, bool validate = true) {
  VS x = matpr(A, validate);
  auto na = count_unique(x);
  na.attr("vars") = Rcpp::colnames(A); //Delta_A[1];
  return na;
}
