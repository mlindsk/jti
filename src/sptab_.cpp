#include "sptab_.h"

/*****************************************
 * In:
 * - x: Vector of strings
 * Out: A namedvector of counts of all unique
 *      elements in x
 ***************************************/
// [[Rcpp::export]]
// RIV count_unique(VS  x) { // std::unordered_map<std::string, int>
//   std::map<std::string, int> tab;
//   int n = x.size();
//   for (int i = 0; i < n; i++) {
//     auto s = x[i];
//     tab[s]++;
//   }
//   return Rcpp::wrap(tab);
// }

/*****************************************
 * In:
 * - x: Vector of strings
 * Out: An environment of counts of all unique
 *      elements in x
 ***************************************/
// [[Rcpp::export]]
RE count_unique_env(VS x) {
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
VS matpr(Rcpp::CharacterMatrix A) {
  // Paste rows in a character matrix
  int n = A.nrow();
  VS  x(n);
  for( int i = 0; i < n; i++ ) {
    auto row = A(i, Rcpp::_);
    std::string s;
    s = std::accumulate(row.begin(),row.end(), s);
    x[i] = s;
  }
  return x;
}

/*****************************************
 * In:
 * - A: A character matrix with all a-variables (and only them)
 * -    A needs to have dimnames = list(NULL, colnames)
 * Out: The a-marginal table with attribute = variable names
 ***************************************/
// [[Rcpp::export]]
// RIV sptab_(RCM & A) {
//   VS x = matpr(A);
//   auto na = count_unique(x);
//   // Rcpp::List Delta_A = A.attr("dimnames"); // Use colnames(A) ?
//   na.attr("vars") = Rcpp::colnames(A); //Delta_A[1];
//   return na;
// }

// [[Rcpp::export]]
RE sptab_env_(RCM & A) {
  VS x = matpr(A);
  auto na = count_unique_env(x);
  na.attr("vars") = Rcpp::colnames(A); //Delta_A[1];
  return na;
}
