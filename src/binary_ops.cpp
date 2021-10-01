#include "jti_types.h"
#include "set_ops.h"

// [[Rcpp::export]]
int nbinary_ops_int_(RL& cliques_int, arma::Mat<int>& collect_tree, VI sp, int root_idx) {

  int sum = 0;
  int nrows = collect_tree.n_rows;
  root_idx -= 1;

  // VI cumsum(nrows);

  for (int i = 0; i < nrows; i++) {

    if (i == root_idx) {
      VI C0 = cliques_int[i];
      int sp_C0 = C0.begin() == C0.end() ? 1 :std::accumulate(
      std::next(C0.begin()), C0.end(), sp[C0[0]-1],
      [&](int i, int j) -> int {return i * sp[j-1];});

      sum += 2*sp_C0 - 1;

      continue;
    }

    // leave
    VI C_leave = cliques_int[i];

    // parent
    auto row_i     = collect_tree.row(i);
    auto it        = std::find_if(row_i.begin(), row_i.end(), [](int x) {return x == 1;});
    int parent_idx = std::distance(row_i.begin(), it);
    VI C_parent    = cliques_int[parent_idx];

    // separator
    VI S = int_set_intersect(C_leave, C_parent);


    // ------------------
    // DEBUGGING - PRINT:
    // ------------------
    // std::cout << " ----------------- \n";
    // std::cout << " i: " << i << "\n";
    // std::cout << " C_leave: \n";
    // for (auto & e : C_leave) {
    //   std::cout << e << ":";
    // }

    // std::cout << " \n C_parent: \n";
    // for (auto & e : C_parent) {
    //   std::cout << e << ":";
    // }    
    
    // statespaces
    int sp_C_leave = C_leave.begin() == C_leave.end() ? 1 : std::accumulate(
       std::next(C_leave.begin()), C_leave.end(), sp[C_leave[0]-1],
       [&](int i, int j) -> int {return i * sp[j-1];});

    int sp_C_parent = C_parent.begin() == C_parent.end() ? 1 : std::accumulate(
       std::next(C_parent.begin()), C_parent.end(), sp[C_parent[0]-1],
       [&](int i, int j) -> int {return i * sp[j-1];});
    
    int sp_S = S.begin() == S.end() ? 1 : std::accumulate(
       std::next(S.begin()), S.end(), sp[S[0]-1],
       [&](int i, int j) -> int {return i * sp[j-1];});

    // std::cout << "statespace: \n";
    // std::cout << "leave: " << sp_C_leave << ",  parent: " << sp_C_parent << ",  S: " << sp_S;
    // std::cout << " \n----------------- \n";
    
    sum += 3*sp_C_leave + 2*sp_C_parent - 2*sp_S;
  }

  return sum;
}
