#include "sptab_.h"

// [[Rcpp::export]]
Rcpp::Environment count_unique2(VS x) {
  int n = x.size();
  Environment env = new_env();
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
Rcpp::Environment sptab_2(RCM & A) {
  VS x = matpr(A);
  auto na = count_unique2(x);
  na.attr("vars") = Rcpp::colnames(A); //Delta_A[1];
  return na;
}


// struct sptab {
//   pair<unordered_map<string, int>, vector<string>> sptab;
// }

// // [[Rcpp::export]]
// unordered_map<string, int> ff(IntegerVector x, IntegerVector y) {

//   vector<string> x_names = x.names();
//   vector<string> y_names = y.names();

//   vector<string> x_vars = x.attr("vars");
//   vector<string> y_vars = y.attr("vars");

//   unordered_map<string, int> x_;
//   unordered_map<string, int> y_;
  
//   for (auto & e : x_names) {
//     x_.insert({e, x[e]});
//   }

//   for (auto & e : y_names) {
//     y_.insert({e, y[e]});
//   }
  
//   return x_;
// }


// std::vector<std::string> m2(Rcpp::Environment x, Rcpp::Environment y, char op = '*') {


//   CharacterVector vx = x.attr("vars");
//   CharacterVector vy = y.attr("vars");
//   CharacterVector sep = Rcpp::setdiff(vx, vy);

//   if (!sep.size()) {
//     std::cout << "size zero!";

//     Rcpp::Environment res;
//     // CharacterVector vres = vx;

//     std::vector<std::string> z{"1", "2", "3"};
//     std::vector<std::string> w{"1", "2", "3"};
    
//     z.insert(z.end(), w.begin(), w.end());
//     return z;
//     // res.attr("vars") = vres;
    
//   }

//   std::vector<std::string> z{"1", "2", "3"};
  
//   // USE SET OR MAP!!!!!
  
//   // CharacterVector names_x = x.ls(false);
//   // CharacterVector names_y = y.ls(false);
//   // std::cout << std::string(names_x[1]) + " " + names_y[1] + " " + vx[0] << "\n";
  
//   return z;
// }
